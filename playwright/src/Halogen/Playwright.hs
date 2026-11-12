-- | Test a built program from outside the page: a native Haskell process
-- drives Chromium through Playwright, and waits on the program's own
-- signal rather than on the clock.
--
-- @
-- main :: IO ()
-- main = hspec $ playwright $ do
--   it "counts" $ inPage $ \\page -> do
--     visit page "http://127.0.0.1:8080/"
--     click (byRoleNamed page "button" "+")
--     locator page ".count" \`shouldHaveText\` "1"
-- @
--
-- = Waiting on the program
--
-- A page started here has @globalThis.__halogenProbe@ before any of its
-- scripts runs, so a Halogen program in it (built with a
-- haskell-halogen-core that has "Halogen.Activity") answers whether it is
-- busy. Every action ends with 'settle': a task of the page's at background
-- priority (a browser event reaches a WebAssembly program as a task of its
-- own, so its handler may not even have started when Playwright's click
-- returns), then the program's answer, asked until it is idle. So after
-- @click@ the handler has run and the page shows what it rendered: read it
-- at once, with no 'eventually'. What the program waits for in a @liftIO@
-- (a request) is not part of 'settle'; 'waitQuiet' waits for that too.
-- A timer or an emitter that has not fired is neither, and is what
-- 'eventually' and the retrying @should@ functions are for.
--
-- On a page with no Halogen program 'settle' waits for the one task and
-- returns, so the package drives any page.
--
-- = Locators
--
-- A 'Locator' is a value, not a handle in the browser: a page and the
-- steps to find an element, sent with each call, so it is as lazy as
-- Playwright's own. Like Playwright's, an action on a locator that matches
-- more than one element fails (use 'nth' or 'first').
--
-- Functions are in 'MonadIO', and every failure is a 'PlaywrightError'
-- (the driver's; a timeout's message is Playwright's, which names the
-- locator) or an @HUnitFailure@ (an expectation's, which hspec reports
-- with the test's line).
module Halogen.Playwright
  ( -- * Starting
    Playwright
  , Options (..)
  , defaultOptions
  , optionsFromEnvironment
  , withPlaywright
  , PlaywrightError (..)

    -- * Contexts and pages
  , Context
  , ContextOptions (..)
  , defaultContextOptions
  , ColorScheme (..)
  , withContext
  , newContext
  , closeContext
  , addInitScript
  , Page
  , newPage
  , closePage
  , withPage
  , withTimeout

    -- * Going places
  , goto
  , visit
  , reload
  , goBack
  , goForward
  , url
  , title
  , setViewport

    -- * The Halogen program
  , Activity (..)
  , waitStarted
  , settle
  , waitQuiet
  , activity

    -- * Locators
  , Locator
  , Scope
  , locator
  , nth
  , first
  , withText
  , byRole
  , byRoleNamed
  , byText
  , byLabel

    -- * Acting
  , click
  , doubleClick
  , hover
  , fill
  , typeText
  , clear
  , press
  , pressOn
  , check
  , uncheck
  , selectOption
  , focus
  , blur
  , dragTo

    -- * Reading
  , textContent
  , innerText
  , inputValue
  , getAttribute
  , isVisible
  , isChecked
  , isEnabled
  , count
  , outerHTML
  , content

    -- * Waiting and expecting
  , LocatorState (..)
  , waitFor
  , waitForFunction
  , eventually
  , eventuallyWithin
  , shouldHaveText
  , shouldContainText
  , shouldBeVisible
  , shouldBeHidden
  , shouldHaveCount
  , shouldHaveValue
  , shouldHaveAttribute
  , shouldHaveURL

    -- * JavaScript
  , evaluate
  , evaluateWith
  , evaluateAll

    -- * What the page said, and what it asked for
  , ConsoleMessage (..)
  , consoleMessages
  , pageErrors
  , Request (..)
  , requests
  , workers

    -- * Pictures
  , screenshot
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, fromException, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Clock (getMonotonicTime)
import GHC.Stack (CallStack, HasCallStack, SrcLoc, callStack, getCallStack, withFrozenCallStack)
import Halogen.Playwright.Internal
import System.Environment (lookupEnv)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Text.Read (readMaybe)

-- | 'defaultOptions', changed by the environment:
--
-- * @HALOGEN_PLAYWRIGHT_HEADED@ (set, not empty): show the browser;
-- * @HALOGEN_PLAYWRIGHT_SLOWMO@: milliseconds before every action;
-- * @HALOGEN_PLAYWRIGHT_TIMEOUT@: the timeout, in milliseconds;
-- * @HALOGEN_PLAYWRIGHT_NODE@: the @node@ to run;
-- * @HALOGEN_PLAYWRIGHT_ARTIFACTS@: where a failing test's screenshot goes.
optionsFromEnvironment :: IO Options
optionsFromEnvironment = do
  headed <- maybe False (not . null) <$> lookupEnv "HALOGEN_PLAYWRIGHT_HEADED"
  slow <- (>>= readMaybe) <$> lookupEnv "HALOGEN_PLAYWRIGHT_SLOWMO"
  time <- (>>= readMaybe) <$> lookupEnv "HALOGEN_PLAYWRIGHT_TIMEOUT"
  nodePath <- lookupEnv "HALOGEN_PLAYWRIGHT_NODE"
  artifactsDir <- lookupEnv "HALOGEN_PLAYWRIGHT_ARTIFACTS"
  pure
    defaultOptions
      { headless = not headed
      , slowMotion = fromMaybe defaultOptions.slowMotion slow
      , timeout = fromMaybe defaultOptions.timeout time
      , node = maybe defaultOptions.node id (nonEmpty nodePath)
      , artifacts = nonEmpty artifactsDir
      }
  where
    nonEmpty = \case
      Just s | not (null s) -> Just s
      _ -> Nothing

----------------------------------------------------------------------
-- Contexts and pages

data ColorScheme = Light | Dark
  deriving stock (Eq, Show)

data ContextOptions = ContextOptions
  { baseURL :: Maybe Text
  -- ^ What a relative address in 'goto' is resolved against.
  , viewport :: Maybe (Int, Int)
  -- ^ Width and height; Playwright's 1280 by 720 when not given.
  , colorScheme :: Maybe ColorScheme
  , javaScriptEnabled :: Bool
  , offline :: Bool
  , serviceWorkers :: Bool
  -- ^ Whether pages may register service workers.
  }
  deriving stock (Eq, Show)

-- | No base address, Playwright's viewport and colour scheme, JavaScript
-- on, online, service workers allowed.
defaultContextOptions :: ContextOptions
defaultContextOptions =
  ContextOptions
    { baseURL = Nothing
    , viewport = Nothing
    , colorScheme = Nothing
    , javaScriptEnabled = True
    , offline = False
    , serviceWorkers = True
    }

newContext :: (MonadIO m) => Playwright -> ContextOptions -> m Context
newContext pw o =
  liftIO $
    Context pw
      <$> call
        pw
        "newContext"
        [ "options"
            .= object
              [ "baseURL" .= o.baseURL
              , "viewport" .= fmap (\(w, h) -> object ["width" .= w, "height" .= h]) o.viewport
              , "colorScheme" .= fmap (\case Light -> "light" :: Text; Dark -> "dark") o.colorScheme
              , "javaScriptEnabled" .= o.javaScriptEnabled
              , "offline" .= o.offline
              , "serviceWorkers" .= (if o.serviceWorkers then "allow" else "block" :: Text)
              ]
        ]

-- | Close a context and its pages. On a driver that has gone there is
-- nothing left to close, and it does nothing.
closeContext :: (MonadIO m) => Context -> m ()
closeContext c = liftIO (ignoringGone (call c.playwright "closeContext" ["context" .= c.handle]))

-- | A call that has nothing to do once the driver is gone, so that the
-- cleanup of a 'withContext' does not hide why the body failed.
ignoringGone :: IO () -> IO ()
ignoringGone io =
  try io >>= \case
    Left (e :: PlaywrightError) | e.name == "DriverGone" -> pure ()
    Left e -> throwIO e
    Right () -> pure ()

-- | A context for the body, closed when it ends.
withContext :: Playwright -> ContextOptions -> (Context -> IO a) -> IO a
withContext pw o = bracket (newContext pw o) closeContext

-- | JavaScript every page of the context runs before its own scripts, in
-- every frame, after the driver's probe: to stand in for something the
-- page would otherwise find (a @Worker@ that refuses to start, say). It
-- applies to documents loaded after it is added.
addInitScript :: (MonadIO m) => Context -> Text -> m ()
addInitScript c script = liftIO (call c.playwright "addInitScript" ["context" .= c.handle, "script" .= script])

-- | What the context's pages, and their workers, have asked the network
-- for so far, oldest first: every request as it was made, whether it was
-- answered or not. A worker's requests are the context's, and not always
-- a page's, which is why this is asked of the context.
requests :: (MonadIO m) => Context -> m [Request]
requests c = liftIO (toList <$> contextRequests c.playwright c.handle)

newPage :: (MonadIO m) => Context -> m Page
newPage c = liftIO (Page c.playwright <$> call c.playwright "newPage" ["context" .= c.handle] <*> pure Nothing)

-- | The same page, whose calls, and the calls of locators made from it,
-- may take this many milliseconds in place of the options' 'timeout':
-- Playwright's action, the 'settle' after it, 'waitStarted', 'waitQuiet',
-- 'waitFor', 'eventually' and the retrying expectations. For the one call
-- that takes longer than the rest (a program loading, a job of seconds):
--
-- > visit (withTimeout 30000 page) address
withTimeout :: Int -> Page -> Page
withTimeout ms p = p {callTimeout = Just ms}

-- | The timeout a page's calls use.
timeoutOf :: Page -> Int
timeoutOf p = fromMaybe p.playwright.options.timeout p.callTimeout

-- | The call's own timeout, for the driver, when the page has one.
timeoutParam :: Page -> [Aeson.Types.Pair]
timeoutParam p = maybe [] (\ms -> ["timeout" .= ms]) p.callTimeout

-- | Close a page; like 'closeContext', nothing on a driver that has gone.
closePage :: (MonadIO m) => Page -> m ()
closePage p = liftIO (ignoringGone (call p.playwright "closePage" ["page" .= p.handle]))

-- | A page in a context of its own, with 'defaultContextOptions'.
withPage :: Playwright -> (Page -> IO a) -> IO a
withPage pw body = withContext pw defaultContextOptions (\c -> newPage c >>= body)

onPage :: (FromJSON a, MonadIO m) => Page -> Text -> [Aeson.Types.Pair] -> m a
onPage p method params = liftIO (call p.playwright method (("page" .= p.handle) : timeoutParam p <> params))

----------------------------------------------------------------------
-- Going places

-- | Go to an address and wait for the page's @load@ event, then 'settle'.
-- The program may not have mounted by then; 'visit' waits for that.
goto :: (MonadIO m) => Page -> Text -> m ()
goto p address = onPage p "goto" ["url" .= address]

-- | 'goto', then 'waitStarted': the page's program has mounted its tree
-- (adopted the prerendered page, if there was one) and is idle.
visit :: (MonadIO m) => Page -> Text -> m ()
visit p address = goto p address >> waitStarted p

reload :: (MonadIO m) => Page -> m ()
reload p = onPage p "reload" []

goBack :: (MonadIO m) => Page -> m ()
goBack p = onPage p "goBack" []

goForward :: (MonadIO m) => Page -> m ()
goForward p = onPage p "goForward" []

url :: (MonadIO m) => Page -> m Text
url p = onPage p "url" []

title :: (MonadIO m) => Page -> m Text
title p = onPage p "title" []

-- | The viewport's width and height.
setViewport :: (MonadIO m) => Page -> Int -> Int -> m ()
setViewport p w h = onPage p "setViewport" ["width" .= w, "height" .= h]

----------------------------------------------------------------------
-- The Halogen program

-- | What the page's program said ("Halogen.Activity"'s reading).
data Activity = Activity
  { trees :: Int
  , running :: Int
  , waiting :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \o -> Activity <$> o .: "trees" <*> o .: "running" <*> o .: "waiting"

-- | Wait until a program in the page has answered the probe with a tree
-- mounted and nothing running. On a page where none does, it fails after
-- the timeout, saying so.
waitStarted :: (MonadIO m) => Page -> m ()
waitStarted p = onPage p "waitStarted" []

-- | Wait for a background-priority task of the page's, then, if a program
-- in the page answers the probe, until it is idle (asking again after a
-- background task each time, never on a timer). Every action ends with it.
-- A navigation that takes the document away under it is not an error.
settle :: (MonadIO m) => Page -> m ()
settle p = onPage p "settle" []

-- | 'settle', and until no fiber of the program is waiting either (what a
-- @liftIO@ waits for, a request say, has come back). It asks a few
-- milliseconds apart. On timeout the error says how many still wait.
waitQuiet :: (MonadIO m) => Page -> m ()
waitQuiet p = onPage p "waitQuiet" []

-- | The program's reading, 'Nothing' on a page with no program that
-- answers.
activity :: (MonadIO m) => Page -> m (Maybe Activity)
activity p = onPage p "activity" []

----------------------------------------------------------------------
-- Locators

-- | Where to find an element: a page and the steps from it.
data Locator = Locator
  { page :: Page
  , steps :: [Value]
  }

instance Show Locator where
  show l = "Locator " <> Text.unpack (describe l)

-- | Where a locator can start: a page, or what another locator finds, as
-- Playwright's own @page.locator@ and @locator.locator@. So
-- @locator (locator page ".card") "button"@ is the buttons inside the
-- cards, and @byRoleNamed (locator page "form") "button" "Save"@ the form's
-- Save.
class Scope s where
  scope :: s -> Locator

instance Scope Page where
  scope p = Locator p []

instance Scope Locator where
  scope = id

stepFrom :: (Scope s) => s -> Value -> Locator
stepFrom s step = let l = scope s in l {steps = l.steps <> [step]}

-- | Elements matching a CSS selector (or any selector Playwright reads).
locator :: (Scope s) => s -> Text -> Locator
locator s css = stepFrom s (object ["css" .= css])

-- | The nth match, from 0.
nth :: Int -> Locator -> Locator
nth i l = l {steps = l.steps <> [object ["nth" .= i]]}

first :: Locator -> Locator
first = nth 0

-- | Those of the matches that contain this text, somewhere inside them.
withText :: Text -> Locator -> Locator
withText t l = l {steps = l.steps <> [object ["hasText" .= t]]}

-- | Elements by their ARIA role: @byRole page "listitem"@.
byRole :: (Scope s) => s -> Text -> Locator
byRole s role = stepFrom s (object ["role" .= role])

-- | Elements by their ARIA role and their accessible name, exactly:
-- @byRoleNamed page "button" "Save"@.
byRoleNamed :: (Scope s) => s -> Text -> Text -> Locator
byRoleNamed s role name = stepFrom s (object ["role" .= role, "name" .= name])

-- | Elements by the text they contain (Playwright's @getByText@).
byText :: (Scope s) => s -> Text -> Locator
byText s t = stepFrom s (object ["text" .= t])

-- | Form controls by their label.
byLabel :: (Scope s) => s -> Text -> Locator
byLabel s t = stepFrom s (object ["label" .= t])

locatorJSON :: Locator -> Value
locatorJSON l = object ["page" .= l.page.handle, "steps" .= l.steps]

-- | A locator as a reader of a failure would want it:
-- @css ".card" >> role "button" name "Save"@.
describe :: Locator -> Text
describe l = Text.intercalate " >> " (map step l.steps)
  where
    step = \case
      Aeson.Object m -> Text.unwords [Key.toText k <> " " <> render v | (k, v) <- KeyMap.toList m, v /= Aeson.Null]
      other -> render other
    render = \case
      Aeson.String t -> Text.pack (show t)
      other -> Text.decodeUtf8Lenient (LazyByteString.toStrict (Aeson.encode other))

on :: (FromJSON a, MonadIO m) => Locator -> Text -> [Aeson.Types.Pair] -> m a
on l method params = liftIO (call l.page.playwright method (("locator" .= locatorJSON l) : timeoutParam l.page <> params))

----------------------------------------------------------------------
-- Acting: each waits for the element as Playwright does, acts, and
-- settles.

click, doubleClick, hover, clear, check, uncheck, focus, blur :: (MonadIO m) => Locator -> m ()
click l = on l "click" []
doubleClick l = on l "doubleClick" []
hover l = on l "hover" []
clear l = on l "clear" []
check l = on l "check" []
uncheck l = on l "uncheck" []
focus l = on l "focus" []
blur l = on l "blur" []

-- | Set an input's value at once, as pasting would (one @input@ event).
fill :: (MonadIO m) => Locator -> Text -> m ()
fill l t = on l "fill" ["text" .= t]

-- | Type into an element a key at a time, as a user would.
typeText :: (MonadIO m) => Locator -> Text -> m ()
typeText l t = on l "typeText" ["text" .= t]

-- | Press a key (@"Enter"@, @"Control+A"@) on the page's keyboard, wherever
-- the focus is.
press :: (MonadIO m) => Page -> Text -> m ()
press p key = onPage p "press" ["key" .= key]

-- | Focus an element and press a key there.
pressOn :: (MonadIO m) => Locator -> Text -> m ()
pressOn l key = on l "pressOn" ["key" .= key]

-- | Choose the options of a @select@ with these values (or labels).
selectOption :: (MonadIO m) => Locator -> [Text] -> m ()
selectOption l values = on l "selectOption" ["values" .= values]

-- | Drag one element onto another, with the mouse.
dragTo :: (MonadIO m) => Locator -> Locator -> m ()
dragTo source target = on source "dragTo" ["target" .= locatorJSON target]

----------------------------------------------------------------------
-- Reading

textContent, innerText, inputValue, outerHTML :: (MonadIO m) => Locator -> m Text
textContent l = on l "textContent" []
innerText l = on l "innerText" []
inputValue l = on l "inputValue" []
outerHTML l = on l "outerHTML" []

getAttribute :: (MonadIO m) => Locator -> Text -> m (Maybe Text)
getAttribute l name = on l "getAttribute" ["name" .= name]

-- | These do not wait: they answer for the page as it is.
isVisible, isChecked, isEnabled :: (MonadIO m) => Locator -> m Bool
isVisible l = on l "isVisible" []
isChecked l = on l "isChecked" []
isEnabled l = on l "isEnabled" []

count :: (MonadIO m) => Locator -> m Int
count l = on l "count" []

-- | The page's whole HTML.
content :: (MonadIO m) => Page -> m Text
content p = onPage p "content" []

----------------------------------------------------------------------
-- Waiting and expecting

data LocatorState = Attached | Detached | Visible | Hidden
  deriving stock (Eq, Show)

-- | Wait until the locator's element is in this state, up to the timeout.
waitFor :: (MonadIO m) => Locator -> LocatorState -> m ()
waitFor l s = on l "waitFor" ["state" .= stateName]
  where
    stateName = case s of
      Attached -> "attached" :: Text
      Detached -> "detached"
      Visible -> "visible"
      Hidden -> "hidden"

-- | Poll a JavaScript expression in the page, at its animation frames,
-- until it is truthy, up to the timeout. For what the page does by itself
-- and not through its program, so that 'settle' does not wait for it: the
-- page's own frames, say.
--
-- > waitForFunction page "document.querySelectorAll('li').length > 3"
waitForFunction :: (MonadIO m) => Page -> Text -> m ()
waitForFunction p expression = onPage p "waitForFunction" ["expression" .= expression]

-- | Try an expectation until it holds or the page's timeout has passed;
-- then the last failure is the failure. A failed expectation or a failed
-- call is retried; any other exception is not.
eventually :: (MonadUnliftIO m) => Page -> m a -> m a
eventually p = eventuallyWithin (timeoutOf p)

-- | 'eventually' with a timeout in milliseconds. Between tries it pauses
-- briefly, a little longer each time (up to a quarter of a second): what
-- it waits for is not the program's, or 'settle' would have waited.
eventuallyWithin :: (MonadUnliftIO m) => Int -> m a -> m a
eventuallyWithin ms action = withRunInIO $ \run -> do
  deadline <- (+ fromIntegral ms / 1000) <$> getMonotonicTime
  let attempt pause =
        try (run action) >>= \case
          Right a -> pure a
          Left (e :: SomeException)
            | retryable e -> do
                now <- getMonotonicTime
                if now >= deadline
                  then throwIO e
                  else threadDelay pause >> attempt (min 250_000 (pause * 2))
            | otherwise -> throwIO e
  attempt 10_000
  where
    retryable e =
      isJust (fromException e :: Maybe HUnitFailure) || isJust (fromException e :: Maybe PlaywrightError)

-- | Retry a reading until it is what was expected, failing with what it
-- last was.
expecting :: (HasCallStack, Eq a, Show a) => Page -> Text -> IO a -> a -> IO ()
expecting p what reading expected = withFrozenCallStack $ eventually p $ do
  actual <- reading
  if actual == expected
    then pure ()
    else throwIO (HUnitFailure (location callStack) (ExpectedButGot (Just (Text.unpack what)) (show expected) (show actual)))

failWith :: (HasCallStack) => Text -> IO a
failWith message = throwIO (HUnitFailure (location callStack) (Reason (Text.unpack message)))

-- | Where the test called the expectation: the outermost frame of the
-- call stack, which the expectations freeze at their caller.
location :: CallStack -> Maybe SrcLoc
location stack = case reverse (getCallStack stack) of
  (_, loc) : _ -> Just loc
  [] -> Nothing

shouldHaveText :: (HasCallStack, MonadIO m) => Locator -> Text -> m ()
shouldHaveText l expected = liftIO $ withFrozenCallStack $ expecting l.page ("the text of " <> describe l) (textContent l) expected

-- | The element's text contains this.
shouldContainText :: (HasCallStack, MonadIO m) => Locator -> Text -> m ()
shouldContainText l part = liftIO $ withFrozenCallStack $ eventually l.page $ do
  actual <- textContent l
  if part `Text.isInfixOf` actual
    then pure ()
    else failWith ("the text of " <> describe l <> " does not contain " <> Text.pack (show part) <> ": it is " <> Text.pack (show actual))

shouldBeVisible :: (HasCallStack, MonadIO m) => Locator -> m ()
shouldBeVisible l = liftIO $ withFrozenCallStack $ expecting l.page (describe l <> " is visible") (isVisible l) True

shouldBeHidden :: (HasCallStack, MonadIO m) => Locator -> m ()
shouldBeHidden l = liftIO $ withFrozenCallStack $ expecting l.page (describe l <> " is visible") (isVisible l) False

shouldHaveCount :: (HasCallStack, MonadIO m) => Locator -> Int -> m ()
shouldHaveCount l n = liftIO $ withFrozenCallStack $ expecting l.page ("the elements matching " <> describe l) (count l) n

shouldHaveValue :: (HasCallStack, MonadIO m) => Locator -> Text -> m ()
shouldHaveValue l v = liftIO $ withFrozenCallStack $ expecting l.page ("the value of " <> describe l) (inputValue l) v

shouldHaveAttribute :: (HasCallStack, MonadIO m) => Locator -> Text -> Maybe Text -> m ()
shouldHaveAttribute l name v = liftIO $ withFrozenCallStack $ expecting l.page ("the attribute " <> name <> " of " <> describe l) (getAttribute l name) v

shouldHaveURL :: (HasCallStack, MonadIO m) => Page -> Text -> m ()
shouldHaveURL p expected = liftIO $ withFrozenCallStack $ expecting p "the page's address" (url p) expected

----------------------------------------------------------------------
-- JavaScript

-- | The value of a JavaScript expression in the page (awaited, if it is a
-- promise), as JSON.
evaluate :: (FromJSON a, MonadIO m) => Page -> Text -> m a
evaluate p expression = onPage p "evaluate" ["expression" .= expression]

-- | A JavaScript function's source, called in the page with this argument.
--
-- > evaluateWith page "(n) => n + 1" (41 :: Int) -- 42
evaluateWith :: (ToJSON arg, FromJSON a, MonadIO m) => Page -> Text -> arg -> m a
evaluateWith p function argument = onPage p "evaluateWith" ["function" .= function, "argument" .= toJSON argument]

-- | A JavaScript function's source, called in the page with every element
-- the locator matches (an array, empty when none does: this does not
-- wait) and this argument.
--
-- > evaluateAll (locator page "li") "(items) => items.map((i) => i.dataset.worker)" ()
evaluateAll :: (ToJSON arg, FromJSON a, MonadIO m) => Locator -> Text -> arg -> m a
evaluateAll l function argument = on l "evaluateAll" ["function" .= function, "argument" .= toJSON argument]

----------------------------------------------------------------------
-- What the page said

-- | What the page has written to its console so far, oldest first.
consoleMessages :: (MonadIO m) => Page -> m [ConsoleMessage]
consoleMessages p = liftIO (toList . (.console) <$> pageLog p.playwright p.handle)

-- | The page's uncaught exceptions and unhandled rejections so far.
pageErrors :: (MonadIO m) => Page -> m [Text]
pageErrors p = liftIO (toList . (.errors) <$> pageLog p.playwright p.handle)

-- | How many dedicated Web Workers the page has alive now: one is counted
-- from when it starts until it ends (it is terminated, or its page goes).
workers :: (MonadIO m) => Page -> m Int
workers p = onPage p "workers" []

-- | The whole page, as a PNG.
screenshot :: (MonadIO m) => Page -> FilePath -> m ()
screenshot p path = onPage p "screenshot" ["path" .= path]
