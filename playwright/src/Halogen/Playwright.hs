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
    -- | One node and its Chromium for a group of tests, configured by the
    -- options or the environment.
    Playwright
  , Options (..)
  , defaultOptions
  , optionsFromEnvironment
  , withPlaywright
  , PlaywrightError (..)

    -- * Contexts and pages
    -- | A context is a browser profile of its own (cookies, storage,
    -- permissions); a page is a tab of one.
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
  , pageContext
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
  , viewportSize

    -- * The Halogen program
    -- | Waits on the page's program itself, through the probe.
  , Activity (..)
  , waitStarted
  , settle
  , waitQuiet
  , activity

    -- * Locators
    -- | Values that say how to find an element, from a page, another
    -- locator, or a frame.
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
  , inFrame
  , frameAt
  , frames

    -- * Acting
    -- | Each waits for its element as Playwright does, acts, and settles.
  , click
  , doubleClick
  , hover
  , tap
  , fill
  , typeText
  , clear
  , press
  , pressOn
  , typeKeys
  , insertText
  , check
  , uncheck
  , selectOption
  , focus
  , blur
  , dragTo
  , mouseMove
  , mouseClick
  , Upload (..)
  , setInputFiles

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
  , Box (..)
  , boundingBox

    -- * Waiting and expecting
    -- | For what is not the program's to say: the page's own timers and
    -- frames, a message on its way, a navigation. Each fails after the
    -- page's timeout.
  , LocatorState (..)
  , waitFor
  , waitForFunction
  , waitForFunctionWith
  , waitForURL
  , waitForConsole
  , awaitText
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
  , evaluateOn

    -- * Requests answered by the test
    -- | A page's or a context's requests matching a glob, each handed to a
    -- handler of the test's, which says what becomes of it; and a gate
    -- built on them, which holds requests until the test lets them go.
  , Routed
  , Route
  , route
  , unroute
  , RoutedRequest (..)
  , Answer (..)
  , Response (..)
  , fulfill
  , Gate
  , hold
  , release
  , heldCount
  , waitHeld

    -- * Downloads
  , Download (..)
  , download
  , downloadBytes
  , saveDownload

    -- * Cookies, the network and the media
  , Cookie (..)
  , cookie
  , cookies
  , addCookies
  , clearCookies
  , setOffline
  , Media (..)
  , emulateMedia

    -- * What the page said, and what it asked for
  , ConsoleMessage (..)
  , consoleMessages
  , pageErrors
  , Dialog (..)
  , dialogs
  , Request (..)
  , requests
  , workers

    -- * The Chrome DevTools Protocol
  , cdp
  , throttleCPU

    -- * Pictures and print
  , screenshot
  , pdf
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay, writeTVar)
import Control.Exception (SomeException, bracket, bracket_, fromException, onException, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (find, toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Clock (getMonotonicTime)
import GHC.Stack (CallStack, HasCallStack, SrcLoc, callStack, getCallStack, withFrozenCallStack)
import Halogen.Playwright.Internal
import Halogen.Playwright.Serve (mediaType)
import System.Directory (copyFile)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Text.Read (readMaybe)

-- | 'defaultOptions', changed by the environment:
--
-- * @HALOGEN_PLAYWRIGHT_HEADED@ (set, not empty): show the browser;
-- * @HALOGEN_PLAYWRIGHT_SLOWMO@: milliseconds before every action;
-- * @HALOGEN_PLAYWRIGHT_TIMEOUT@: the timeout, in milliseconds;
-- * @HALOGEN_PLAYWRIGHT_NODE@: the @node@ to run;
-- * @HALOGEN_PLAYWRIGHT_ARTIFACTS@: where a failing test's screenshot goes;
-- * @HALOGEN_PLAYWRIGHT_CONSOLE@ (set, not empty): print what pages write to
--   their console as it arrives ('relayConsole');
-- * @HALOGEN_PLAYWRIGHT_THROTTLE@: slow every page's processor down by this
--   factor ('throttle'), to find on purpose the races a slow machine would.
optionsFromEnvironment :: IO Options
optionsFromEnvironment = do
  headed <- maybe False (not . null) <$> lookupEnv "HALOGEN_PLAYWRIGHT_HEADED"
  slow <- (>>= readMaybe) <$> lookupEnv "HALOGEN_PLAYWRIGHT_SLOWMO"
  time <- (>>= readMaybe) <$> lookupEnv "HALOGEN_PLAYWRIGHT_TIMEOUT"
  nodePath <- lookupEnv "HALOGEN_PLAYWRIGHT_NODE"
  artifactsDir <- lookupEnv "HALOGEN_PLAYWRIGHT_ARTIFACTS"
  relay <- maybe False (not . null) <$> lookupEnv "HALOGEN_PLAYWRIGHT_CONSOLE"
  rate <- (>>= readMaybe) <$> lookupEnv "HALOGEN_PLAYWRIGHT_THROTTLE"
  pure
    defaultOptions
      { headless = not headed
      , slowMotion = fromMaybe defaultOptions.slowMotion slow
      , timeout = fromMaybe defaultOptions.timeout time
      , node = maybe defaultOptions.node id (nonEmpty nodePath)
      , artifacts = nonEmpty artifactsDir
      , relayConsole = relay
      , throttle = rate
      }
  where
    nonEmpty = \case
      Just s | not (null s) -> Just s
      _ -> Nothing

----------------------------------------------------------------------
-- Contexts and pages

data ColorScheme = Light | Dark
  deriving stock (Eq, Show)

schemeName :: ColorScheme -> Text
schemeName = \case
  Light -> "light"
  Dark -> "dark"

data ContextOptions = ContextOptions
  { baseURL :: Maybe Text
  -- ^ What a relative address in 'goto' is resolved against.
  , viewport :: Maybe (Int, Int)
  -- ^ Width and height; Playwright's 1280 by 720 when not given.
  , colorScheme :: Maybe ColorScheme
  , javaScriptEnabled :: Bool
  , offline :: Bool
  , serviceWorkers :: Bool
  -- ^ Whether pages may register service workers. A request a service
  -- worker makes, or answers from its cache, does not pass a page's
  -- 'route's: a test that routes a page's requests usually wants this off.
  , permissions :: [Text]
  -- ^ What the pages are granted without asking, such as
  -- @"clipboard-read"@ and @"clipboard-write"@.
  , isMobile :: Bool
  -- ^ A phone's browser: the viewport's @meta@ tag is read, and the pages
  -- are laid out as on a phone. With 'hasTouch', 'tap' works.
  , hasTouch :: Bool
  , acceptDialogs :: Bool
  -- ^ Whether a dialog a page opens (@alert@, @confirm@, @prompt@, a
  -- @beforeunload@ asking whether to leave) is accepted rather than
  -- dismissed. Either way it is answered at once, so a page never waits on
  -- one, and kept ('dialogs').
  }
  deriving stock (Eq, Show)

-- | No base address, Playwright's viewport and colour scheme, JavaScript
-- on, online, service workers allowed, no permissions, a desktop's
-- browser, dialogs dismissed (as Playwright dismisses them).
defaultContextOptions :: ContextOptions
defaultContextOptions =
  ContextOptions
    { baseURL = Nothing
    , viewport = Nothing
    , colorScheme = Nothing
    , javaScriptEnabled = True
    , offline = False
    , serviceWorkers = True
    , permissions = []
    , isMobile = False
    , hasTouch = False
    , acceptDialogs = False
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
              , "colorScheme" .= fmap schemeName o.colorScheme
              , "javaScriptEnabled" .= o.javaScriptEnabled
              , "offline" .= o.offline
              , "serviceWorkers" .= (if o.serviceWorkers then "allow" else "block" :: Text)
              , "permissions" .= o.permissions
              , "isMobile" .= o.isMobile
              , "hasTouch" .= o.hasTouch
              , "acceptDialogs" .= o.acceptDialogs
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
newPage c = liftIO $ do
  handle <- call c.playwright "newPage" ["context" .= c.handle]
  pure Page {playwright = c.playwright, handle, context = c.handle, callTimeout = Nothing}

-- | The context a page is in: for what is the context's, which a test
-- holding only the page (as 'Halogen.Playwright.Hspec.inPage' gives it)
-- still wants: 'requests', 'addInitScript', 'cookies', a 'route' of the
-- context's.
pageContext :: Page -> Context
pageContext p = Context {playwright = p.playwright, handle = p.context}

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

-- | The viewport's width and height now (what the context's options or
-- 'setViewport' made it), to check that something fits a phone's screen.
viewportSize :: (MonadIO m) => Page -> m (Int, Int)
viewportSize p = do
  size <- onPage p "viewportSize" []
  case Aeson.Types.parseMaybe (withObject "size" (\o -> (,) <$> o .: "width" <*> o .: "height")) size of
    Just wh -> pure wh
    Nothing -> liftIO (throwIO (PlaywrightError "viewportSize" "NoViewport" "the page has no fixed viewport"))

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

-- | Inside the frame an @iframe@ element shows, as Playwright's
-- @frameLocator@: the steps after this one are taken in the frame's
-- document. So @locator (inFrame page "iframe.wiki-svg") "#status"@ is the
-- element @#status@ of the document in the frame. Every reading and
-- action works there, and an action settles the page.
inFrame :: (Scope s) => s -> Text -> Locator
inFrame s css = stepFrom s (object ["frameLocator" .= css])

-- | Inside the page's frame whose address matches a glob (Playwright's, as
-- in 'route'), however its @iframe@ is found or wherever it has navigated
-- since: @innerText (locator (frameAt page "**/api/v1/session") "body")@.
-- No such frame is a 'PlaywrightError' named @NoFrame@, which 'eventually'
-- retries, as a frame that has not loaded yet is the usual reason.
frameAt :: Page -> Text -> Locator
frameAt p glob = Locator p [object ["frame" .= glob]]

-- | The addresses of the page's frames now, the page's own first, then
-- those of its @iframe@s (a frame the browser refused to show has its
-- error page's, @chrome-error://@...).
frames :: (MonadIO m) => Page -> m [Text]
frames p = onPage p "frames" []

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

-- | Tap an element, as a finger would: the context must have 'hasTouch'.
tap :: (MonadIO m) => Locator -> m ()
tap l = on l "tap" []

-- | Type text on the page's keyboard, a key at a time, wherever the focus
-- is (Playwright's @keyboard.type@): after a key that opened a search box,
-- say. 'typeText' types into an element.
typeKeys :: (MonadIO m) => Page -> Text -> m ()
typeKeys p t = onPage p "typeKeys" ["text" .= t]

-- | Put text where the focus is at once, as pasting it would (one @input@
-- event, no keys): into an editor that would act on each key typed.
insertText :: (MonadIO m) => Page -> Text -> m ()
insertText p t = onPage p "insertText" ["text" .= t]

-- | Move the mouse to a point of the viewport, in CSS pixels: off an
-- element, so that what it showed on hover goes.
mouseMove :: (MonadIO m) => Page -> Double -> Double -> m ()
mouseMove p x y = onPage p "mouseMove" ["x" .= x, "y" .= y]

-- | Click at a point of the viewport: in a canvas, whose elements no
-- locator finds (a point from its 'boundingBox').
mouseClick :: (MonadIO m) => Page -> Double -> Double -> m ()
mouseClick p x y = onPage p "mouseClick" ["x" .= x, "y" .= y]

-- | A file for 'setInputFiles': one on disk, or bytes the test made, with
-- the name and the media type the page will see.
data Upload
  = UploadFile FilePath
  | UploadBytes {name :: Text, mimeType :: Text, bytes :: ByteString}
  deriving stock (Eq, Show)

-- | Choose the files of an @input type=file@, as a user picking them would
-- (an empty list clears it). A file on disk goes with its name and the
-- media type its extension says.
setInputFiles :: (MonadIO m) => Locator -> [Upload] -> m ()
setInputFiles l uploads = do
  files <- liftIO (mapM payload uploads)
  on l "setInputFiles" ["files" .= files]
  where
    payload = \case
      UploadBytes {name, mimeType, bytes} -> pure (file name mimeType bytes)
      UploadFile path -> do
        bytes <- ByteString.readFile path
        let kind = Text.strip (Text.takeWhile (/= ';') (Text.decodeUtf8 (mediaType path)))
        pure (file (Text.pack (takeFileName path)) kind bytes)
    file name kind bytes = object ["name" .= name, "mimeType" .= kind, "bytes" .= base64 bytes]

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

-- | Where an element is drawn, in CSS pixels from the viewport's top left.
data Box = Box {x :: Double, y :: Double, width :: Double, height :: Double}
  deriving stock (Eq, Show)

instance FromJSON Box where
  parseJSON = withObject "Box" $ \o -> Box <$> o .: "x" <*> o .: "y" <*> o .: "width" <*> o .: "height"

-- | The element's box, once it is attached; 'Nothing' if it is not drawn
-- (hidden, say).
boundingBox :: (MonadIO m) => Locator -> m (Maybe Box)
boundingBox l = on l "boundingBox" []

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

-- | 'waitForFunction' with a function's source and an argument for it, so
-- that a value of the test's is passed rather than spliced into the
-- JavaScript:
--
-- > waitForFunctionWith page "(n) => document.querySelectorAll('li').length >= n" (3 :: Int)
waitForFunctionWith :: (ToJSON arg, MonadIO m) => Page -> Text -> arg -> m ()
waitForFunctionWith p function argument = onPage p "waitForFunctionWith" ["function" .= function, "argument" .= toJSON argument]

-- | Wait until the page's address is this one, or matches it as a glob
-- (Playwright's: @**@ any characters, @*@ any but @/@), then 'settle': after
-- a click on a link that the program follows without loading a page, or one
-- that loads one (then this waits for its @load@).
--
-- > waitForURL page (base <> "06-router/")
waitForURL :: (MonadIO m) => Page -> Text -> m ()
waitForURL p address = onPage p "waitForURL" ["url" .= address]

-- | The first message in the page's console, from its start, that satisfies
-- the test, waiting for one if there is none yet, up to the page's timeout.
-- A message written before the call counts, so one an action wrote cannot
-- be missed; to wait for a later one, make the test tell it apart.
waitForConsole :: (MonadIO m) => Page -> (ConsoleMessage -> Bool) -> m ConsoleMessage
waitForConsole p test = liftIO $ do
  found <- waitForLog p.playwright p.handle (timeoutOf p) (find test . (.console))
  case found of
    Just m -> pure m
    Nothing ->
      throwIO (PlaywrightError "waitForConsole" "TimeoutError" ("no message in the page's console satisfied the test within " <> Text.pack (show (timeoutOf p)) <> "ms"))

-- | The element's text, once it satisfies the test (retried as the @should@
-- functions are, up to the page's timeout); the failure says what it last
-- was.
--
-- > status <- awaitText (locator page "#status") (`elem` ["done", "failed"])
awaitText :: (HasCallStack, MonadIO m) => Locator -> (Text -> Bool) -> m Text
awaitText l test = liftIO $ withFrozenCallStack $ eventually l.page $ do
  actual <- textContent l
  if test actual
    then pure actual
    else failWith ("the text of " <> describe l <> " is " <> Text.pack (show actual) <> ", which does not satisfy the test")

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

-- | A JavaScript function's source, called in the page with the locator's
-- one element (waited for, as an action waits) and this argument; in a
-- frame, when the locator is in one.
--
-- > evaluateOn (locator page "canvas") "(c, _) => c.width" () :: IO Int
evaluateOn :: (ToJSON arg, FromJSON a, MonadIO m) => Locator -> Text -> arg -> m a
evaluateOn l function argument = on l "evaluateOn" ["function" .= function, "argument" .= toJSON argument]

----------------------------------------------------------------------
-- What the page said

-- | What the page has written to its console so far, oldest first.
consoleMessages :: (MonadIO m) => Page -> m [ConsoleMessage]
consoleMessages p = liftIO (toList . (.console) <$> pageLog p.playwright p.handle)

-- | The page's uncaught exceptions and unhandled rejections so far.
pageErrors :: (MonadIO m) => Page -> m [Text]
pageErrors p = liftIO (toList . (.errors) <$> pageLog p.playwright p.handle)

-- | The dialogs the page has opened so far, oldest first, each answered as
-- the context's 'acceptDialogs' says.
dialogs :: (MonadIO m) => Page -> m [Dialog]
dialogs p = liftIO (toList . (.dialogs) <$> pageLog p.playwright p.handle)

-- | How many dedicated Web Workers the page has alive now: one is counted
-- from when it starts until it ends (it is terminated, or its page goes).
workers :: (MonadIO m) => Page -> m Int
workers p = onPage p "workers" []

-- | The whole page, as a PNG.
screenshot :: (MonadIO m) => Page -> FilePath -> m ()
screenshot p path = onPage p "screenshot" ["path" .= path]

-- | The page printed to PDF, its pages the size its CSS asks for (an
-- @\@page@ rule's), as for a deck of slides printed one to a page. Use
-- 'emulateMedia' first to see what printing shows.
pdf :: (MonadIO m) => Page -> m ByteString
pdf p = onPage p "pdf" [] >>= liftIO . fromBase64 "pdf"

----------------------------------------------------------------------
-- Requests answered by the test

-- | What a 'route' can be given: a 'Page', whose requests it sees, or a
-- 'Context', whose pages' and workers' requests it sees. A request a
-- service worker makes, or answers from its cache, is seen by neither:
-- open the context with 'serviceWorkers' off when that matters.
class Routed s where
  routedOn :: s -> (Playwright, Int, Aeson.Types.Pair, Int)
  -- ^ The driver, the context, the target as node is told it, and the
  -- timeout of waits on it.

instance Routed Page where
  routedOn p = (p.playwright, p.context, "page" .= p.handle, timeoutOf p)

instance Routed Context where
  routedOn c = (c.playwright, c.handle, "context" .= c.handle, c.playwright.options.timeout)

-- | A route, to take away with 'unroute'.
data Route = Route
  { playwright :: Playwright
  , handle :: Int
  , pattern :: Text
  }

-- | A request on its way, as a route's handler is given it.
data RoutedRequest = RoutedRequest
  { url :: Text
  , method :: Text
  , resourceType :: Text
  -- ^ Playwright's name for what it is for: @document@, @script@,
  -- @fetch@, @image@, ...
  , headers :: [(Text, Text)]
  , postData :: Maybe ByteString
  }
  deriving stock (Eq, Show)

-- | What becomes of a routed request.
data Answer
  = -- | It goes on as if this route were not there: to the next route that
    -- matches it, or to the network.
    Continue
  | -- | It is answered with this, and nothing is asked of the network.
    Fulfill Response
  | -- | It fails with this error, Playwright's name for it:
    -- @"connectionrefused"@ (a server out of reach), @"failed"@,
    -- @"internetdisconnected"@, @"timedout"@, ...
    Abort Text
  deriving stock (Eq, Show)

-- | An answer the test gives in the server's place.
data Response = Response
  { status :: Int
  , headers :: [(Text, Text)]
  , contentType :: Maybe Text
  , body :: ByteString
  }
  deriving stock (Eq, Show)

-- | A 'Fulfill' with a status, a media type (if any) and a body, and no
-- other headers:
--
-- > fulfill 204 Nothing ""
-- > fulfill 200 (Just "image/png") standInImage
fulfill :: Int -> Maybe Text -> ByteString -> Answer
fulfill status contentType body = Fulfill Response {status, headers = [], contentType, body}

-- | Hand a page's or a context's requests whose address matches a glob
-- (Playwright's: @**@ any characters, @*@ any but @/@, @{a,b}@ either; a
-- whole address, so @"**/app.wasm"@ or @"https://upload.wikimedia.org/**"@)
-- to a handler, which says what becomes of each.
--
-- Each request's handler runs on a thread of its own, so a handler may
-- block: wait for the test to let the request go ('hold' is such a
-- handler), or for a moment. A handler that throws aborts its request;
-- its failure is written to stderr and kept with the context, and
-- "Halogen.Playwright.Hspec" adds it to a test's failure. When the route
-- is taken away ('unroute'), or its page or context closes, or the driver
-- ends, the handlers still under way are killed, and their requests go on
-- as if the route had not been there.
--
-- When several routes match a request, the one added last is asked first,
-- and its 'Continue' passes the request to the one before.
route :: (Routed s, MonadIO m) => s -> Text -> (RoutedRequest -> IO Answer) -> m Route
route s pattern handler = liftIO $ do
  let (pw, context, target, _) = routedOn s
  handle <- addRoute pw context target pattern (\request -> answerFields <$> (parseRequest request >>= handler))
  pure Route {playwright = pw, handle, pattern}
  where
    parseRequest o = case Aeson.Types.parseEither parser o of
      Right r -> pure r
      Left e -> throwIO (PlaywrightError "route" "DecodeError" (Text.pack e))
    parser o = do
      headerMap <- o .: "headers"
      encoded <- o .:? "postData"
      postData <- traverse (either (fail . ("postData: " <>)) pure . Base64.decode . Text.encodeUtf8) encoded
      RoutedRequest
        <$> o .: "url"
        <*> o .: "method"
        <*> o .: "resourceType"
        <*> pure [(Key.toText k, v) | (k, v) <- KeyMap.toList headerMap]
        <*> pure postData
    answerFields = \case
      Continue -> ["action" .= ("continue" :: Text)]
      Abort code -> ["action" .= ("abort" :: Text), "error" .= code]
      Fulfill r ->
        [ "action" .= ("fulfill" :: Text)
        , "status" .= r.status
        , "headers" .= object [Key.fromText k .= v | (k, v) <- r.headers]
        , "contentType" .= r.contentType
        , "body" .= base64 r.body
        ]

-- | Take a route away. Its handlers under way are killed and their
-- requests go on, as if the route had not been there, and later requests
-- do not see it: the requests in flight are let go before Playwright's
-- route is removed, so nothing is left waiting on a handler that will not
-- answer. Nothing on a route already gone with its page, or a driver that
-- has ended.
unroute :: (MonadIO m) => Route -> m ()
unroute r = liftIO (removeRoute r.playwright r.handle)

-- | Requests held until the test lets them go.
data Gate = Gate
  { route :: Route
  , open :: TVar Bool
  , waiting :: TVar Int
  , timeoutMs :: Int
  }

-- | Hold a page's or a context's requests matching a glob (as 'route's)
-- until 'release': to look at a prerendered page before its program
-- (@app.wasm@) arrives, or to keep a request under way while the test
-- checks what the page shows meanwhile.
--
-- > gate <- hold page "**/app.wasm"
-- > goto page address
-- > ... the prerendered page, as the server sent it ...
-- > release gate
-- > waitStarted page
hold :: (Routed s, MonadIO m) => s -> Text -> m Gate
hold s pattern = liftIO $ do
  open <- newTVarIO False
  waiting <- newTVarIO 0
  let (_, _, _, ms) = routedOn s
      held _ = do
        bracket_
          (atomically (modifyTVar' waiting (+ 1)))
          (atomically (modifyTVar' waiting (subtract 1)))
          (atomically (readTVar open >>= STM.check))
        pure Continue
  r <- route s pattern held
  pure Gate {route = r, open, waiting, timeoutMs = ms}

-- | Let the held requests go on, and every later one pass straight
-- through.
release :: (MonadIO m) => Gate -> m ()
release g = liftIO (atomically (writeTVar g.open True))

-- | How many requests the gate holds now.
heldCount :: (MonadIO m) => Gate -> m Int
heldCount g = liftIO (readTVarIO g.waiting)

-- | Wait until the gate holds at least one request, up to the timeout of
-- the page or context it was made on: the page has asked for what is
-- held, and is waiting for it.
waitHeld :: (MonadIO m) => Gate -> m ()
waitHeld g = liftIO $ do
  late <- registerDelay (g.timeoutMs * 1000)
  held <- atomically $ ((readTVar g.waiting >>= STM.check . (> 0)) >> pure True) `orElse` (readTVar late >>= STM.check >> pure False)
  if held
    then pure ()
    else throwIO (PlaywrightError "waitHeld" "TimeoutError" ("no request matching " <> g.route.pattern <> " was held within " <> Text.pack (show g.timeoutMs) <> "ms"))

----------------------------------------------------------------------
-- Downloads

-- | A file the page downloaded, saved whole: its name as the page or the
-- server suggested it, the address it came from, and where it was saved
-- (in the driver's temporary directory, which goes when 'withPlaywright'
-- ends: 'saveDownload' keeps it).
data Download = Download
  { suggestedFilename :: Text
  , url :: Text
  , path :: FilePath
  }
  deriving stock (Eq, Show)

instance FromJSON Download where
  parseJSON = withObject "Download" $ \o -> Download <$> o .: "suggestedFilename" <*> o .: "url" <*> o .: "path"

-- | Run an action that makes the page download a file (a click on an
-- export button, a 'goto' to an address that is a download), and the
-- download, once it has been saved whole. The wait for it is armed before
-- the action, so that it cannot be missed. A 'goto' that becomes a download
-- returns without an error. No download within the page's timeout, or one
-- that does not end within it, is a @TimeoutError@.
--
-- > exported <- download page (click (locator page ".demo-export"))
-- > exported.suggestedFilename `shouldSatisfy` Text.isSuffixOf ".zip"
-- > zip <- downloadBytes exported
download :: (MonadIO m) => Page -> IO () -> m Download
download p action = liftIO $ do
  armed :: Int <- onPage p "expectDownload" []
  action `onException` ignoringFailure (onPage p "forgetDownload" ["download" .= armed] :: IO Value)
  onPage p "awaitDownload" ["download" .= armed]
  where
    ignoringFailure io = () <$ (try io :: IO (Either PlaywrightError Value))

-- | The downloaded file's bytes.
downloadBytes :: (MonadIO m) => Download -> m ByteString
downloadBytes d = liftIO (ByteString.readFile d.path)

-- | Copy the downloaded file to a path of the test's.
saveDownload :: (MonadIO m) => Download -> FilePath -> m ()
saveDownload d to = liftIO (copyFile d.path to)

----------------------------------------------------------------------
-- Cookies, the network and the media

-- | A cookie of a context's. One to add needs either 'url' (it is then
-- that site's, its domain and path taken from the address) or 'domain' and
-- 'path'; 'cookies' gives them with a domain and a path.
data Cookie = Cookie
  { name :: Text
  , value :: Text
  , url :: Maybe Text
  , domain :: Maybe Text
  , path :: Maybe Text
  , expires :: Maybe Double
  -- ^ Seconds since 1970; 'Nothing' for a cookie of the session.
  , httpOnly :: Bool
  , secure :: Bool
  , sameSite :: Maybe Text
  -- ^ @Strict@, @Lax@ or @None@.
  }
  deriving stock (Eq, Show)

instance FromJSON Cookie where
  parseJSON = withObject "Cookie" $ \o -> do
    name <- o .: "name"
    value <- o .: "value"
    domain <- o .:? "domain"
    path <- o .:? "path"
    expiry <- o .:? "expires"
    httpOnly <- fromMaybe False <$> o .:? "httpOnly"
    secure <- fromMaybe False <$> o .:? "secure"
    sameSite <- o .:? "sameSite"
    -- Playwright says -1 for a cookie of the session.
    let expires = case expiry of
          Just e | e >= 0 -> Just e
          _ -> Nothing
    pure Cookie {name, value, url = Nothing, domain, path, expires, httpOnly, secure, sameSite}

instance ToJSON Cookie where
  toJSON c =
    object
      [ "name" .= c.name
      , "value" .= c.value
      , "url" .= c.url
      , "domain" .= c.domain
      , "path" .= c.path
      , "expires" .= c.expires
      , "httpOnly" .= c.httpOnly
      , "secure" .= c.secure
      , "sameSite" .= c.sameSite
      ]

-- | A cookie with a name and a value for a site's address, as a test gives
-- a fresh context the session another signed in with:
--
-- > addCookies ctx [cookie "wiki_session" token origin]
cookie :: Text -> Text -> Text -> Cookie
cookie name value address =
  Cookie {name, value, url = Just address, domain = Nothing, path = Nothing, expires = Nothing, httpOnly = False, secure = False, sameSite = Nothing}

-- | The context's cookies, for every site.
cookies :: (MonadIO m) => Context -> m [Cookie]
cookies c = liftIO (call c.playwright "cookies" ["context" .= c.handle])

addCookies :: (MonadIO m) => Context -> [Cookie] -> m ()
addCookies c cs = liftIO (call c.playwright "addCookies" ["context" .= c.handle, "cookies" .= cs])

-- | Take every cookie of the context away: signed out, everywhere.
clearCookies :: (MonadIO m) => Context -> m ()
clearCookies c = liftIO (call c.playwright "clearCookies" ["context" .= c.handle])

-- | Take the context's pages off the network, or put them back, then
-- 'settle' each: they hear @offline@ or @online@ as a browser that lost its
-- network would, and a service worker answers from what it kept. A request
-- in a route's hands is not the network's, and goes on.
setOffline :: (MonadIO m) => Context -> Bool -> m ()
setOffline c offline = liftIO (call c.playwright "setOffline" ["context" .= c.handle, "offline" .= offline])

-- | What CSS media queries see.
data Media
  = -- | As printed: @\@media print@ applies.
    Print
  | -- | As on a screen again.
    Screen
  | -- | @prefers-color-scheme@.
    Scheme ColorScheme
  deriving stock (Eq, Show)

-- | Make the page's media queries see this from now on, then 'settle' (a
-- program listening to a @matchMedia@ hears the change).
emulateMedia :: (MonadIO m) => Page -> Media -> m ()
emulateMedia p = \case
  Print -> onPage p "emulateMedia" ["media" .= ("print" :: Text)]
  Screen -> onPage p "emulateMedia" ["media" .= ("screen" :: Text)]
  Scheme scheme -> onPage p "emulateMedia" ["colorScheme" .= schemeName scheme]

----------------------------------------------------------------------
-- The Chrome DevTools Protocol

-- | A method of the Chrome DevTools Protocol, on the page's session (made
-- on the first call, and kept), with its parameters; its result. For what
-- Playwright has no call for: a virtual authenticator for passkeys
-- (@WebAuthn.addVirtualAuthenticator@), Chromium's own installability
-- check (@Page.getInstallabilityErrors@), an element's event listeners.
-- Like 'evaluate', it does not settle.
--
-- > _ :: Value <- cdp page "WebAuthn.enable" ()
-- > Authenticator {authenticatorId} <- cdp page "WebAuthn.addVirtualAuthenticator" (object ["options" .= options])
cdp :: (ToJSON params, FromJSON a, MonadIO m) => Page -> Text -> params -> m a
cdp p method params = onPage p "cdp" ["cdpMethod" .= method, "params" .= paramsOf (toJSON params)]
  where
    -- () and nothing at all are no parameters.
    paramsOf = \case
      Aeson.Array a | null a -> object []
      Aeson.Null -> object []
      other -> other

-- | Slow the page's processor down by a factor (1 is full speed again), as
-- Chromium's DevTools do. 'throttle' in the options does this to every
-- page as it is made.
throttleCPU :: (MonadIO m) => Page -> Double -> m ()
throttleCPU p rate = do
  _ :: Value <- cdp p "Emulation.setCPUThrottlingRate" (object ["rate" .= rate])
  pure ()

----------------------------------------------------------------------
-- Bytes across the protocol

base64 :: ByteString -> Text
base64 = Text.decodeUtf8 . Base64.encode

fromBase64 :: Text -> Text -> IO ByteString
fromBase64 method t = either (throwIO . PlaywrightError method "DecodeError" . Text.pack) pure (Base64.decode (Text.encodeUtf8 t))
