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
-- (a request) is not part of 'settle'; 'waitQuiet' waits for that too, but
-- not for a pause the program wrote with core's @H.delay@ (a notice's
-- timer, a ticker). A timer or an emitter that has not fired is neither,
-- and is what 'eventually' and the retrying @should@ functions are for.
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
-- A step can go into a frame: 'inFrame' (by its @iframe@ element) or
-- 'frameAt' (by its address), after which the following steps are taken
-- in the frame's document.
--
-- = Requests answered by the test
--
-- 'route' hands a page's or a context's requests matching a glob to a
-- handler of the test's, which answers each ('Fulfill'), fails it
-- ('Abort') or lets it go on ('Continue'), on a thread of its own, so it
-- may block; or sends it on to the server and looks at the response
-- first ('fetchResponse', 'passThrough'). 'hold' is such a handler:
-- requests wait until 'release', as a test holds @app.wasm@ to look at the
-- prerendered page first. 'routeWith' and 'holdWith' take only some
-- resource types or methods.
--
-- = The rest of the browser
--
-- 'download' runs an action and gives the file it downloaded;
-- 'setInputFiles' chooses files for an input. 'cookies', 'addCookies' and
-- 'clearCookies' are a context's session; 'setOffline' takes it off the
-- network; 'emulateMedia' prints or changes the colour scheme, 'pdf'
-- prints. 'ContextOptions' make a phone ('isMobile', 'hasTouch', for
-- 'tap'), grant 'permissions', and answer dialogs. 'typeKeys',
-- 'insertText', 'mouseMove', 'mouseClick' and 'boundingBox' act where no
-- locator does. 'cdp' calls the Chrome DevTools Protocol; 'throttleCPU',
-- or 'throttle' in the options for every page, slows the processor down.
-- 'browserProcesses' is the browser's processes and their memory.
-- 'webSocketFrames' and 'waitForWebSocket' are what crossed the page's
-- WebSockets; 'gotoStatus' is a navigation's status; 'screenshotWith'
-- takes the window alone. 'listenCDP' keeps a DevTools event, for
-- 'cdpEvents' and 'waitForCDP'. A context whose options say
-- 'recordExchanges' keeps each request with its headers and body and its
-- response ('exchanges', 'waitForExchange').
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
  , gotoStatus
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

    -- * A prerendered page, adopted

    -- | Whether the program took the page the server sent as it was
    -- (@hydrateUI@), or repaired it: the page's nodes marked before the
    -- program arrives, and compared once it has started.
  , AdoptionOptions (..)
  , defaultAdoptionOptions
  , Repair (..)
  , adopt
  , adoptWith
  , markServed
  , repairsSinceMarked
  , describeRepair
  , shouldAdoptCleanly
  , shouldAdoptCleanlyWith
  , shouldHaveNoRepairs

    -- * Locators

    -- | Values that say how to find an element, from a page, another
    -- locator, or a frame.
  , Locator
  , Scope
  , locator
  , nth
  , first
  , lastOf
  , withText
  , withTextExact
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
  , isDisabled
  , count
  , allTextContents
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
  , waitForWebSocket
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
  , RouteFilter (..)
  , routeFilter
  , routeWith
  , unroute
  , RoutedRequest (..)
  , Asked
  , Answer (..)
  , Response (..)
  , fulfill
  , fetchResponse
  , passThrough
  , Gate
  , hold
  , holdWith
  , release
  , heldCount
  , waitHeld
  , rearm
  , closeGate

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
  , WebSocketFrame (..)
  , webSocketFrames
  , pageErrors
  , Dialog (..)
  , dialogs
  , Request (..)
  , requests
  , Exchange (..)
  , exchanges
  , waitForExchange
  , workers
  , BrowserProcess (..)
  , browserProcesses

    -- * The Chrome DevTools Protocol
  , cdp
  , throttleCPU
  , listenCDP
  , cdpEvents
  , waitForCDP

    -- * Pictures and print
  , screenshot
  , ScreenshotOptions (..)
  , defaultScreenshotOptions
  , screenshotWith
  , pdf
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay, writeTVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException, bracket, bracket_, finally, fromException, onException, throwIO, try)
import Control.Monad (unless)
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
import Data.Sequence qualified as Seq
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
  , reducedMotion :: Bool
  -- ^ Whether the pages' @prefers-reduced-motion@ media query says
  -- @reduce@, as a system set to reduce motion does: a page that animates
  -- only when it may then shows its end state at once, which a screenshot
  -- wants.
  , acceptDialogs :: Bool
  -- ^ Whether a dialog a page opens (@alert@, @confirm@, @prompt@, a
  -- @beforeunload@ asking whether to leave) is accepted rather than
  -- dismissed. Either way it is answered at once, so a page never waits on
  -- one, and kept ('dialogs').
  , recordExchanges :: Maybe Int
  -- ^ Whether the context's requests are kept with what was sent and what
  -- came back ('exchanges'), and the largest response body kept, in bytes:
  -- @Just 65536@ keeps every exchange, and the bodies of up to 64 KiB.
  -- Each costs the driver two more questions to the browser and the memory
  -- of what it keeps, which is why it is off ('Nothing') unless asked.
  }
  deriving stock (Eq, Show)

-- | No base address, Playwright's viewport and colour scheme, JavaScript
-- on, online, service workers allowed, no permissions, a desktop's
-- browser, motion not reduced, dialogs dismissed (as Playwright dismisses
-- them), exchanges not recorded.
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
    , reducedMotion = False
    , acceptDialogs = False
    , recordExchanges = Nothing
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
              , "reducedMotion" .= o.reducedMotion
              , "acceptDialogs" .= o.acceptDialogs
              , "recordExchanges" .= o.recordExchanges
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

-- | The context's exchanges so far, in the order they ended, when its
-- options record them ('recordExchanges'; none otherwise): each request
-- with every header it was sent with (its cookies too, which 'requests'
-- and a page's own view of the request leave out) and its body, and the
-- response's status, headers and body. An exchange is kept once its
-- response's body has been read, a moment after the page has the
-- response: to look for one an action caused, 'waitForExchange'.
--
-- > saved <- waitForExchange ctx (\e -> e.method == "POST" && "/api/save" `Text.isSuffixOf` e.url)
-- > saved.status `shouldBe` Just 200
exchanges :: (MonadIO m) => Context -> m [Exchange]
exchanges c = liftIO (toList <$> contextExchanges c.playwright c.handle)

-- | The first of the context's exchanges, from its start, that satisfies
-- the test, waiting for one if there is none yet, up to the options'
-- timeout: as 'waitForConsole', one that ended before the call counts. On
-- timeout the error lists the last exchanges kept (and says when the
-- context records none).
waitForExchange :: (MonadIO m) => Context -> (Exchange -> Bool) -> m Exchange
waitForExchange c test = liftIO $ do
  let ms = c.playwright.options.timeout
  found <- waitForExchangeLog c.playwright c.handle ms (find test)
  case found of
    Just e -> pure e
    Nothing -> do
      seen <- toList <$> contextExchanges c.playwright c.handle
      let shown = [e.method <> " " <> e.url <> maybe "" ((" " <>) . Text.pack . show) e.status | e <- drop (length seen - 10) seen]
      throwIO $
        PlaywrightError
          "waitForExchange"
          "TimeoutError"
          ( "no exchange of the context's satisfied the test within "
              <> Text.pack (show ms)
              <> "ms; "
              <> (if null seen then "none was kept (does the context's recordExchanges say to keep them?)" else "the last: " <> Text.intercalate "; " shown)
          )

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
goto p address = () <$ gotoStatus p address

-- | 'goto', and the status of the response the navigation ended with (after
-- any redirects): a server's 404 page, a 401 for a page that is not the
-- visitor's. 'Nothing' when there was no response: a navigation within the
-- document (to another @#fragment@), or an address that became a download.
--
-- > gotoStatus page (base <> "no-such-page/") `shouldReturn` Just 404
gotoStatus :: (MonadIO m) => Page -> Text -> m (Maybe Int)
gotoStatus p address = onPage p "goto" ["url" .= address]

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
-- @liftIO@ waits for, a request say, has come back). A fiber pausing in
-- core's @H.delay@ is not waiting, so a notice's timer written with it does
-- not hold this up, and one written @liftIO (threadDelay n)@ does. It asks
-- a few milliseconds apart. On timeout the error says how many still wait.
waitQuiet :: (MonadIO m) => Page -> m ()
waitQuiet p = onPage p "waitQuiet" []

-- | The program's reading, 'Nothing' on a page with no program that
-- answers.
activity :: (MonadIO m) => Page -> m (Maybe Activity)
activity p = onPage p "activity" []

----------------------------------------------------------------------
-- A prerendered page, adopted

-- | What 'adoptWith' and 'markServed' look at.
data AdoptionOptions = AdoptionOptions
  { root :: Text
  -- ^ The element the program mounts into, whose content is compared (not
  -- the element itself): @#app@.
  , program :: Text
  -- ^ A glob of the request that brings the program (as 'route' takes
  -- one), held until the page as the server sent it has been marked:
  -- @**\/app.wasm@.
  , leaveOut :: [Text]
  -- ^ Selectors of elements that are not compared, with everything in
  -- them: what another library fills in after the program has started (a
  -- formula's typesetting, an editor that takes the place of a
  -- @textarea@), or what the program's @Initialize@ changes on purpose,
  -- which is no repair of the server's HTML. Such an element is not looked
  -- at at all: it may be replaced, or go.
  , leaveOutContents :: [Text]
  -- ^ Selectors of elements that are compared themselves (each must be the
  -- node the server sent, with its attributes), but whose contents are
  -- not: an island a library draws in, which the program is to keep while
  -- the library fills it.
  , ignoredAttributes :: [Text]
  -- ^ Attributes the page's own scripts write, whatever the server sent:
  -- a change to one of these is not a repair.
  }
  deriving stock (Eq, Show)

-- | @#app@, @**\/app.wasm@, nothing left out, every attribute compared.
defaultAdoptionOptions :: AdoptionOptions
defaultAdoptionOptions = AdoptionOptions {root = "#app", program = "**/app.wasm", leaveOut = [], leaveOutContents = [], ignoredAttributes = []}

-- | One thing the program changed in the page the server sent, as it
-- adopted it: HTML a page rendered ahead of time got wrong, which the
-- program put right (and the reader saw change). Each has a path from the
-- root to the node (@#app > div.learn > main > p:nth-child(2) > text()@),
-- and what the node was or is: an element's outer HTML, cut short when
-- long, or a text.
data Repair
  = -- | A node of the page is gone, and another is where it was.
    Replaced {path :: Text, was :: Text, now :: Text}
  | -- | A node of the page is gone, and nothing took its place. Only the
    -- outermost of what went is listed.
    Removed {path :: Text, was :: Text}
  | -- | A node the page did not have. Only the outermost of what was
    -- built is listed.
    Built {path :: Text, now :: Text}
  | -- | A text of the page that the program kept, with another text.
    TextChanged {path :: Text, was :: Text, now :: Text}
  | -- | An attribute of an element the program kept, set, changed or taken
    -- away.
    AttributeChanged {path :: Text, attribute :: Text, before :: Maybe Text, after :: Maybe Text}
  deriving stock (Eq, Show)

instance FromJSON Repair where
  parseJSON = withObject "Repair" $ \o -> do
    path <- o .: "path"
    (o .: "kind" :: Aeson.Types.Parser Text) >>= \case
      "replaced" -> Replaced path <$> o .: "was" <*> o .: "now"
      "removed" -> Removed path <$> o .: "was"
      "built" -> Built path <$> o .: "now"
      "text" -> TextChanged path <$> o .: "was" <*> o .: "now"
      "attribute" -> AttributeChanged path <$> o .: "attribute" <*> o .:? "before" <*> o .:? "after"
      other -> fail ("no such repair: " <> Text.unpack other)

-- | A repair as a line of a report.
describeRepair :: Repair -> Text
describeRepair = \case
  Replaced {path, was, now} -> "replaced " <> path <> ": " <> was <> " by " <> now
  Removed {path, was} -> "removed " <> path <> ": " <> was
  Built {path, now} -> "built " <> path <> ": " <> now
  TextChanged {path, was, now} -> "text changed " <> path <> ": " <> quoted was <> " to " <> quoted now
  AttributeChanged {path, attribute, before, after} ->
    "attribute " <> attribute <> " changed " <> path <> ": " <> maybe "absent" quoted before <> " to " <> maybe "absent" quoted after
  where
    quoted = Text.pack . show

-- | 'adoptWith' 'defaultAdoptionOptions'.
adopt :: (MonadIO m) => Page -> Text -> m [Repair]
adopt = adoptWith defaultAdoptionOptions

-- | Go to an address whose page is prerendered, and say what the program
-- repaired as it adopted it: the program's request ('program') held, the
-- page as the server sent it marked ('markServed') once it has asked for
-- the program, the program let go, its start waited for ('waitStarted':
-- the adopting render and every @Initialize@'s synchronous part are done),
-- and the page compared with what was marked ('repairsSinceMarked'). An
-- empty list is a page adopted with nothing repaired. The gate is released
-- at the end and left in place, letting the page's later loads straight
-- through: it is not taken away ('closeGate'), since the program's own
-- requests start as it does, and taking a route away while a page's
-- requests are under way can leave them stalled (the tutorial's test saw a
-- compiler's first request never answered). So each call leaves one more
-- released gate on the page, asked about every later request for the
-- program: a test that adopts many addresses on one page holds one gate of
-- its own, armed again for each ('rearm'), between the two halves
-- ('markServed', 'repairsSinceMarked').
--
-- What the program changes after it has started (a timer's render, a
-- response's) is not compared; what its @Initialize@ changes is, unless it
-- is under 'leaveOut'.
adoptWith :: (MonadIO m) => AdoptionOptions -> Page -> Text -> m [Repair]
adoptWith options page address = liftIO $ do
  gate <- hold page options.program
  ( do
      goto page address
      waitHeld gate
      markServed options page
      release gate
      waitStarted page
      repairsSinceMarked page
    )
    `finally` release gate

-- | Mark the page as it is now, as the server sent it before its program
-- has run: every node under the 'root' (but what 'leaveOut' names, and
-- what is inside what 'leaveOutContents' names), with its text or its
-- attributes, for 'repairsSinceMarked' to compare
-- with. The marks are kept in the page, by node, so that a node the program
-- replaced with an equal one is told from one it kept. For a test with a
-- gate of its own, or with more to do between marking and comparing; else
-- 'adoptWith' does the whole of it.
markServed :: (MonadIO m) => AdoptionOptions -> Page -> m ()
markServed options page = do
  _ :: Int <- evaluateWith page markScript (object ["root" .= options.root, "leaveOut" .= options.leaveOut, "leaveOutContents" .= options.leaveOutContents, "ignored" .= options.ignoredAttributes])
  pure ()

-- | What has changed since 'markServed', in the page as it is now: the
-- nodes marked that are gone (replaced, or removed), the nodes there that
-- were not marked (built), the texts and the attributes changed. Gone
-- texts of white space alone are not listed: the HTML parser makes them of
-- the space between tags, and a program that renders none takes them away
-- with nothing on the page changing. Nor is an empty text built.
repairsSinceMarked :: (MonadIO m) => Page -> m [Repair]
repairsSinceMarked page = evaluateWith page compareScript ()

-- | Adopting the page at the address repairs nothing ('adopt'); the
-- failure lists every repair.
shouldAdoptCleanly :: (HasCallStack, MonadIO m) => Page -> Text -> m ()
shouldAdoptCleanly page address = withFrozenCallStack (shouldAdoptCleanlyWith defaultAdoptionOptions page address)

-- | 'shouldAdoptCleanly' with options ('adoptWith').
shouldAdoptCleanlyWith :: (HasCallStack, MonadIO m) => AdoptionOptions -> Page -> Text -> m ()
shouldAdoptCleanlyWith options page address = liftIO $ withFrozenCallStack $ do
  repairs <- adoptWith options page address
  unless (null repairs) (failWith (reportRepairs ("adopting the page at " <> address) repairs))

-- | No repairs, from 'repairsSinceMarked' after a gate of the test's own;
-- the failure lists them.
shouldHaveNoRepairs :: (HasCallStack, MonadIO m) => [Repair] -> m ()
shouldHaveNoRepairs repairs = liftIO $ withFrozenCallStack $ unless (null repairs) (failWith (reportRepairs "adopting the page" repairs))

reportRepairs :: Text -> [Repair] -> Text
reportRepairs what repairs =
  what <> " repaired " <> Text.pack (show (length repairs)) <> " thing(s):\n" <> Text.unlines (map (("  " <>) . describeRepair) repairs)

-- | The page's half of 'markServed'. A node's path names each element by
-- its tag, its id, two of its classes and its place among its parent's
-- elements, so that a reader of a failure can find it; a text by its place
-- among its parent's texts. The marks are kept under a symbol of the
-- global object, where the page's own code does not look.
markScript :: Text
markScript =
  Text.unlines
    [ "(o) => {"
    , "  const root = document.querySelector(o.root);"
    , "  if (root === null) throw new Error(`no element matches ${o.root}: there is nothing to mark`);"
    , "  const skip = o.leaveOut.join(', ');"
    , "  const opaque = o.leaveOutContents.join(', ');"
    , "  const marks = new Map();"
    , adoptionHelpers
    , "  const visit = (parent, path) => {"
    , "    for (const node of parent.childNodes) {"
    , "      if (node.nodeType === Node.ELEMENT_NODE) {"
    , "        if (skip !== '' && node.matches(skip)) continue;"
    , "        const at = `${path} > ${segment(node)}`;"
    , "        marks.set(node, { path: at, parent, index: place(node), text: null, html: html(node),"
    , "          attributes: Object.fromEntries([...node.attributes].map((a) => [a.name, a.value])) });"
    , "        if (opaque === '' || !node.matches(opaque)) visit(node, at);"
    , "      } else if (node.nodeType === Node.TEXT_NODE) {"
    , "        marks.set(node, { path: `${path} > ${segment(node)}`, parent, index: place(node), text: node.data, html: null });"
    , "      }"
    , "    }"
    , "  };"
    , "  visit(root, o.root);"
    , "  globalThis[Symbol.for('halogen.playwright.adoption')] = { root: o.root, skip, opaque, ignored: o.ignored, marks };"
    , "  return marks.size;"
    , "}"
    ]

-- | The page's half of 'repairsSinceMarked'.
compareScript :: Text
compareScript =
  Text.unlines
    [ "() => {"
    , "  const state = globalThis[Symbol.for('halogen.playwright.adoption')];"
    , "  if (!state) throw new Error('nothing was marked: markServed first');"
    , "  const { skip, opaque, ignored, marks } = state;"
    , "  const root = document.querySelector(state.root);"
    , adoptionHelpers
    , "  const here = (node) => root !== null && root !== node && root.contains(node);"
    , "  const gone = [];"
    , "  const repairs = [];"
    , "  for (const [node, mark] of marks) {"
    , "    if (!here(node)) {"
    , "      if (mark.text !== null && mark.text.trim() === '') continue;"
    , "      // The outermost of what went: its parent is still there."
    , "      if (marks.has(mark.parent) && !here(mark.parent)) continue;"
    , "      gone.push(mark);"
    , "    } else if (mark.text !== null) {"
    , "      if (node.data !== mark.text) repairs.push({ kind: 'text', path: mark.path, was: mark.text, now: node.data });"
    , "    } else {"
    , "      const now = Object.fromEntries([...node.attributes].map((a) => [a.name, a.value]));"
    , "      for (const name of new Set([...Object.keys(mark.attributes), ...Object.keys(now)])) {"
    , "        if (ignored.includes(name) || mark.attributes[name] === now[name]) continue;"
    , "        repairs.push({ kind: 'attribute', path: mark.path, attribute: name, before: mark.attributes[name] ?? null, after: now[name] ?? null });"
    , "      }"
    , "    }"
    , "  }"
    , "  const built = [];"
    , "  const visit = (parent, path) => {"
    , "    for (const node of parent.childNodes) {"
    , "      const element = node.nodeType === Node.ELEMENT_NODE;"
    , "      if (!element && node.nodeType !== Node.TEXT_NODE) continue;"
    , "      if (element && skip !== '' && node.matches(skip)) continue;"
    , "      const at = `${path} > ${segment(node)}`;"
    , "      if (!marks.has(node)) {"
    , "        if (!element && node.data === '') continue;"
    , "        built.push({ path: at, parent, index: place(node), now: element ? html(node) : node.data });"
    , "      } else if (element && (opaque === '' || !node.matches(opaque))) visit(node, at);"
    , "    }"
    , "  };"
    , "  if (root !== null) visit(root, state.root);"
    , "  // A node built where one went, in the same parent at the same place,"
    , "  // replaced it."
    , "  for (const mark of gone) {"
    , "    const i = built.findIndex((b) => b.parent === mark.parent && b.index === mark.index);"
    , "    const was = mark.text ?? mark.html;"
    , "    if (i >= 0) {"
    , "      const [b] = built.splice(i, 1);"
    , "      repairs.push({ kind: 'replaced', path: mark.path, was, now: b.now });"
    , "    } else repairs.push({ kind: 'removed', path: mark.path, was });"
    , "  }"
    , "  for (const b of built) repairs.push({ kind: 'built', path: b.path, now: b.now });"
    , "  return repairs;"
    , "}"
    ]

-- | What both halves name nodes with.
adoptionHelpers :: Text
adoptionHelpers =
  Text.unlines
    [ "  // A node's place among its parent's elements and texts. Comments, which"
    , "  // only keep texts apart for the HTML parser, and the space between tags,"
    , "  // which a render does not make, do not count."
    , "  const counted = (n) => n.nodeType === Node.ELEMENT_NODE || (n.nodeType === Node.TEXT_NODE && n.data.trim() !== '');"
    , "  const place = (node) => [...node.parentNode.childNodes].filter(counted).indexOf(node);"
    , "  const html = (e) => { const h = e.outerHTML; return h.length > 160 ? `${h.slice(0, 159)}…` : h; };"
    , "  const segment = (node) => {"
    , "    const parent = node.parentNode;"
    , "    if (node.nodeType === Node.TEXT_NODE) {"
    , "      const texts = [...parent.childNodes].filter((n) => n.nodeType === Node.TEXT_NODE);"
    , "      return texts.length > 1 ? `text()[${texts.indexOf(node) + 1}]` : 'text()';"
    , "    }"
    , "    const classes = [...node.classList].slice(0, 2).map((c) => `.${c}`).join('');"
    , "    const id = node.id ? `#${node.id}` : '';"
    , "    const siblings = [...parent.children];"
    , "    const nth = siblings.length > 1 ? `:nth-child(${siblings.indexOf(node) + 1})` : '';"
    , "    return `${node.localName}${id}${classes}${nth}`;"
    , "  };"
    ]

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

-- | The last match (Playwright's @last@; named so that it does not meet the
-- Prelude's @last@ in a test that imports this module whole).
lastOf :: Locator -> Locator
lastOf l = l {steps = l.steps <> [object ["nth" .= (-1 :: Int)]]}

-- | Those of the matches that contain this text, somewhere inside them
-- (Playwright's @hasText@: ignoring case and runs of white space).
withText :: Text -> Locator -> Locator
withText t l = l {steps = l.steps <> [object ["hasText" .= t]]}

-- | Those of the matches whose whole text is this one, exactly: so
-- @withTextExact "Version 2"@ is neither @Version 2 (current)@ nor
-- @Version 12@, which 'withText' would both take. The text is compared as
-- it is (Playwright's @hasText@ with the regular expression
-- @^Version 2$@): case counts, and so does white space, at the ends too.
withTextExact :: Text -> Locator -> Locator
withTextExact t l = l {steps = l.steps <> [object ["hasTextExact" .= t]]}

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

-- | 'isVisible' does not wait: it answers for the page as it is. The others
-- wait for the element, as an action does.
isVisible, isChecked, isEnabled, isDisabled :: (MonadIO m) => Locator -> m Bool
isVisible l = on l "isVisible" []
isChecked l = on l "isChecked" []
isEnabled l = on l "isEnabled" []
isDisabled l = on l "isDisabled" []

-- | The text of every element the locator matches, in the page's order;
-- an empty list when none does: like 'count', this does not wait.
allTextContents :: (MonadIO m) => Locator -> m [Text]
allTextContents l = on l "allTextContents" []

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
-- page's own frames, say. A navigation under the wait starts it again in
-- the new document; what the expression throws is the call's error.
--
-- It works under a Content Security Policy that refuses @eval@ (no
-- @'unsafe-eval'@), unlike Playwright's own @waitForFunction@, which
-- compiles the expression in the page with @eval@: the expression is
-- compiled with the loop that polls it, by the DevTools protocol, which
-- the page's policy does not govern. So do the other waits of this module.
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

-- | The first frame of the page's WebSockets, from the page's start, that
-- satisfies the test, waiting for one if there is none yet, up to the page's
-- timeout: as 'waitForConsole', a frame that crossed before the call counts,
-- so that a frame an action (or the page's load) caused cannot be missed.
-- To wait for a later one, make the test tell it apart (by its payload, or
-- by counting 'webSocketFrames' first). The failure lists the last frames
-- seen.
--
-- > -- The server's answer to the page's subscription: pushes to it follow.
-- > _ <- waitForWebSocket page (\f -> not f.sent && "\"subscribed\"" `Text.isInfixOf` f.payload)
waitForWebSocket :: (MonadIO m) => Page -> (WebSocketFrame -> Bool) -> m WebSocketFrame
waitForWebSocket p test = liftIO $ do
  found <- waitForLog p.playwright p.handle (timeoutOf p) (find test . (.webSocketFrames))
  case found of
    Just f -> pure f
    Nothing -> do
      seen <- toList . (.webSocketFrames) <$> pageLog p.playwright p.handle
      let shown = [(if f.sent then "sent " else "received ") <> f.url <> " " <> f.payload | f <- drop (length seen - 10) seen]
      throwIO $
        PlaywrightError
          "waitForWebSocket"
          "TimeoutError"
          ( "no WebSocket frame of the page's satisfied the test within "
              <> Text.pack (show (timeoutOf p))
              <> "ms; "
              <> (if null seen then "no frame crossed" else "the last frames: " <> Text.intercalate "; " shown)
          )

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
-- promise), as JSON. A promise that has not settled by the page's timeout
-- is a @TimeoutError@ (it is left to end with the page). Like the other
-- three below, this is not an action and does not 'settle': after code that
-- dispatches an event to the program, call 'settle'.
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

-- | Every frame of the page's WebSockets so far, both ways, oldest first.
webSocketFrames :: (MonadIO m) => Page -> m [WebSocketFrame]
webSocketFrames p = liftIO (toList . (.webSocketFrames) <$> pageLog p.playwright p.handle)

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

-- | One of the browser's processes, as the operating system counts it.
data BrowserProcess = BrowserProcess
  { pid :: Int
  , kind :: Text
  -- ^ Chromium's @--type@ for it: @renderer@ (a page's, which runs its
  -- JavaScript and WebAssembly), @gpu-process@, @utility@, @zygote@; or
  -- @browser@ for the browser's own process.
  , rss :: Int
  -- ^ The bytes it has resident now (@VmRSS@).
  , peak :: Int
  -- ^ The most bytes it has had resident since it started (@VmHWM@).
  }
  deriving stock (Eq, Show)

-- | The processes of the browser the page is in, every page's and not
-- only this one's, with their memory, read from @/proc@: none where there
-- is no @/proc@ (on Linux only). For what a page costs the machine, which
-- the page cannot see from inside: a renderer's peak is what a program
-- that compiles in the page (the tutorial's GHC) needs at most, its JS
-- heap and its WebAssembly memories together.
browserProcesses :: (MonadIO m) => Page -> m [BrowserProcess]
browserProcesses p = liftIO $ do
  node <- driverProcessId p.playwright
  family <- maybe (pure []) descendants node
  concat <$> traverse described family
  where
    described pid = do
      read' <- try @SomeException ((,) <$> readProcFile pid "status" <*> readProcFile pid "cmdline")
      pure $ case read' of
        -- Gone since it was listed.
        Left _ -> []
        Right (status, cmdline) ->
          [ BrowserProcess
              { pid = fromIntegral pid
              , kind = fromMaybe "browser" (lookup "--type" (map flag (args cmdline)))
              , rss = kilobytes "VmRSS" status
              , peak = kilobytes "VmHWM" status
              }
          ]
    flag arg = let (name, value) = break (== '=') arg in (name, Text.pack (drop 1 value))
    -- The arguments are NUL-separated, but Chromium sets its processes'
    -- titles, which joins them with spaces: either separates them here.
    args = words . map (\c -> if c == '\0' then ' ' else c)
    -- A field of status, "VmRSS:     123456 kB", in bytes.
    kilobytes field status =
      case [rest | line <- lines status, Just rest <- [Text.stripPrefix (Text.pack field <> ":") (Text.pack line)]] of
        rest : _ | [n, "kB"] <- Text.words rest, Just k <- readMaybe (Text.unpack n) -> k * 1024
        _ -> 0

-- | The whole page, as a PNG: 'screenshotWith' 'defaultScreenshotOptions'.
screenshot :: (MonadIO m) => Page -> FilePath -> m ()
screenshot p = screenshotWith p defaultScreenshotOptions

-- | What 'screenshotWith' takes.
newtype ScreenshotOptions = ScreenshotOptions
  { fullPage :: Bool
  -- ^ The whole page, however long ('True'), or only what the viewport
  -- shows ('False'): a picture of the window, the size the context's
  -- 'viewport' says.
  }
  deriving stock (Eq, Show)

-- | The whole page.
defaultScreenshotOptions :: ScreenshotOptions
defaultScreenshotOptions = ScreenshotOptions {fullPage = True}

-- | A picture of the page, written to a path: a PNG, or a JPEG if the path
-- ends in @.jpg@ or @.jpeg@. It does not settle: 'settle' or 'waitQuiet'
-- first, and wait for what the page draws by itself (fonts, images).
--
-- > screenshotWith page defaultScreenshotOptions {fullPage = False} "home.png"
screenshotWith :: (MonadIO m) => Page -> ScreenshotOptions -> FilePath -> m ()
screenshotWith p o path = onPage p "screenshot" ["path" .= path, "fullPage" .= o.fullPage]

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
  -- ^ Playwright's @headers()@, documented to leave out @cookie@ and the
  -- other headers the browser adds for its security (Chromium gives them
  -- for a routed request all the same): 'allHeaders' has them all.
  , allHeaders :: [(Text, Text)]
  -- ^ Every header the browser sends it with, @cookie@ among them, each
  -- name in lower case.
  , postData :: Maybe ByteString
  , asked :: Asked
  -- ^ The request as the driver holds it, for 'fetchResponse'.
  }
  deriving stock (Eq, Show)

-- | A routed request as the driver holds it while its handler runs: what
-- 'fetchResponse' sends on. Two are equal when they are the same ask.
data Asked = Asked
  { playwright :: Playwright
  , number :: Int
  , timeoutMs :: Int
  }

instance Eq Asked where
  a == b = a.number == b.number

instance Show Asked where
  show a = "Asked " <> show a.number

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

-- | Inside a route's handler: send the request on to the server, as it is,
-- and the response, for the handler to look at, or to change, before it
-- answers with it ('Fulfill'). The response's body is the one the page
-- would read (decoded), so its headers leave out those that described the
-- bytes on the wire (@content-length@, @content-encoding@,
-- @transfer-encoding@); its media type is among its headers (the
-- 'contentType' field is 'Nothing'), each name in lower case. The handler's
-- route's timeout bounds it; a request whose route has gone meanwhile is a
-- 'PlaywrightError'.
--
-- > _ <- route page "**/api/settings" $ \request -> do
-- >   r <- fetchResponse request
-- >   pure (Fulfill r {body = "{\"theme\": \"dark\"}"})
fetchResponse :: (MonadIO m) => RoutedRequest -> m Response
fetchResponse request = liftIO $ do
  let a = request.asked
  fetched <- call a.playwright "fetchRouted" ["ask" .= a.number, "timeout" .= a.timeoutMs]
  case Aeson.Types.parseEither parser fetched of
    Right r -> pure r
    Left e -> throwIO (PlaywrightError "fetchResponse" "DecodeError" (Text.pack e))
  where
    parser = withObject "response" $ \o -> do
      status <- o .: "status"
      headers <- o .: "headers" >>= traverse headerPair
      body <- o .: "body" >>= either (fail . ("body: " <>)) pure . Base64.decode . Text.encodeUtf8
      pure Response {status, headers, contentType = Nothing, body}
    headerPair = \case
      [name, value] -> pure (name, value)
      other -> fail ("a header is a name and a value: " <> show (other :: [Text]))

-- | A route's handler that only looks: each request goes to the server
-- ('fetchResponse'), the function is given it and its response, and the
-- page gets the response as it came.
--
-- > seen <- newIORef []
-- > _ <- route page "**/api/**" $ passThrough $ \request response ->
-- >   atomicModifyIORef' seen (\rs -> (rs <> [(request.url, response.status)], ()))
passThrough :: (RoutedRequest -> Response -> IO ()) -> RoutedRequest -> IO Answer
passThrough look request = do
  response <- fetchResponse request
  look request response
  pure (Fulfill response)

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
route s pattern = routeWith s (routeFilter pattern)

-- | Which requests a route ('routeWith') or a gate ('holdWith') takes: those
-- whose address matches the glob, and, when the lists are not empty, whose
-- resource type and method are among them. The lists are decided by the
-- driver before the test is asked, so a request they leave out costs
-- nothing and is not seen by the handler at all: it goes on as if the
-- route were not there.
data RouteFilter = RouteFilter
  { pattern :: Text
  -- ^ As 'route' takes it.
  , resourceTypes :: [Text]
  -- ^ Playwright's names (@document@, @script@, @fetch@, @xhr@, @image@,
  -- @stylesheet@, @font@, @media@, @websocket@, @other@, ...); none is
  -- any.
  , methods :: [Text]
  -- ^ @GET@, @POST@, ..., in any case; none is any.
  }
  deriving stock (Eq, Show)

-- | Every request whose address matches the glob, of any kind.
routeFilter :: Text -> RouteFilter
routeFilter pattern = RouteFilter {pattern, resourceTypes = [], methods = []}

-- | 'route' for the requests a filter takes: an API's @fetch@ requests and
-- not its images, its @POST@s and not its @GET@s.
--
-- > _ <- routeWith page (routeFilter "**/api/**") {methods = ["POST"]} (\_ -> pure (fulfill 503 Nothing ""))
routeWith :: (Routed s, MonadIO m) => s -> RouteFilter -> (RoutedRequest -> IO Answer) -> m Route
routeWith s f handler = liftIO $ do
  let (pw, context, target, ms) = routedOn s
      fields = ["resourceTypes" .= f.resourceTypes | not (null f.resourceTypes)] <> ["methods" .= f.methods | not (null f.methods)]
  handle <- addRouteWith pw context target f.pattern fields (\ask request -> answerFields <$> (parseRequest (Asked pw ask ms) request >>= handler))
  pure Route {playwright = pw, handle, pattern = f.pattern}
  where
    parseRequest asked o = case Aeson.Types.parseEither (parser asked) o of
      Right r -> pure r
      Left e -> throwIO (PlaywrightError "route" "DecodeError" (Text.pack e))
    parser asked o = do
      headerMap <- o .: "headers"
      allMap <- fromMaybe headerMap <$> o .:? "allHeaders"
      encoded <- o .:? "postData"
      postData <- traverse (either (fail . ("postData: " <>)) pure . Base64.decode . Text.encodeUtf8) encoded
      RoutedRequest
        <$> o .: "url"
        <*> o .: "method"
        <*> o .: "resourceType"
        <*> pure (pairs headerMap)
        <*> pure [(Text.toLower k, v) | (k, v) <- pairs allMap]
        <*> pure postData
        <*> pure asked
    pairs headerMap = [(Key.toText k, v) | (k, v) <- KeyMap.toList (headerMap :: KeyMap.KeyMap Text)]
    answerFields = \case
      Continue -> ["action" .= ("continue" :: Text)]
      Abort code -> ["action" .= ("abort" :: Text), "error" .= code]
      Fulfill r ->
        [ "action" .= ("fulfill" :: Text)
        , "status" .= r.status
        , "headers" .= headerObject r.headers
        , "contentType" .= r.contentType
        , "body" .= base64 r.body
        ]

-- | Headers as Playwright takes them, an object: a name given more than
-- once is one entry, its values joined as Playwright joins them (a
-- @set-cookie@'s by a new line, which Playwright sends as that many
-- headers; another's by a comma).
headerObject :: [(Text, Text)] -> Value
headerObject hs = object [Key.fromText name .= joined name [v | (k, v) <- hs, Text.toLower k == name] | name <- names]
  where
    names = nubOrdered (map (Text.toLower . fst) hs)
    joined name = Text.intercalate (if name == "set-cookie" then "\n" else ", ")
    nubOrdered = foldr (\n acc -> n : filter (/= n) acc) []

-- | Take a route away. Its handlers under way are killed and their
-- requests go on, as if the route had not been there, and later requests
-- do not see it: the requests in flight are let go before Playwright's
-- route is removed, so nothing is left waiting on a handler that will not
-- answer. Nothing on a route already gone with its page, or a driver that
-- has ended.
--
-- Take a route away while the page is at rest. When a page's last route
-- goes, the browser stops handing the page's requests to Playwright, and a
-- request of the page's that was under way at that moment, whether the
-- route matched it or not, has been seen left unanswered (once, in the
-- tutorial's test, as its compiler started loading). A
-- route that only has to stop answering is better left in place with a
-- handler that says 'Continue', as a released gate is.
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
hold s pattern = holdWith s (routeFilter pattern)

-- | 'hold' for the requests a filter takes: an API's @fetch@ requests,
-- while the page's images, which its @load@ waits for, go through.
--
-- > gate <- holdWith page (routeFilter "**/api/**") {resourceTypes = ["fetch"]}
holdWith :: (Routed s, MonadIO m) => s -> RouteFilter -> m Gate
holdWith s f = liftIO $ do
  open <- newTVarIO False
  waiting <- newTVarIO 0
  let (_, _, _, ms) = routedOn s
      held _ = do
        bracket_
          (atomically (modifyTVar' waiting (+ 1)))
          (atomically (modifyTVar' waiting (subtract 1)))
          (atomically (readTVar open >>= STM.check))
        pure Continue
  r <- routeWith s f held
  pure Gate {route = r, open, waiting, timeoutMs = ms}

-- | Let the held requests go on, and every later one pass straight
-- through.
release :: (MonadIO m) => Gate -> m ()
release g = liftIO (atomically (writeTVar g.open True))

-- | Close a gate that was released again: the requests that match from
-- now on are held until the next 'release'. One gate serves a walk that
-- holds every load (@rearm@, 'goto', 'waitHeld', ..., 'release'), rather
-- than a new 'hold' per load, whose released routes would pile up.
-- Requests let through before are not held back.
rearm :: (MonadIO m) => Gate -> m ()
rearm g = liftIO (atomically (writeTVar g.open False))

-- | Take the gate away: the requests it holds go on, as on 'release', and
-- later ones do not pass it at all ('unroute'). For a page at rest, as
-- 'unroute' says: where the page's own requests start as the gate lets its
-- program go, 'release' it and leave it.
closeGate :: (MonadIO m) => Gate -> m ()
closeGate g = release g >> unroute g.route

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

-- | Keep, from now on, every occurrence of a DevTools event on the page's
-- session (the one 'cdp' calls on), with its parameters, for 'cdpEvents'
-- and 'waitForCDP': the way the page's console is kept. Most events come
-- only once their domain is enabled, by a call on the same session:
--
-- > listenCDP page "Network.webSocketFrameReceived"
-- > _ :: Value <- cdp page "Network.enable" ()
--
-- Listening twice to an event is listening once.
listenCDP :: (MonadIO m) => Page -> Text -> m ()
listenCDP p event = onPage p "listenCDP" ["cdpEvent" .= event]

-- | The parameters of every occurrence of the event kept so far ('listenCDP'),
-- oldest first.
cdpEvents :: (MonadIO m) => Page -> Text -> m [Value]
cdpEvents p event = liftIO $ do
  kept <- toList . (.cdpEvents) <$> pageLog p.playwright p.handle
  pure [params | (name, params) <- kept, name == event]

-- | The parameters of the first occurrence of the event, since 'listenCDP',
-- that satisfy the test, waiting for one if there is none yet, up to the
-- page's timeout: as 'waitForConsole', one that came before the call
-- counts.
--
-- > sent <- waitForCDP page "Network.requestWillBeSent" (\v -> address `Text.isInfixOf` Text.pack (show v))
waitForCDP :: (MonadIO m) => Page -> Text -> (Value -> Bool) -> m Value
waitForCDP p event test = liftIO $ do
  found <- waitForLog p.playwright p.handle (timeoutOf p) (find test . fmap snd . Seq.filter ((== event) . fst) . (.cdpEvents))
  case found of
    Just v -> pure v
    Nothing -> do
      seen <- length . Seq.filter ((== event) . fst) . (.cdpEvents) <$> pageLog p.playwright p.handle
      throwIO $
        PlaywrightError
          "waitForCDP"
          "TimeoutError"
          ( "no "
              <> event
              <> " satisfied the test within "
              <> Text.pack (show (timeoutOf p))
              <> "ms; "
              <> (if seen == 0 then "none came (is it listened to, with listenCDP, and its domain enabled?)" else Text.pack (show seen) <> " came")
          )

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
