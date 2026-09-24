-- | Test Halogen components in a real browser, from hspec.
--
-- A spec mounts a component into the page, finds its elements with CSS
-- selectors, acts on them and checks what it reads back. It names no monad:
-- the component runs in whatever @m@ the suite is started with, anything
-- with a 'MonadBrowserTest' instance.
--
-- @
-- spec :: forall m -> (MonadBrowserTest m) => Spec
-- spec m = describe "counter" $
--   it "counts clicks" $ do
--     ui <- mount m Counter.component ()
--     find ui "button" >>= click
--     find ui ".count" >>= (`shouldHaveText` "1")
--     query ui (H.mkRequest Counter.GetCount) `shouldReturn` Just 1
--
-- main :: IO ()
-- main = runBrowserTests (spec BrowserDOM)
-- @
--
-- The suite runs inside the page it tests: its @main@ is 'runBrowserTests', it
-- is built as a WebAssembly reactor, and @toolchain/browser-test-runner.mjs@
-- loads it into headless Chromium. That is what lets a test hold the
-- component itself rather than only the DOM it renders. The runner also backs
-- 'click' and 'typeText' with Playwright, so input arrives as trusted events,
-- to an element Playwright has checked is visible, enabled and not covered.
--
-- On other backends the package builds, so that a suite type-checks natively
-- and the language server can load it, but 'runBrowserTests' only reports
-- that it skipped the suite.
--
-- Every action waits for the component to react before it returns (see
-- 'settle'), so a synchronous @handleAction@ has rendered by then. What
-- finishes later -- a forked effect, a timer, work a JavaScript widget defers
-- to the next frame -- is what 'eventually' is for, and 'find' and the
-- @should@ helpers here already retry.
module Test.Hspec.Halogen
  ( -- * Running a suite
    runBrowserTests

    -- * Mounting
  , MonadBrowserTest (..)
  , Mounted
  , mount
  , query
  , outputs
  , root

    -- * Finding elements
  , Element
  , find
  , findAll
  , findIn
  , findAllIn

    -- * Acting
  , click
  , typeText
  , clear
  , press
  , focus
  , blur
  , settle

    -- * Reading
  , textContent
  , getProperty
  , getAttribute
  , classes
  , outerHTML
  , isVisible

    -- * Waiting and expecting
  , eventually
  , eventuallyWithin
  , shouldHaveClass
  , shouldNotHaveClass
  , shouldHaveText
  , shouldBeVisible
  , shouldBeHidden
  )
where

import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..))
import Halogen.Subscription qualified as HS
import Protolude hiding (find)
import System.Environment (withArgs)
import System.IO (BufferMode (..), hFlush, hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (assertFailure)
import Test.Hspec (Spec, around_, expectationFailure)
import Test.Hspec.Halogen.Internal.Monad (MonadBrowserTest (..))
import Test.Hspec.Halogen.Internal.Page qualified as Page
import Test.Hspec.Runner (Summary (..), defaultConfig, hspecWithResult)
import Web.DOM.Internal.Types (Element)

--------------------------------------------------------------------------------
-- Running a suite
--------------------------------------------------------------------------------

-- | Run a suite in the page and report the number of failures to the runner.
--
-- A page has no command line, so under the runner hspec's arguments
-- (@--match@ and the rest) come through a page global. In browser GHCi they
-- are whatever @:main@ was given. Every test ends by disposing of what it
-- mounted, pass or fail, so the next one starts from an empty page; so does
-- the suite, in case a run before it was interrupted.
--
-- Off the WebAssembly backend there is no page, so it says so and returns
-- without running anything.
runBrowserTests :: Spec -> IO ()
runBrowserTests spec
  | not Page.inBrowser =
      putText "hspec-halogen: skipped, this suite runs in a browser on the WebAssembly backend (npm run test-wasm)"
  | otherwise = do
      hSetBuffering stdout LineBuffering
      Page.removeLeftovers
      args <- Page.runnerArgs
      summary <- maybe identity (withArgs . map toS) args $ hspecWithResult defaultConfig (around_ (`finally` unmountAll) spec)
      hFlush stdout
      Page.reportDone (summaryFailures summary)

--------------------------------------------------------------------------------
-- Mounting
--------------------------------------------------------------------------------

-- | A component mounted into its own container in the page.
data Mounted q o m = Mounted
  { container :: Element
  , socket :: HalogenSocket q o m
  , raised :: IORef [o]
  }

-- | Every mounted component's teardown, run when its test ends.
cleanups :: IORef [IO ()]
cleanups = unsafePerformIO (newIORef [])
{-# NOINLINE cleanups #-}

unmountAll :: IO ()
unmountAll = do
  pending <- atomicModifyIORef' cleanups ([],)
  sequence_ pending

-- | Mount a component into a fresh container at the end of the body. It is
-- unmounted, and the container removed, when the test ends.
--
-- The component has rendered and run its initialisers by the time this
-- returns. Its outputs are collected from then on, for 'outputs'.
--
-- The monad is the first argument, @mount BrowserDOM component input@, so a
-- spec polymorphic in it passes it on as @mount m@.
mount :: forall m -> (MonadBrowserTest m) => Component q i o m -> i -> IO (Mounted q o m)
mount _ component input = do
  element <- Page.createContainer
  socket <- runTest $ mountInto component input element
  raised <- newIORef []
  subscription <- runTest $ HS.subscribe socket.messages $ \o -> liftIO $ modifyIORef' raised (<> [o])
  let teardown = do
        runTest $ HS.unsubscribe subscription >> socket.dispose
        Page.removeElement element
  atomicModifyIORef' cleanups (\cs -> (teardown : cs, ()))
  pure Mounted {container = element, socket, raised}

-- | Send the component a query, as a parent would.
query :: (MonadBrowserTest m) => Mounted q o m -> q a -> IO (Maybe a)
query Mounted {socket = HalogenSocket {query = send}} q = runTest (send q)

-- | Everything the component has raised since it was mounted, oldest first.
outputs :: Mounted q o m -> IO [o]
outputs ui = readIORef ui.raised

-- | The container the component renders into.
root :: Mounted q o m -> Element
root ui = ui.container

--------------------------------------------------------------------------------
-- Finding elements
--------------------------------------------------------------------------------

-- | The first element in the component matching a CSS selector, waiting for
-- one to appear. Fails with the component's markup if none does.
find :: (HasCallStack) => Mounted q o m -> Text -> IO Element
find ui = findIn ui.container

-- | Every element in the component matching a CSS selector, as it is now.
findAll :: Mounted q o m -> Text -> IO [Element]
findAll ui = findAllIn ui.container

-- | 'find' within one element rather than the whole component.
findIn :: (HasCallStack) => Element -> Text -> IO Element
findIn scope selector = eventually $ do
  Page.querySelector scope selector >>= \case
    Just found -> pure found
    Nothing -> do
      html <- outerHTML scope
      assertFailure $ toS $ "No element matches " <> show selector <> " in\n" <> html

-- | 'findAll' within one element rather than the whole component.
findAllIn :: Element -> Text -> IO [Element]
findAllIn = Page.querySelectorAll

--------------------------------------------------------------------------------
-- Acting
--------------------------------------------------------------------------------

-- | Click an element as a user would: Playwright waits until it is visible,
-- stable, enabled and not covered, then clicks its centre.
click :: Element -> IO ()
click = viaRunner "click" ""

-- | Type text into an element key by key, after what it already holds.
typeText :: Element -> Text -> IO ()
typeText element text = viaRunner "type" text element

-- | Empty an input as a user selecting all and deleting would.
clear :: Element -> IO ()
clear = viaRunner "clear" ""

-- | Press a key, or a chord such as @Shift+Tab@, on the focused element. Key
-- names are Playwright's: @Enter@, @Backspace@, @ArrowDown@, …
press :: Text -> IO ()
press key = Page.pressKey key >> settle

focus :: Element -> IO ()
focus element = Page.focusElement element >> settle

blur :: Element -> IO ()
blur element = Page.blurElement element >> settle

-- | Let the component finish reacting to what just happened in the page.
--
-- A JavaScript event reaches Haskell as a callback, which the wasm runtime
-- does not run on the spot: it schedules its scheduler loop as a task of its
-- own (@scheduler.postTask@ in Chromium), and this thread would otherwise be
-- resumed before that task ran. Waiting for a background-priority task lets
-- every task already queued at the normal priority run first. Every action
-- here ends with it; call it directly after acting on the page some other way.
settle :: IO ()
settle = Page.settleTasks

-- The runner acts on a selector, so the element is tagged with one for the
-- length of the call.
viaRunner :: Text -> Text -> Element -> IO ()
viaRunner action argument element = Page.act action element argument >> settle

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

textContent :: Element -> IO Text
textContent = Page.textContentOf

-- | A property as text, the way JavaScript's @String()@ renders it: a boolean
-- reads @true@ or @false@.
getProperty :: Element -> Text -> IO Text
getProperty = Page.propertyOf

getAttribute :: Element -> Text -> IO (Maybe Text)
getAttribute = Page.attributeOf

classes :: Element -> IO [Text]
classes element = T.words <$> getProperty element "className"

outerHTML :: Element -> IO Text
outerHTML = Page.outerHTMLOf

-- | Whether the element takes up space on the page: not hidden, not inside
-- something hidden, not @display: none@.
isVisible :: Element -> IO Bool
isVisible = Page.checkVisibility

--------------------------------------------------------------------------------
-- Waiting and expecting
--------------------------------------------------------------------------------

-- | Retry an action until it stops throwing, for up to two seconds, then
-- rethrow what it last threw. Any hspec expectation can be wrapped in it.
eventually :: IO a -> IO a
eventually = eventuallyWithin 2000

-- | 'eventually' with a timeout in milliseconds.
eventuallyWithin :: Int -> IO a -> IO a
eventuallyWithin timeoutMs action = go (timeoutMs `div` pollMs)
  where
    pollMs = 20
    go attempts =
      tryJust notAsync action >>= \case
        Right a -> pure a
        Left e
          | attempts <= 0 -> throwIO e
          | otherwise -> threadDelay (pollMs * 1000) >> go (attempts - 1)
    notAsync e = case fromException e of
      Just (_ :: SomeAsyncException) -> Nothing
      Nothing -> Just e

shouldHaveClass :: (HasCallStack) => Element -> Text -> IO ()
shouldHaveClass element name = eventually $ do
  cs <- classes element
  unless (name `elem` cs) $ failText $ "expected class " <> show name <> ", found " <> show cs

shouldNotHaveClass :: (HasCallStack) => Element -> Text -> IO ()
shouldNotHaveClass element name = eventually $ do
  cs <- classes element
  when (name `elem` cs) $ failText $ "expected no class " <> show name <> ", found " <> show cs

shouldHaveText :: (HasCallStack) => Element -> Text -> IO ()
shouldHaveText element expected = eventually $ do
  actual <- textContent element
  unless (actual == expected) $ failText $ "expected text " <> show expected <> ", found " <> show actual

shouldBeVisible :: (HasCallStack) => Element -> IO ()
shouldBeVisible element = eventually $ do
  visible <- isVisible element
  unless visible $ outerHTML element >>= \html -> failText (("expected to be visible:\n" <> html))

shouldBeHidden :: (HasCallStack) => Element -> IO ()
shouldBeHidden element = eventually $ do
  visible <- isVisible element
  when visible $ outerHTML element >>= \html -> failText (("expected to be hidden:\n" <> html))

failText :: (HasCallStack) => Text -> IO ()
failText = expectationFailure . toS
