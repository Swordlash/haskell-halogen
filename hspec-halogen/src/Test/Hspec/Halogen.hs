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
--   it "counts clicks" $ withPage $ \page -> do
--     ui <- mount page m Counter.component ()
--     find ui "button" >>= click
--     find ui ".count" >>= (`shouldHaveText` "1")
--     query ui (H.mkRequest Counter.GetCount) `shouldReturn` Just 1
--
-- main :: IO ()
-- main = runBrowserTests (spec BrowserDOM)
-- @
--
-- Each test works in a 'Page' of its own, which 'withPage' opens and tears
-- down. Like an @STRef s@, nothing typed with its @s@ -- the page, what was
-- mounted on it, the elements found there -- can outlive it, so no test can
-- reach into another's. A page has one mouse, one keyboard and one focused
-- element, so pages are opened one at a time: tests marked @parallel@ still
-- run in turn.
--
-- The suite runs inside the page it tests: its @main@ is 'runBrowserTests', it
-- is built as a WebAssembly reactor, and the @hspec-halogen@ executable, as
-- cabal's test wrapper, loads it into headless Chromium. That is what lets a test hold the
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

    -- * Pages
  , Page
  , withPage

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

import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..))
import Halogen.Subscription qualified as HS
import Protolude hiding (find)
import System.Environment (withArgs)
import System.IO (BufferMode (..), hFlush, hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (assertFailure)
import Test.Hspec (Spec, expectationFailure)
import Test.Hspec.Halogen.Internal.Monad (MonadBrowserTest (..))
import Test.Hspec.Halogen.Internal.Page qualified as Page
import Test.Hspec.Runner (Summary (..), defaultConfig, hspecWithResult)
import Web.DOM.Internal.Types qualified as DOM

--------------------------------------------------------------------------------
-- Running a suite
--------------------------------------------------------------------------------

-- | Run a suite in the page and report the number of failures to the runner.
--
-- A page has no command line, so under the runner hspec's arguments
-- (@--match@ and the rest) come through a page global. In browser GHCi they
-- are whatever @:main@ was given. It starts by removing whatever a run before
-- it left behind, in case that one was interrupted.
--
-- Off the WebAssembly backend there is no page, so it says so and returns
-- without running anything.
runBrowserTests :: Spec -> IO ()
runBrowserTests spec
  | not Page.inBrowser =
      putText "hspec-halogen: skipped, this suite runs in a browser on the WebAssembly backend"
  | otherwise = do
      hSetBuffering stdout LineBuffering
      Page.removeLeftovers
      args <- Page.runnerArgs
      summary <- maybe identity (withArgs . map toS) args $ hspecWithResult defaultConfig spec
      hFlush stdout
      Page.reportDone (summaryFailures summary)

--------------------------------------------------------------------------------
-- Pages
--------------------------------------------------------------------------------

-- | One test's share of the browser: a container at the end of the body for
-- what it mounts, and the teardown of everything mounted there.
data Page s = Page
  { container :: DOM.Element
  , teardowns :: IORef [IO ()]
  }

-- | An element of the page a test is working in.
newtype Element s = Element DOM.Element

-- | Run a test in a page of its own, and tear down everything mounted on it
-- when the test ends, pass or fail.
--
-- The @forall s@ keeps what the test finds and mounts inside it, as 'runST'
-- keeps an @STRef@. Only one page is open at a time -- the browser has one
-- mouse, one keyboard and one focused element to share -- so a test waits
-- here for the one before it, whatever hspec was told about parallelism.
-- Opening a page inside another would wait on itself, and fails instead.
withPage :: (forall s. Page s -> IO a) -> IO a
withPage test = do
  me <- myThreadId
  holder <- readIORef pageHolder
  when (holder == Just me) $
    panic "hspec-halogen: withPage inside withPage; a test works in one page"
  withMVar pageLock $ \() ->
    bracket open close $ \page -> do
      atomicWriteIORef pageHolder (Just me)
      test page
  where
    open = Page <$> Page.createContainer <*> newIORef []
    close page = do
      atomicWriteIORef pageHolder Nothing
      pending <- atomicModifyIORef' page.teardowns ([],)
      sequence_ pending
      Page.removeElement page.container

pageLock :: MVar ()
pageLock = unsafePerformIO (newMVar ())
{-# NOINLINE pageLock #-}

pageHolder :: IORef (Maybe ThreadId)
pageHolder = unsafePerformIO (newIORef Nothing)
{-# NOINLINE pageHolder #-}

--------------------------------------------------------------------------------
-- Mounting
--------------------------------------------------------------------------------

-- | A component mounted on a page.
data Mounted s q o m = Mounted
  { container :: DOM.Element
  , socket :: HalogenSocket q o m
  , raised :: IORef [o]
  }

-- | Mount a component into a container of its own on the page. It is
-- unmounted when the page is torn down.
--
-- The component has rendered and run its initialisers by the time this
-- returns. Its outputs are collected from then on, for 'outputs'.
--
-- The monad follows the page, @mount page BrowserDOM component input@, so a
-- spec polymorphic in it passes it on as @mount page m@.
mount :: Page s -> forall m -> (MonadBrowserTest m) => Component q i o m -> i -> IO (Mounted s q o m)
mount page _ component input = do
  element <- Page.createContainerIn page.container
  socket <- runTest $ mountInto component input element
  raised <- newIORef []
  subscription <- runTest $ HS.subscribe socket.messages $ \o -> liftIO $ modifyIORef' raised (<> [o])
  let teardown = runTest $ HS.unsubscribe subscription >> socket.dispose
  atomicModifyIORef' page.teardowns (\ts -> (teardown : ts, ()))
  pure Mounted {container = element, socket, raised}

-- | Send the component a query, as a parent would.
query :: (MonadBrowserTest m) => Mounted s q o m -> q a -> IO (Maybe a)
query Mounted {socket = HalogenSocket {query = send}} q = runTest (send q)

-- | Everything the component has raised since it was mounted, oldest first.
outputs :: Mounted s q o m -> IO [o]
outputs ui = readIORef ui.raised

-- | The container the component renders into.
root :: Mounted s q o m -> Element s
root ui = Element ui.container

--------------------------------------------------------------------------------
-- Finding elements
--------------------------------------------------------------------------------

-- | The first element in the component matching a CSS selector, waiting for
-- one to appear. Fails with the component's markup if none does.
find :: (HasCallStack) => Mounted s q o m -> Text -> IO (Element s)
find ui = findIn (root ui)

-- | Every element in the component matching a CSS selector, as it is now.
findAll :: Mounted s q o m -> Text -> IO [Element s]
findAll ui = findAllIn (root ui)

-- | 'find' within one element rather than the whole component.
findIn :: (HasCallStack) => Element s -> Text -> IO (Element s)
findIn scope@(Element element) selector = eventually $ do
  Page.querySelector element selector >>= \case
    Just found -> pure (Element found)
    Nothing -> do
      html <- outerHTML scope
      assertFailure $ toS $ "No element matches " <> show selector <> " in\n" <> html

-- | 'findAll' within one element rather than the whole component.
findAllIn :: Element s -> Text -> IO [Element s]
findAllIn (Element element) selector = map Element <$> Page.querySelectorAll element selector

--------------------------------------------------------------------------------
-- Acting
--------------------------------------------------------------------------------

-- | Click an element as a user would: Playwright waits until it is visible,
-- stable, enabled and not covered, then clicks its centre.
click :: Element s -> IO ()
click = viaRunner "click" ""

-- | Type text into an element key by key, after what it already holds.
typeText :: Element s -> Text -> IO ()
typeText element text = viaRunner "type" text element

-- | Empty an input as a user selecting all and deleting would.
clear :: Element s -> IO ()
clear = viaRunner "clear" ""

-- | Press a key, or a chord such as @Shift+Tab@, on the focused element. Key
-- names are Playwright's: @Enter@, @Backspace@, @ArrowDown@, …
--
-- It takes the page only to show that it happens in one.
press :: Page s -> Text -> IO ()
press _ key = Page.pressKey key >> settle

focus :: Element s -> IO ()
focus (Element element) = Page.focusElement element >> settle

blur :: Element s -> IO ()
blur (Element element) = Page.blurElement element >> settle

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
viaRunner :: Text -> Text -> Element s -> IO ()
viaRunner action argument (Element element) = Page.act action element argument >> settle

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

textContent :: Element s -> IO Text
textContent (Element element) = Page.textContentOf element

-- | A property as text, the way JavaScript's @String()@ renders it: a boolean
-- reads @true@ or @false@.
getProperty :: Element s -> Text -> IO Text
getProperty (Element element) = Page.propertyOf element

getAttribute :: Element s -> Text -> IO (Maybe Text)
getAttribute (Element element) = Page.attributeOf element

classes :: Element s -> IO [Text]
classes element = T.words <$> getProperty element "className"

outerHTML :: Element s -> IO Text
outerHTML (Element element) = Page.outerHTMLOf element

-- | Whether the element takes up space on the page: not hidden, not inside
-- something hidden, not @display: none@.
isVisible :: Element s -> IO Bool
isVisible (Element element) = Page.checkVisibility element

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

shouldHaveClass :: (HasCallStack) => Element s -> Text -> IO ()
shouldHaveClass element name = eventually $ do
  cs <- classes element
  unless (name `elem` cs) $ failText $ "expected class " <> show name <> ", found " <> show cs

shouldNotHaveClass :: (HasCallStack) => Element s -> Text -> IO ()
shouldNotHaveClass element name = eventually $ do
  cs <- classes element
  when (name `elem` cs) $ failText $ "expected no class " <> show name <> ", found " <> show cs

shouldHaveText :: (HasCallStack) => Element s -> Text -> IO ()
shouldHaveText element expected = eventually $ do
  actual <- textContent element
  unless (actual == expected) $ failText $ "expected text " <> show expected <> ", found " <> show actual

shouldBeVisible :: (HasCallStack) => Element s -> IO ()
shouldBeVisible element = eventually $ do
  visible <- isVisible element
  unless visible $ outerHTML element >>= \html -> failText (("expected to be visible:\n" <> html))

shouldBeHidden :: (HasCallStack) => Element s -> IO ()
shouldBeHidden element = eventually $ do
  visible <- isVisible element
  when visible $ outerHTML element >>= \html -> failText (("expected to be hidden:\n" <> html))

failText :: (HasCallStack) => Text -> IO ()
failText = expectationFailure . toS
