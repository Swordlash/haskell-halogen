-- | Test Halogen components in a real browser, from hspec.
--
-- A test runs in 'PageM', a page of its own: it mounts components there,
-- finds their elements with CSS selectors, acts on them and checks what it
-- reads back. It names no monad for the components: they run in whatever @m@
-- the suite is started with, anything with a 'MonadBrowserTest' instance.
--
-- @
-- spec :: forall m -> (MonadBrowserTest m) => Spec
-- spec m = describe "counter" $
--   it "counts clicks" $ runPage $ do
--     ui <- mount m Counter.component ()
--     find ui "button" >>= click
--     find ui ".count" >>= (`shouldHaveText` "1")
--     query ui (H.mkRequest Counter.GetCount) `shouldReturn` Just 1
--
-- main :: IO ()
-- main = runBrowserTests (spec BrowserDOM)
-- @
--
-- 'runPage' is to 'PageM' what @runST@ is to @ST@: nothing typed with a
-- page's @s@ -- what was mounted on it, the elements found there -- can
-- outlive it, so no test can reach into another's. 'PageM' has no @MonadIO@,
-- so one page cannot be opened inside another; the expectations a test needs
-- are provided here in 'PageM', and 'unsafeIOToPageM' is there for the rest.
-- A page has one mouse, one keyboard and one focused element, so pages are
-- opened one at a time: tests marked @parallel@ still run in turn.
--
-- Import this module rather than "Test.Hspec": it re-exports what a spec
-- needs to describe tests, and its expectations share hspec's names.
--
-- The suite runs inside the page it tests: its @main@ is 'runBrowserTests', it
-- is built by the WebAssembly backend (as a reactor) or the JavaScript backend,
-- and the @hspec-halogen@ executable, as cabal's test wrapper, loads it into
-- headless Chromium. That is what lets a
-- test hold the component itself rather than only the DOM it renders. The
-- runner also backs 'click' and 'typeText' with Playwright, so input arrives
-- as trusted events, to an element Playwright has checked is visible, enabled
-- and not covered.
--
-- Natively the package builds, so that a suite type-checks and the language
-- server can load it, but 'runBrowserTests' only reports that it skipped the
-- suite.
--
-- Every action waits for the component to react before it returns (see
-- 'settle'), so a synchronous @handleAction@ has rendered by then. What
-- finishes later -- a forked effect, a timer, work a JavaScript widget defers
-- to the next frame -- is what 'eventually' is for, and 'find' and the
-- @should@ helpers here already retry.
module Test.Hspec.Halogen
  ( -- * Running a suite
    runBrowserTests

    -- * Describing tests
  , Spec
  , SpecWith
  , describe
  , context
  , it
  , specify
  , xit
  , parallel
  , sequential

    -- * Pages
  , PageM
  , runPage
  , unsafeIOToPageM

    -- * Mounting
  , MonadBrowserTest (..)
  , Mounted
  , mount
  , unmount
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
  , isAttached

    -- * Expecting
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  , shouldContain
  , shouldReturn
  , expectationFailure
  , shouldHaveClass
  , shouldNotHaveClass
  , shouldHaveText
  , shouldBeVisible
  , shouldBeHidden
  , shouldBeSameElement

    -- * Waiting
  , eventually
  , eventuallyWithin
  )
where

import Control.Monad.Fail qualified as Fail
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
import Test.Hspec (Spec, SpecWith, context, describe, it, parallel, sequential, specify, xit)
import Test.Hspec qualified as Hspec
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
-- Without a page -- natively, or a JavaScript-backend suite run as the Node
-- script it also is -- it says so and returns without running anything.
runBrowserTests :: Spec -> IO ()
runBrowserTests spec =
  Page.inBrowser >>= \case
    False -> putText "hspec-halogen: skipped, this suite runs in a browser (run it with hspec-halogen test)"
    True -> do
      hSetBuffering stdout LineBuffering
      Page.removeLeftovers
      args <- Page.runnerArgs
      -- The runner is told how it went even if the suite itself throws;
      -- otherwise it would wait for a report until it timed out.
      summary <-
        (maybe identity (withArgs . map toS) args $ hspecWithResult defaultConfig spec)
          `onException` (hFlush stdout >> Page.reportDone (-1))
      hFlush stdout
      Page.reportDone (summaryFailures summary)

--------------------------------------------------------------------------------
-- Pages
--------------------------------------------------------------------------------

-- | A test's work in a page of its own, as @ST s@ is work on a state thread
-- of its own.
newtype PageM s a = PageM (ReaderT PageEnv IO a)
  deriving newtype (Functor, Applicative, Monad)

-- | A failed pattern, such as @[a, b] <- findAll ui "li"@ finding three,
-- fails the test.
instance Fail.MonadFail (PageM s) where
  fail message = unsafeIOToPageM (assertFailure message)

-- | The page a test works in: a container at the end of the body for what it
-- mounts, and the teardown of everything mounted there.
data PageEnv = PageEnv
  { container :: DOM.Element
  , teardowns :: IORef [IO ()]
  }

-- | Run a test in a page of its own, and tear down everything mounted on it
-- when the test ends, pass or fail.
--
-- The @forall s@ keeps what the test finds and mounts inside it, as @runST@
-- keeps an @STRef@. Only one page is open at a time -- the browser has one
-- mouse, one keyboard and one focused element to share -- so a test waits
-- here for the one before it, whatever hspec was told about parallelism.
runPage :: (forall s. PageM s a) -> IO a
runPage (PageM test) = do
  me <- myThreadId
  holder <- readIORef pageHolder
  -- Only 'unsafeIOToPageM' can get here: the lock would wait on itself.
  when (holder == Just me) $
    panic "hspec-halogen: runPage inside runPage; a test works in one page"
  withMVar pageLock $ \() ->
    bracket open close $ \env -> do
      atomicWriteIORef pageHolder (Just me)
      runReaderT test env
  where
    open = PageEnv <$> Page.createContainer <*> newIORef []
    close env = do
      atomicWriteIORef pageHolder Nothing
      pending <- atomicModifyIORef' env.teardowns ([],)
      sequence_ pending
      Page.removeElement env.container

-- | Run any 'IO' in a page, as @unsafeIOToSTM@ runs it in a transaction. It
-- is unsafe the same way: nothing stops that 'IO' opening another page (which
-- fails) or keeping what should have stayed in this one.
unsafeIOToPageM :: IO a -> PageM s a
unsafeIOToPageM = PageM . lift

pageEnv :: PageM s PageEnv
pageEnv = PageM ask

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

-- | An element of the page a test works in.
newtype Element s = Element DOM.Element

-- | The same node, as JavaScript's @===@ has it: what a test compares to see
-- that a render patched an element rather than replacing it.
instance Eq (Element s) where
  Element a == Element b = Page.sameElement a b

-- | Mount a component into a container of its own on the page. It is
-- unmounted when the page is torn down.
--
-- The component has rendered and run its initialisers by the time this
-- returns. Its outputs are collected from then on, for 'outputs'.
--
-- The monad is the first argument, @mount BrowserDOM component input@, so a
-- spec polymorphic in it passes it on as @mount m@.
mount :: forall m -> (MonadBrowserTest m) => Component q i o m -> i -> PageM s (Mounted s q o m)
mount _ component input = do
  env <- pageEnv
  unsafeIOToPageM $ do
    element <- Page.createContainerIn env.container
    socket <- runTest $ mountInto component input element
    raised <- newIORef []
    subscription <- runTest $ HS.subscribe socket.messages $ \o -> liftIO $ modifyIORef' raised (<> [o])
    let teardown = runTest $ HS.unsubscribe subscription >> socket.dispose
    atomicModifyIORef' env.teardowns (\ts -> (teardown : ts, ()))
    pure Mounted {container = element, socket, raised}

-- | Unmount a component before the test ends, running its finalisers and
-- removing what it rendered, to check what happens once it is gone.
unmount :: (MonadBrowserTest m) => Mounted s q o m -> PageM s ()
unmount ui = unsafeIOToPageM $ do
  runTest ui.socket.dispose
  Page.removeElement ui.container

-- | Send the component a query, as a parent would.
query :: (MonadBrowserTest m) => Mounted s q o m -> q a -> PageM s (Maybe a)
query Mounted {socket = HalogenSocket {query = send}} q = unsafeIOToPageM $ runTest (send q)

-- | Everything the component has raised since it was mounted, oldest first.
outputs :: Mounted s q o m -> PageM s [o]
outputs ui = unsafeIOToPageM $ readIORef ui.raised

-- | The container the component renders into.
root :: Mounted s q o m -> Element s
root ui = Element ui.container

--------------------------------------------------------------------------------
-- Finding elements
--------------------------------------------------------------------------------

-- | The first element in the component matching a CSS selector, waiting for
-- one to appear. Fails with the component's markup if none does.
find :: (HasCallStack) => Mounted s q o m -> Text -> PageM s (Element s)
find ui = findIn (root ui)

-- | Every element in the component matching a CSS selector, as it is now.
findAll :: Mounted s q o m -> Text -> PageM s [Element s]
findAll ui = findAllIn (root ui)

-- | 'find' within one element rather than the whole component.
findIn :: (HasCallStack) => Element s -> Text -> PageM s (Element s)
findIn scope@(Element element) selector = eventually $ do
  unsafeIOToPageM (Page.querySelector element selector) >>= \case
    Just found -> pure (Element found)
    Nothing -> do
      html <- outerHTML scope
      unsafeIOToPageM $ assertFailure $ toS $ "No element matches " <> show selector <> " in\n" <> html

-- | 'findAll' within one element rather than the whole component.
findAllIn :: Element s -> Text -> PageM s [Element s]
findAllIn (Element element) selector = unsafeIOToPageM $ map Element <$> Page.querySelectorAll element selector

--------------------------------------------------------------------------------
-- Acting
--------------------------------------------------------------------------------

-- | Click an element as a user would: Playwright waits until it is visible,
-- stable, enabled and not covered, then clicks its centre.
click :: Element s -> PageM s ()
click = viaRunner "click" ""

-- | Type text into an element key by key, after what it already holds.
typeText :: Element s -> Text -> PageM s ()
typeText element text = viaRunner "type" text element

-- | Empty an input as a user selecting all and deleting would.
clear :: Element s -> PageM s ()
clear = viaRunner "clear" ""

-- | Press a key, or a chord such as @Shift+Tab@, on the focused element. Key
-- names are Playwright's: @Enter@, @Backspace@, @ArrowDown@, …
press :: Text -> PageM s ()
press key = unsafeIOToPageM (Page.pressKey key) >> settle

focus :: Element s -> PageM s ()
focus (Element element) = unsafeIOToPageM (Page.focusElement element) >> settle

blur :: Element s -> PageM s ()
blur (Element element) = unsafeIOToPageM (Page.blurElement element) >> settle

-- | Let the component finish reacting to what just happened in the page.
--
-- A JavaScript event reaches Haskell as a callback, which the wasm runtime
-- does not run on the spot (the JavaScript backend's does, and waiting costs
-- it only a task): it schedules its scheduler loop as a task of its
-- own (@scheduler.postTask@ in Chromium), and this thread would otherwise be
-- resumed before that task ran. Waiting for a background-priority task lets
-- every task already queued at the normal priority run first. Every action
-- here ends with it; call it directly after acting on the page some other way.
settle :: PageM s ()
settle = unsafeIOToPageM Page.settleTasks

-- The runner acts on a selector, so the element is tagged with one for the
-- length of the call.
viaRunner :: Text -> Text -> Element s -> PageM s ()
viaRunner action argument (Element element) = unsafeIOToPageM (Page.act action element argument) >> settle

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

textContent :: Element s -> PageM s Text
textContent (Element element) = unsafeIOToPageM $ Page.textContentOf element

-- | A property as text, the way JavaScript's @String()@ renders it: a boolean
-- reads @true@ or @false@.
getProperty :: Element s -> Text -> PageM s Text
getProperty (Element element) name = unsafeIOToPageM $ Page.propertyOf element name

getAttribute :: Element s -> Text -> PageM s (Maybe Text)
getAttribute (Element element) name = unsafeIOToPageM $ Page.attributeOf element name

classes :: Element s -> PageM s [Text]
classes element = T.words <$> getProperty element "className"

outerHTML :: Element s -> PageM s Text
outerHTML (Element element) = unsafeIOToPageM $ Page.outerHTMLOf element

-- | Whether the element takes up space on the page: not hidden, not inside
-- something hidden, not @display: none@.
isVisible :: Element s -> PageM s Bool
isVisible (Element element) = unsafeIOToPageM $ Page.checkVisibility element

-- | Whether the element is still in the page: a render that replaced it, or
-- removed it, has taken it out.
isAttached :: Element s -> PageM s Bool
isAttached (Element element) = unsafeIOToPageM $ Page.isConnected element

--------------------------------------------------------------------------------
-- Expecting
--------------------------------------------------------------------------------

-- hspec's expectations, in a page. They keep hspec's names, so a spec
-- imports this module rather than "Test.Hspec".

infix 1 `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldContain`, `shouldReturn`

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> PageM s ()
actual `shouldBe` expected = unsafeIOToPageM (actual `Hspec.shouldBe` expected)

shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> PageM s ()
actual `shouldNotBe` unexpected = unsafeIOToPageM (actual `Hspec.shouldNotBe` unexpected)

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> PageM s ()
actual `shouldSatisfy` predicate = unsafeIOToPageM (actual `Hspec.shouldSatisfy` predicate)

shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> PageM s ()
actual `shouldContain` sublist = unsafeIOToPageM (actual `Hspec.shouldContain` sublist)

shouldReturn :: (HasCallStack, Show a, Eq a) => PageM s a -> a -> PageM s ()
action `shouldReturn` expected = action >>= (`shouldBe` expected)

expectationFailure :: (HasCallStack) => Text -> PageM s a
expectationFailure message = unsafeIOToPageM (assertFailure (toS message))

-- | The element has the class, or will within two seconds.
shouldHaveClass :: (HasCallStack) => Element s -> Text -> PageM s ()
shouldHaveClass element name = eventually $ do
  cs <- classes element
  unless (name `elem` cs) $ expectationFailure $ "expected class " <> show name <> ", found " <> show cs

shouldNotHaveClass :: (HasCallStack) => Element s -> Text -> PageM s ()
shouldNotHaveClass element name = eventually $ do
  cs <- classes element
  when (name `elem` cs) $ expectationFailure $ "expected no class " <> show name <> ", found " <> show cs

shouldHaveText :: (HasCallStack) => Element s -> Text -> PageM s ()
shouldHaveText element expected = eventually $ do
  actual <- textContent element
  unless (actual == expected) $ expectationFailure $ "expected text " <> show expected <> ", found " <> show actual

shouldBeVisible :: (HasCallStack) => Element s -> PageM s ()
shouldBeVisible element = eventually $ do
  visible <- isVisible element
  unless visible $ outerHTML element >>= \html -> expectationFailure ("expected to be visible:\n" <> html)

shouldBeHidden :: (HasCallStack) => Element s -> PageM s ()
shouldBeHidden element = eventually $ do
  visible <- isVisible element
  when visible $ outerHTML element >>= \html -> expectationFailure ("expected to be hidden:\n" <> html)

-- | Both are the same node: a render kept the element, patching it where it
-- stood, rather than building a new one.
shouldBeSameElement :: (HasCallStack) => Element s -> Element s -> PageM s ()
shouldBeSameElement actual expected =
  unless (actual == expected) $ do
    now <- outerHTML actual
    before <- outerHTML expected
    attached <- isAttached expected
    expectationFailure $
      "expected the same element, found another:\n"
        <> now
        <> "\nwhere there was"
        <> (if attached then ":\n" else ", now out of the page:\n")
        <> before

--------------------------------------------------------------------------------
-- Waiting
--------------------------------------------------------------------------------

-- | Retry until it stops failing, for up to two seconds, then fail as it last
-- did. Any expectation can be wrapped in it.
eventually :: PageM s a -> PageM s a
eventually = eventuallyWithin 2000

-- | 'eventually' with a timeout in milliseconds.
eventuallyWithin :: Int -> PageM s a -> PageM s a
eventuallyWithin timeoutMs (PageM action) = PageM $ ReaderT $ \env -> go env (timeoutMs `div` pollMs)
  where
    pollMs = 20
    go env attempts =
      tryJust notAsync (runReaderT action env) >>= \case
        Right a -> pure a
        Left e
          | attempts <= 0 -> throwIO e
          | otherwise -> threadDelay (pollMs * 1000) >> go env (attempts - 1)
    notAsync e = case fromException e of
      Just (_ :: SomeAsyncException) -> Nothing
      Nothing -> Just e
