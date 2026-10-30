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
-- Each browser context has one mouse, keyboard and focused element, so
-- examples and hooks run sequentially within it. With
-- @HSPEC_HALOGEN_JOBS=2@, the runner distributes examples marked 'parallel'
-- across isolated contexts, after unmarked and 'sequential' examples finish.
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
  , runBrowserTestsIn

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
  , htmlContainer
  , hydrate
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
  , dragTo
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
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', newIORef, readIORef)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Unique (Unique, newUnique)
import GHC.Clock (getMonotonicTime)
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..))
import Halogen.Subscription qualified as HS
import Halogen.Trace qualified as Trace
import Protolude hiding (find)
import System.Environment (withArgs)
import System.IO (BufferMode (..), hFlush, hSetBuffering)
import System.IO.Error (userError)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..), assertFailure)
import Test.Hspec (Spec, SpecWith, context, describe, it, parallel, sequential, specify, xit)
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen.Internal.Monad (MonadBrowserTest (..))
import Test.Hspec.Halogen.Internal.Page qualified as Page
import Test.Hspec.Halogen.Internal.Shard (Shard (..), onePage, shardSpec)
import Test.Hspec.Runner (Summary (..), defaultConfig, hspecWithResult)
import Text.Show (Show (showsPrec))
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
runBrowserTests = runSuite Nothing

-- | 'runBrowserTests', with every test's page in this element rather than at
-- the end of the body: for a page that has more on it than the suite, such
-- as the haskell-halogen tutorial's, which compiles a suite in the page and
-- runs it where the reader can watch it. The element is as the suite found
-- it once the suite is over, as a page takes down what it mounted.
--
-- It does not need the runner: without it, as in the tutorial, the page's
-- actions are synthetic events (see 'click'), and the report is only
-- printed, there being no runner to tell how it went.
runBrowserTestsIn :: DOM.HTMLElement -> Spec -> IO ()
runBrowserTestsIn = runSuite . Just . (coerce :: DOM.HTMLElement -> DOM.Element)

runSuite :: Maybe DOM.Element -> Spec -> IO ()
runSuite parent spec =
  Page.inBrowser >>= \case
    False -> putText "hspec-halogen: skipped, this suite runs in a browser (run it with hspec-halogen test)"
    True -> do
      hSetBuffering stdout LineBuffering
      Page.removeLeftovers
      args <- Page.runnerArgs
      shard <- Page.runnerShard
      selected <- case T.splitOn "/" shard of
        [""] -> pure spec
        ["serial"] -> pure (shardSpec Serial spec)
        [index, count]
          | Just i <- readMaybe index
          , Just n <- readMaybe count
          , n > 0
          , i >= 0
          , i < n ->
              pure (shardSpec (Parallel i n) spec)
        _ -> throwIO (userError "hspec-halogen: invalid browser shard")
      let run = maybe identity (withArgs . map toS) args $ hspecWithResult defaultConfig (onePage selected)
          -- The tests' pages go where this suite was told, while it runs.
          inParent = bracket_ (atomicWriteIORef pagesIn parent) (atomicWriteIORef pagesIn Nothing)
      -- The runner is told how it went even if the suite itself throws;
      -- otherwise it would wait for a report until it timed out.
      summary <- inParent run `onException` (hFlush stdout >> Page.reportDone (-1))
      hFlush stdout
      Page.reportDone (summaryFailures summary)

-- | Where the suite running now puts its pages: in this element, or, when
-- 'Nothing', at the end of the body.
pagesIn :: IORef (Maybe DOM.Element)
pagesIn = unsafePerformIO (newIORef Nothing)
{-# NOINLINE pagesIn #-}

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
-- when the test ends, pass or fail. The page is a container at the end of the
-- body, or of the element 'runBrowserTestsIn' was given.
--
-- While the test runs, the last 50 events of the components it mounted are
-- kept ("Halogen.Trace"), and an expectation that fails says what they were,
-- oldest first, after its own message: what the tree did up to the failure,
-- not only what was expected of it.
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
    withTimeline $ \timeline ->
      bracket open close $ \env -> do
        atomicWriteIORef pageHolder (Just me)
        runReaderT test env `catch` \failure -> throwIO =<< withEvents timeline failure
  where
    open = PageEnv <$> (maybe Page.createContainer Page.createRootIn =<< readIORef pagesIn) <*> newIORef []
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

-- | The events of a test's trees, as many as a failure shows, and when the
-- test started.
data Timeline = Timeline
  { started :: Double
  , events :: IORef (Seq Trace.Event)
  }

-- | How many events a failure shows.
timelineLength :: Int
timelineLength = 50

-- | Keep the last events of the trees while the body runs.
withTimeline :: (Timeline -> IO a) -> IO a
withTimeline body = do
  started <- (* 1000) <$> getMonotonicTime
  events <- newIORef Seq.empty
  let keep event = atomicModifyIORef' events $ \kept ->
        (Seq.drop (Seq.length kept + 1 - timelineLength) (kept Seq.|> event), ())
  bracket (Trace.subscribe keep) identity $ \_ -> body Timeline {started, events}

-- | The failure, with what the trees did before it.
withEvents :: Timeline -> HUnitFailure -> IO HUnitFailure
withEvents timeline (HUnitFailure location reason) = do
  kept <- toList <$> readIORef timeline.events
  pure $ HUnitFailure location $ case timelineText timeline.started kept of
    Nothing -> reason
    Just section -> case reason of
      Reason message -> Reason (message <> "\n\n" <> section)
      -- hspec prints the preface above what was expected and what came, so
      -- the events are there too, before the outcome they led to.
      ExpectedButGot preface expected actual ->
        ExpectedButGot (Just (maybe "" (<> "\n\n") preface <> section)) expected actual

-- | The section a failure gets, one line per event, oldest first, with its
-- time since the test started; 'Nothing' for no events.
--
-- > What the tree did (the last 50 events, oldest first):
-- >    12.3 ms  Counter#3  action
-- >    12.9 ms  Counter#3  state changed
-- >    13.0 ms  Counter#3  rendered in 0.4 ms
timelineText :: Double -> [Trace.Event] -> Maybe [Char]
timelineText _ [] = Nothing
timelineText started events =
  Just . toS . T.intercalate "\n" $
    ("What the tree did (the last " <> show timelineLength <> " events, oldest first):")
      : map line events
  where
    line event =
      T.justifyRight 7 ' ' (tenths (event.at - started))
        <> " ms  "
        <> event.tag.label
        <> "#"
        <> show event.tag.id
        <> "  "
        <> Trace.summary event.what
    tenths ms = let (whole, part) = (round (ms * 10) :: Integer) `quotRem` 10 in show whole <> "." <> show part

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
-- The component has rendered, and run its initialisers up to their first
-- wait, by the time this returns: what one loads when it starts may still be
-- on its way, so check it with an expectation that waits ('shouldHaveText',
-- 'eventually'). Its outputs are collected from then on, for 'outputs'.
--
-- The monad is the first argument, @mount BrowserDOM component input@, so a
-- spec polymorphic in it passes it on as @mount m@.
mount :: forall m -> (MonadBrowserTest m) => Component q i o m -> i -> PageM s (Mounted s q o m)
mount _ component input = do
  env <- pageEnv
  element <- unsafeIOToPageM (Page.createContainerIn env.container)
  mountWith mountInto element component input

-- | A container on the page holding this HTML, parsed as the browser parses
-- a page that arrives with it: what 'hydrate' mounts a component into, as
-- a page rendered by "Halogen.Static" holds the component's HTML before its
-- program has loaded. Find the elements in it ('findIn') before hydrating
-- to see which of them the component kept ('shouldBeSameElement').
--
-- It is torn down with the page.
htmlContainer :: Text -> PageM s (Element s)
htmlContainer html = do
  env <- pageEnv
  unsafeIOToPageM $ do
    element <- Page.createContainerIn env.container
    Page.setInnerHTML element html
    pure (Element element)

-- | 'mount', into a container that holds the component's HTML already
-- ('htmlContainer'): the component adopts it, with
-- 'Halogen.VDom.Driver.hydrateUI', rather than building its own.
hydrate :: forall m -> (MonadBrowserTest m) => Element s -> Component q i o m -> i -> PageM s (Mounted s q o m)
hydrate _ (Element element) = mountWith hydrateInto element

-- | Mount a component into this element with one of the class's ways, and
-- collect its outputs from then on.
mountWith
  :: (MonadBrowserTest m)
  => (Component q i o m -> i -> DOM.Element -> m (HalogenSocket q o m))
  -> DOM.Element
  -> Component q i o m
  -> i
  -> PageM s (Mounted s q o m)
mountWith into element component input = do
  env <- pageEnv
  unsafeIOToPageM $ do
    socket <- runTest $ into component input element
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
--
-- Without the runner (a page with no Playwright behind it, such as the
-- tutorial's, see 'runBrowserTestsIn') this and the other actions are done
-- by the page itself, with synthetic events: the pointer and mouse events of
-- a click and the focus it gives, a key's @keydown@, @keypress@, @input@ and
-- @keyup@, after waiting up to five seconds for the element to be in the
-- page, visible and enabled. The events are untrusted (@isTrusted@ is
-- false), whether something covers the element is not checked, and of what
-- the browser does by itself only the commonest is reproduced (see the
-- README).
click :: Element s -> PageM s ()
click = viaRunner "click" ""

-- | Drag the first element onto the second as a user would with the mouse:
-- press on its centre, move to the other's, release. Under the runner
-- Playwright does it, so the page sees pointer and mouse events along the
-- way, and Chromium's HTML5 @dragstart@, @dragover@ and @drop@.
dragTo :: Element s -> Element s -> PageM s ()
dragTo (Element source) (Element target) = unsafeIOToPageM (Page.drag source target) >> settle

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

-- | Whether the element is visible as Playwright counts it, the sense the
-- actions wait for: it has a box, and neither it nor what holds it is
-- @display: none@, @content-visibility: hidden@ or @visibility: hidden@.
-- Opacity is not looked at, nor whether something covers it.
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

-- | 'eventually' with a timeout in milliseconds. The timeout is a deadline
-- on the clock: an attempt still running when it passes is cut short,
-- however slow it is or whatever it waits for (a 'find' inside, a render
-- that never comes). It then fails as the last finished attempt did, or, if
-- none finished, says so. A timeout of zero or less makes one attempt.
eventuallyWithin :: Int -> PageM s a -> PageM s a
eventuallyWithin timeoutMs (PageM action)
  | timeoutMs <= 0 = PageM action
  | otherwise = PageM $ ReaderT $ \env -> do
      lastFailure <- newIORef Nothing
      let go =
            tryJust notAsync (runReaderT action env) >>= \case
              Right a -> pure a
              Left e -> atomicWriteIORef lastFailure (Just e) >> threadDelay (pollMs * 1000) >> go
      withinMs timeoutMs go >>= \case
        Just a -> pure a
        Nothing ->
          readIORef lastFailure
            >>= maybe (assertFailure ("no attempt finished within " <> show timeoutMs <> " ms")) throwIO
  where
    pollMs = 20
    notAsync e = case fromException e of
      Just (_ :: SomeAsyncException) -> Nothing
      Nothing -> Just e

-- | 'System.Timeout.timeout' in milliseconds, without its microsecond 'Int',
-- which overflows after 35 minutes where 'Int' has 32 bits (the WebAssembly
-- and JavaScript backends): the timer sleeps in steps short enough for any
-- 'Int'.
withinMs :: Int -> IO a -> IO (Maybe a)
withinMs ms act = do
  key <- newUnique
  me <- myThreadId
  let sleep left
        | left <= 0 = pure ()
        | otherwise = threadDelay (min left stepMs * 1000) >> sleep (left - min left stepMs)
      timer = sleep ms >> throwTo me (Expired key)
  handleJust (\(Expired k) -> guard (k == key)) (const (pure Nothing)) $
    bracket (forkIOWithUnmask (\unmask -> unmask timer)) killThread (const (Just <$> act))
  where
    stepMs = 1000000

-- | What 'withinMs' throws to cut its action short, told apart from another
-- 'withinMs' by its key.
newtype Expired = Expired Unique

instance Show Expired where
  showsPrec _ _ = ("Expired" ++)

instance Exception Expired where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
