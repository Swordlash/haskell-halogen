{-# LANGUAGE CPP #-}

-- | Pages of the in-memory DOM. Each page has its own document, which is
-- also its window, and its own storage. Events in one page do not reach
-- listeners in another, so tests of components can run side by side.
module Test.Pages (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "pages of the in-memory DOM" $ pure ()

#else

import Control.Concurrent (forkIO, rtsSupportsBoundThreads, setNumCapabilities, threadDelay)
import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, modify)
import Data.Aeson (object)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import Data.Row (Empty, type (.==))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Void (Void, absurd)
import Halogen qualified as H
import Halogen.Activity qualified as Activity
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Portal qualified as Portal
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM, StorageKind (..), runMemDOM)
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (runUI)
import System.Mem (performMajorGC)
import System.Mem.Weak (deRefWeak)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Web.DOM.Internal.Types (Element (..), toNode)
import Web.Event.Event (EventType (..))
import Web.HTML.Common (AttrName (..))
import Web.Storage.IndexedDB qualified as IDB

--------------------------------------------------------------------------------
-- A bell: a press anywhere in the document closes its list
--------------------------------------------------------------------------------

data Bell = Bell {open :: Bool, heard :: Int}

data BellAction = Watch | Ring | Pressed

-- | Ask a bell whether its list is open and how many presses it heard.
data BellQuery a = Report ((Bool, Int) -> a)

-- | A bell. A @pointerdown@ anywhere in the document closes its list. It
-- subscribes to a listener on the document, as a page's bell does.
bell :: H.Component BellQuery () Void MemDOM
bell =
  H.component
    (\() -> pure Bell {open = False, heard = 0})
    render
    [ H.onInitialize (act Watch)
    , H.onAction act
    , H.onQuery $ \case
        Report reply -> do
          s <- get
          pure (Just (reply (s.open, s.heard)))
    ]
  where
    render :: Bell -> H.ComponentHTML BellAction Empty MemDOM
    render s =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "bell"), HE.onClick (const Ring)] [HH.text "bell"]
        , if s.open then HH.ul [HP.class_ (HH.ClassName "list")] [HH.li_ [HH.text "news"]] else HH.text ""
        ]
    act = \case
      Watch -> do
        presses <- H.sync $ do
          doc <- DOM.window >>= DOM.document
          HS.lowerEmitter (HQE.eventListener (EventType "pointerdown") (N.fromNative (N.toNative doc)) Just)
        void (H.subscribe (Pressed <$ presses))
      Ring -> modify $ \s -> s {open = True}
      Pressed -> modify $ \s -> s {open = False, heard = s.heard + 1}

-- | A mounted bell: its container, a way to ask it for its state, and a
-- way to end it.
data Mounted = Mounted
  { container :: N.NativeNode
  , report :: IO (Bool, Int)
  , dispose :: IO ()
  }

-- | Mount a bell in a page.
mountBell :: N.Page -> IO Mounted
mountBell page = do
  container <- N.newContainerIn page
  H.HalogenSocket {H.query = ask, H.dispose = end} <- N.runMemDOMIn page (runUI bell () (N.fromNative container))
  let report = N.runMemDOMIn page (ask (H.mkRequest Report)) >>= maybe (fail "the bell did not answer") pure
  pure Mounted {container, report, dispose = N.runMemDOMIn page end}

-- | Ring a bell. A click on its button opens its list.
ring :: Mounted -> IO ()
ring mounted =
  N.querySelector "button.bell" mounted.container >>= \case
    Just button -> N.dispatch "click" (object []) button
    Nothing -> fail "the bell has no button"

-- | Press on a page's @body@, outside every bell.
press :: N.Page -> IO ()
press page =
  N.querySelector "body" (N.pageDocument page) >>= \case
    Just body -> N.dispatch "pointerdown" (object []) body
    Nothing -> fail "the page has no body"

-- | Run twenty bells at once, each on its own thread. Every bell rings.
-- When all have rung, the even ones get a press in their page. The result
-- is each bell's state, in order. The function gives the page for each
-- bell: one page each, or one page for all.
twentyBells :: (Int -> N.Page) -> IO [(Bool, Int)]
twentyBells pageOf = do
  when rtsSupportsBoundThreads (setNumCapabilities 4)
  rung <- barrier 20
  pressed <- barrier 20
  inParallel
    [ do
        mounted <- mountBell (pageOf i)
        ring mounted
        rung
        when (even i) (press (pageOf i))
        pressed
        outcome <- mounted.report
        mounted.dispose
        pure outcome
    | i <- [0 .. 19]
    ]

-- | Run each action on its own thread, all at once, and wait for them all.
inParallel :: [IO a] -> IO [a]
inParallel actions = do
  results <- for actions $ \action -> do
    result <- newEmptyMVar
    void (forkIO (try action >>= putMVar result))
    pure result
  for results $ \result -> takeMVar result >>= either (throwIO @SomeException) pure

-- | A barrier for this many threads. Each thread waits until all arrive.
barrier :: Int -> IO (IO ())
barrier n = do
  arrived <- newMVar (0 :: Int)
  everyone <- newEmptyMVar
  pure $ do
    count <- modifyMVar arrived (\c -> pure (c + 1, c + 1))
    when (count == n) (putMVar everyone ())
    readMVar everyone

--------------------------------------------------------------------------------
-- Where a tree's later work runs
--------------------------------------------------------------------------------

-- | A component that notes which document it finds in its later work:
-- after a wait, in a forked fiber, after a wait in its monad, and in its
-- finalizer. It also renders a portal into the body of its document.
noting :: IORef [(Text, N.NativeNode)] -> H.Component H.VoidF () Void MemDOM
noting found =
  H.component
    (\() -> pure ())
    render
    [ H.onInitialize $ do
        H.async (liftIO (threadDelay 1000))
        note "after a wait"
        void (H.fork (H.async (liftIO (threadDelay 1000)) >> note "in a fork"))
        doc <- H.async (liftIO (threadDelay 1000) >> DOM.window >>= DOM.document)
        H.sync (liftIO (atomicModifyIORef' found (\before -> (("in its monad", N.toNative doc) : before, ()))))
    , H.onFinalize (note "in its finalizer")
    ]
  where
    -- Both steps are in one effect, which does not wait. So the
    -- finalizer's note is written before the tree's disposal returns.
    note label = H.sync $ do
      doc <- DOM.window >>= DOM.document
      liftIO (atomicModifyIORef' found (\before -> ((label, N.toNative doc) : before, ())))
    render :: () -> H.ComponentHTML () ("portal" .== H.Slot H.VoidF Void ()) MemDOM
    render () = HH.div_ [Portal.portal "portal" () ported () Nothing absurd]

-- | The content the portal puts in the body.
ported :: H.Component H.VoidF () Void MemDOM
ported =
  H.component
    (\() -> pure ())
    (\() -> HH.p [HP.class_ (HH.ClassName "ported")] [HH.text "ported"])
    []

--------------------------------------------------------------------------------

-- | Use a page and drop it. The result says whether the page's document
-- has been collected. The page holds its document, so a collected
-- document means a collected page.
usedAndDropped :: IO (IO Bool)
usedAndDropped = do
  page <- N.newPage
  mounted <- mountBell page
  ring mounted
  press page
  mounted.dispose
  N.runMemDOMIn page (DOM.writeStorageItem LocalStorage "kept" "here")
  weak <- mkWeakChildren (N.pageDocument page)
  pure (isNothing <$> deRefWeak weak)
  where
    mkWeakChildren node = mkWeakIORef node.kids (pure ())
{-# NOINLINE usedAndDropped #-}

spec :: Spec
spec = describe "pages of the in-memory DOM" $ do
  describe "a document listener" $ do
    it "hears only its own page's presses, with twenty bells side by side" $ do
      pages <- for [0 .. 19 :: Int] (const N.newPage)
      outcomes <- twentyBells (pages !!)
      outcomes `shouldBe` [if even i then (False, 1) else (True, 0) | i <- [0 .. 19 :: Int]]

    it "hears every press in a page the twenty bells share" $ do
      -- This is the test above with one page for all bells. Every bell
      -- hears every press. Separate pages prevent this.
      page <- N.newPage
      outcomes <- twentyBells (const page)
      outcomes `shouldBe` replicate 20 (False, 10)

  describe "a page" $ do
    it "has a window that hears nothing dispatched in another page, and listeners that run in it" $ do
      one <- N.newPage
      other <- N.newPage
      heard <- newIORef []
      -- The listener notes the document it finds. It must be the page it
      -- was made in, whoever dispatches the event.
      N.runMemDOMIn one $ do
        target <- DOM.window >>= DOM.windowToEventTarget
        listener <- DOM.mkEventListener $ \_ -> do
          doc <- DOM.window >>= DOM.document
          liftIO (atomicModifyIORef' heard (\before -> (N.toNative doc : before, ())))
        DOM.addEventListener (EventType "ping") listener target
      inOne <- N.newContainerIn one
      inOther <- N.newContainerIn other
      inAmbient <- N.newContainer
      N.dispatch "ping" (object []) inOther
      N.dispatch "ping" (object []) inAmbient
      (length <$> readIORef heard) `shouldReturn` 0
      N.dispatch "ping" (object []) inOne
      (map (== N.pageDocument one) <$> readIORef heard) `shouldReturn` [True]

    it "has a document that finds and contains its own nodes only" $ do
      one <- N.newPage
      other <- N.newPage
      inOther <- N.newContainerIn other
      N.runMemDOMIn other (DOM.setAttribute Nothing (AttrName "class") "mine" (N.fromNative inOther :: Element))
      N.isConnected inOther `shouldReturn` True
      found <- N.querySelector ".mine" (N.pageDocument other)
      isJust found `shouldBe` True
      (isNothing <$> N.querySelector ".mine" (N.pageDocument one)) `shouldReturn` True
      (isNothing <$> N.querySelector ".mine" N.ambientDocument) `shouldReturn` True
      let otherDoc = N.fromNative (N.pageDocument other) :: Element
          oneDoc = N.fromNative (N.pageDocument one) :: Element
          node = toNode (N.fromNative inOther :: Element)
      otherDoc.contains node `shouldReturn` True
      oneDoc.contains node `shouldReturn` False
      -- A program in a page gets that page's document.
      doc <- N.runMemDOMIn one (DOM.window >>= DOM.document)
      (N.toNative doc == N.pageDocument one) `shouldBe` True
      ambient <- runMemDOM (DOM.window >>= DOM.document)
      (N.toNative ambient == N.ambientDocument) `shouldBe` True

    it "has local and session storage that no other page reads" $ do
      one <- N.newPage
      other <- N.newPage
      for_ [LocalStorage, SessionStorage] $ \kind -> do
        N.runMemDOMIn one (DOM.writeStorageItem kind "pages-key" "one's")
        N.runMemDOMIn one (DOM.readStorageItem kind "pages-key") `shouldReturn` Just "one's"
        N.runMemDOMIn other (DOM.readStorageItem kind "pages-key") `shouldReturn` Nothing
        runMemDOM (DOM.readStorageItem kind "pages-key") `shouldReturn` Nothing
        N.runMemDOMIn other (DOM.storageItemKeys kind) `shouldReturn` []

    it "has an IndexedDB that no other page reads" $ do
      one <- N.newPage
      other <- N.newPage
      let db = IDB.Database {name = "pages-test", version = 1, stores = [IDB.StoreName "notes"]}
      N.runMemDOMIn one (IDB.put db (IDB.StoreName "notes") "k" "one's")
      N.runMemDOMIn one (IDB.get db (IDB.StoreName "notes") "k") `shouldReturn` Just "one's"
      N.runMemDOMIn other (IDB.get db (IDB.StoreName "notes") "k") `shouldReturn` Nothing
      runMemDOM (IDB.deleteDatabase db.name >> IDB.get db (IDB.StoreName "notes") "k") `shouldReturn` Nothing
      -- Deleting the ambient page's database does not touch this page's.
      N.runMemDOMIn one (IDB.get db (IDB.StoreName "notes") "k") `shouldReturn` Just "one's"

    it "is where a tree's later work runs: fibers, forks, its monad, finalizers and portals" $ do
      page <- N.newPage
      found <- newIORef []
      container <- N.newContainerIn page
      H.HalogenSocket {H.dispose = end} <- N.runMemDOMIn page (runUI (noting found) () (N.fromNative container))
      Activity.waitQuiet
      inPage <- N.querySelector "p.ported" (N.pageDocument page)
      isJust inPage `shouldBe` True
      (isNothing <$> N.querySelector "p.ported" N.ambientDocument) `shouldReturn` True
      N.runMemDOMIn page end
      notes <- reverse <$> readIORef found
      sort (map fst notes) `shouldBe` ["after a wait", "in a fork", "in its finalizer", "in its monad"]
      for_ notes $ \(label, doc) -> (label, doc == N.pageDocument page) `shouldBe` (label, True)

    it "is garbage once it is let go of" $ do
      gone <- usedAndDropped
      performMajorGC
      performMajorGC
      gone `shouldReturn` True

#endif
