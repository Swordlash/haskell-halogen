{-# LANGUAGE MultiWayIf #-}

-- | The driver, with what only this package uses besides 'runUI'
-- ('runUIInitialized', for "Halogen.Static"). Not exposed: the public face
-- is "Halogen.IO.Driver".
module Halogen.IO.Driver.Mount
  ( RenderSpec (..)
  , runUI
  , runUIInitialized
  , HalogenSocket (..)
  )
where

import Control.Exception (onException, throwIO, try)
import Data.Dynamic (Dynamic (..))
import Data.Map.Strict qualified as M
import Data.NT
import Data.Row
import GHC.Clock (getMonotonicTime)
import HPrelude hiding (get, onException, throwIO, try)
import Halogen.Component
import Halogen.Data.Slot qualified as Slot
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver.Eval
import Halogen.IO.Driver.Runtime
import Halogen.IO.Driver.State
import Halogen.Query.HalogenQ qualified as HQ
import Halogen.Query.Input
import Halogen.Query.Input qualified as Input
import Halogen.Subscription qualified as HS
import Halogen.Trace.Internal qualified as Trace
import Halogen.VDom.DOM.Prop (ElemRef (..))
import Type.Reflection (eqTypeRep, (:~~:) (..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)
import Web.DOM.Internal.Types (Node)

data HalogenSocket query output m = HalogenSocket
  { query :: forall a. query a -> m (Maybe a)
  -- ^ Waits for the answer: not to be called from a component's
  -- 'Halogen.Query.HalogenM.liftEffect' in the same tree, which would wait
  -- for itself.
  , messages :: HS.Emitter m output
  , dispose :: m ()
  }

data RenderSpec (m :: Type -> Type) (r :: Type -> Type -> Row Type -> Type -> Type) = RenderSpec
  { render
      :: forall s act ps o
       . (Input act -> m ())
      -> (ComponentSlotBox ps m act -> m (RenderStateX r))
      -> HC.HTML (ComponentSlot ps m act) act
      -> Maybe (r s act ps o)
      -> m (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> m ()
  , dispose :: forall s act ps o. r s act ps o -> m ()
  , sameElement :: Element -> Element -> m Bool
  -- ^ Whether two handles are the one element. Only the renderer knows
  -- what its elements are (on wasm two handles for one element need not be
  -- equal), and the driver asks when a @ref@'s element is removed: see
  -- 'Input.RefUpdate'. A renderer that makes no elements is never asked.
  , rootNode :: forall s act ps o. r s act ps o -> m (Maybe Node)
  -- ^ The DOM node that stands for a component's rendering on the page,
  -- for "Halogen.Trace"'s 'Halogen.Trace.rootNode' (a panel outlines it).
  -- 'Nothing' from a renderer whose output is not the DOM's.
  }

-- | Mount a component. The tree runs on a loop of its own (see
-- "Halogen.IO.Driver.Runtime"): the component's state, children, renders
-- and lifecycle are only ever touched there, one piece of work at a time,
-- and its programs wait only in 'Halogen.Query.HalogenM.Lift' ('liftIO').
--
-- The tree is handed over once it has rendered and its components have
-- run the synchronous part of their @Initialize@: what an @Initialize@
-- waits for in 'liftIO' goes on afterwards, and holds up only its own
-- component (see 'Halogen.Query.HalogenQ.Initialize').
--
-- All the driver asks of the component monad is a way into 'IO' and back:
-- the loop, the fibers and their workers are the runtime's own threads.
runUI
  :: forall m r f i o
   . (MonadUnliftIO m)
  => RenderSpec m r
  -> Component f i o m
  -> i
  -> m (HalogenSocket f o m)
runUI = mount False

-- | 'runUI', handed over only once every @Initialize@ the tree has started
-- has ended, what it waited for included, and the components those made
-- have ended theirs. Nothing in a running tree waits for that; a page
-- rendered ahead of its program does ("Halogen.Static"), so that it shows
-- what the components load when they start.
runUIInitialized
  :: forall m r f i o
   . (MonadUnliftIO m)
  => RenderSpec m r
  -> Component f i o m
  -> i
  -> m (HalogenSocket f o m)
runUIInitialized = mount True

mount
  :: forall m r f i o
   . (MonadUnliftIO m)
  => Bool
  -> RenderSpec m r
  -> Component f i o m
  -> i
  -> m (HalogenSocket f o m)
mount toTheEnd spec@RenderSpec {dispose} c i = withRunInIO $ \runInIO -> do
  loop <- newLoop
  batch <- newIORef Nothing
  initializing <- newIORef (0, [])
  let tree = Tree {loop, runM = runInIO, batch, render = renderComponent tree spec, initializing}
  sio <- runInIO HS.create
  mounted <- newEmptyMVar
  -- The tree's outputs reach its listeners at once, on the tree's loop, as
  -- in purescript-halogen: a listener must not wait for the tree itself.
  let emit o = runInIO (HS.notify sio.listener o)
  enter loop $ do
    -- The initializers have run their synchronous parts when this returns.
    root <- withBatch tree (runComponent tree spec Nothing emit i c)
    if toTheEnd
      then whenInitialized tree (putMVar mounted root)
      else putMVar mounted root
  root <- takeMVar mounted
  disposed <- newIORef False
  pure
    $ HalogenSocket
      { query = liftIO . askRoot tree root
      , messages = sio.emitter
      , dispose = liftIO (disposeRoot tree disposed root)
      }
  where
    askRoot :: forall a. Tree m r -> DriverStateRef m r f o -> f a -> IO (Maybe a)
    askRoot tree (DriverStateRef var) q = do
      answer <- newEmptyMVar
      enter tree.loop $ do
        ds <- readIORef var
        life <- readIORef ds.life
        if life /= Alive
          then putMVar answer (Done Nothing)
          else launchOwned var False (const pass) (evalQ var q) (putMVar answer)
      takeMVar answer >>= \case
        Done a -> pure a
        Failed e -> throwIO e
        Cancelled -> pure Nothing

    disposeRoot :: Tree m r -> IORef Bool -> DriverStateRef m r f o -> IO ()
    disposeRoot tree disposed (DriverStateRef var) = do
      done <- newEmptyMVar
      -- Whichever thread runs this, a failure goes back to the caller.
      enter tree.loop $ putMVar done <=< (try :: IO () -> IO (Either SomeException ())) $ do
        wasDisposed <- atomicModifyIORef' disposed (True,)
        unless wasDisposed $ withBatch tree $ do
          ds <- readIORef var
          finalize tree (DriverStateX ds)
          for_ ds.rendering (treeRunM tree . dispose)
      takeMVar done >>= either throwIO pure

-- | Run the lifecycle work that a render transaction collects once the
-- outermost one is over: finalizers, then initializers (a parent's after
-- its children's). Also when it fails: what it did before failing stays
-- done. Neither is waited for past its synchronous part, so when this
-- returns every @Initialize@ the transaction started has reached its first
-- wait or ended; a nested transaction leaves its own to the outermost one.
withBatch :: Tree m r -> IO a -> IO a
withBatch tree act =
  readIORef tree.batch >>= \case
    Just _ -> act
    Nothing -> do
      b <- Batch <$> newIORef [] <*> newIORef []
      writeIORef tree.batch (Just b)
      result <- act `onException` carryOut b
      result <$ carryOut b
  where
    carryOut b = do
      writeIORef tree.batch Nothing
      sequence_ . reverse =<< readIORef b.finalizers
      traverse_ (launchFree tree.loop . sync) . reverse =<< readIORef b.initializers

-- | Run this (on the loop) once none of the tree's @Initialize@ programs is
-- left running: now, if none is.
whenInitialized :: Tree m r -> IO () -> IO ()
whenInitialized tree k = do
  now <- atomicModifyIORef' tree.initializing $ \(n, ks) ->
    if n == 0 then ((n, ks), True) else ((n, k : ks), False)
  when now k

-- | Create a component, render it, and have the render transaction
-- initialize it after its children.
runComponent
  :: forall m r f' i' o'
   . (MonadIO m)
  => Tree m r
  -> RenderSpec m r
  -> Maybe Int
  -> (o' -> IO ())
  -> i'
  -> Component f' i' o' m
  -> IO (DriverStateRef m r f' o')
runComponent tree spec parent handler input (Component info cs) = do
  initial <- treeRunM tree (cs.initialState input)
  ds <- initDriverState info cs initial handler tree parent
  Trace.emit ds.tag Trace.Mounted
  Trace.register
    ds.tag.id
    Trace.Entry
      { restore = restoreWith tree spec ds.selfRef
      , node = readIORef ds.selfRef >>= \d -> join <$> traverse (treeRunM tree . specRootNode spec) d.rendering
      }
  b <- currentBatch tree
  before <- atomicModifyIORef' b.initializers ([],)
  renderComponent tree spec ds.selfRef
  children <- readIORef b.initializers
  let var = ds.selfRef
      initializer = do
        -- None for a component a failed render pass took back.
        life <- readIORef ds.life
        when (life == Alive) $ do
          -- The children's initializers, then this one's @Initialize@, then
          -- what the component was sent before it was ready for it. Each
          -- @Initialize@ is a fiber of its component, which runs until it
          -- first waits before 'launchOwned' returns, and no more is waited
          -- for: unlike purescript-halogen, where a component's initializer
          -- waits for its children's to end, one that waits (for what it
          -- loads, say) holds up nothing but the rest of itself. Its
          -- ancestors, the program whose render made it, and the queries and
          -- outputs that pass through them would otherwise all wait for the
          -- slowest @Initialize@ below them.
          sequence_ (reverse children)
          atomicModifyIORef' tree.initializing (\(n, ks) -> ((n + 1, ks), ()))
          launchOwned var False (const pass) (evalM var (runNT cs.eval (HQ.Initialize ()))) $ \outcome -> do
            report "A lifecycle handler" outcome
            ks <- atomicModifyIORef' tree.initializing $ \(n, ks) ->
              if n == 1 then ((0, []), ks) else ((n - 1, ks), [])
            sequence_ (reverse ks)
          Trace.emit ds.tag Trace.Initialized
          handlePending ds.pendingQueries >> handlePending ds.pendingOuts
  writeIORef b.initializers (initializer : before)
  pure (DriverStateRef var)

currentBatch :: Tree m r -> IO Batch
currentBatch tree =
  readIORef tree.batch
    >>= maybe (throwIO (ErrorCall "Halogen internal error: no render transaction")) pure

-- | Render a component (if it is alive), in a render transaction: the
-- actions raised meanwhile start once it is over.
renderComponent :: (MonadIO m) => Tree m r -> RenderSpec m r -> IORef (DriverState m r s f act ps i o) -> IO ()
renderComponent tree spec var = do
  ds <- readIORef var
  life <- readIORef ds.life
  inPass <- readIORef ds.inPass
  if
    | life /= Alive -> pure ()
    | inPass -> writeIORef ds.renderAgain True
    | otherwise -> withBatch tree $ do
        outermost <- isNothing <$> readIORef ds.pendingHandlers
        when outermost $ writeIORef ds.pendingHandlers (Just [])
        let passes = do
              writeIORef ds.renderAgain False
              timed (renderPass tree spec var)
              again <- readIORef ds.renderAgain
              when again passes
            -- The clock is read only for a listener.
            timed pass' =
              Trace.listening >>= \case
                False -> pass'
                True -> do
                  start <- getMonotonicTime
                  pass'
                  end <- getMonotonicTime
                  Trace.emit ds.tag (Trace.Rendered ((end - start) * 1000))
            handlers = do
              pending <- readIORef ds.pendingHandlers
              writeIORef ds.pendingHandlers (Just [])
              case pending of
                Just hs@(_ : _) -> sequence_ (reverse hs) >> handlers
                _ -> writeIORef ds.pendingHandlers Nothing
        writeIORef ds.inPass True
        (passes >> writeIORef ds.inPass False)
          `onException` (writeIORef ds.inPass False >> when outermost handlers)
        when outermost handlers

-- | One walk of the component's HTML against what it rendered last.
--
-- A walk that fails commits nothing: the component keeps the children and
-- rendering it had, and the children this walk created are removed (none
-- of their code has run: they go without a finalizer, and their
-- initializers do nothing). That holds up to the commit, taking the
-- children the walk dropped off the page included. It is not tried again
-- by itself; the next state change renders again (and the renderer, which
-- may have changed part of the page before failing, has to cope: see
-- "Halogen.VDom.Driver").
renderPass :: forall m r s f act ps i o. (MonadIO m) => Tree m r -> RenderSpec m r -> IORef (DriverState m r s f act ps i o) -> IO ()
renderPass tree spec var = do
  ds <- readIORef var
  childrenIn <- newIORef ds.children
  childrenOut <- newIORef Slot.empty
  created <- newIORef []
  let handler :: Input act -> IO ()
      handler = \case
        -- A ref is recorded as soon as its element is created or removed:
        -- initializers, which run after the render, may look it up.
        Input.RefUpdate (Input.RefLabel p) (Created el) -> modifyIORef' var $ \d -> d {refs = M.insert p el d.refs}
        -- A removal clears the label only if it still holds the element
        -- that goes. When a keyed element is replaced by one under a new
        -- key with the same label, the reconciler builds the children under
        -- new keys before it halts those whose keys went
        -- ('Halogen.VDom.Utils.diffWithKeyAndIxE'), so the removal comes
        -- second and would otherwise take away the element just recorded.
        Input.RefUpdate (Input.RefLabel p) (Removed el) -> do
          held <- M.lookup p . (.refs) <$> readIORef var
          for_ held $ \current -> do
            same <- treeRunM tree (specSameElement spec current el)
            when same $ modifyIORef' var $ \d -> d {refs = M.delete p d.refs}
        Input.Action act -> enterCallback tree.loop (runHandler var (launchAction var act))
      childHandler :: act -> IO ()
      childHandler act = queueOrRun ds.pendingQueries (runHandler var (launchAction var act))
  (rendering, children, gone) <-
    ( do
        rendering <-
          treeRunM tree
            $ specRender
              spec
              (liftIO . handler)
              (liftIO . slotChild tree spec ds.tag.id childHandler childrenIn childrenOut created)
              (ds.component.render ds.state)
              ds.rendering
        children <- readIORef childrenOut
        gone <- readIORef childrenIn
        Slot.foreachSlot gone $ \(DriverStateRef child) -> do
          cds <- readIORef child
          for_ cds.rendering (treeRunM tree . specRemove spec)
        pure (rendering, children, gone)
    )
      `onException` (sequence_ . reverse =<< readIORef created)
  modifyIORef' var $ \d -> d {rendering = Just rendering, children}
  Slot.foreachSlot gone $ \(DriverStateRef child) ->
    readIORef child >>= finalize tree . DriverStateX

-- | A child slot of a render pass: the child already there (which gets its
-- new input), or a new one.
slotChild
  :: forall m r ps act
   . (MonadIO m)
  => Tree m r
  -> RenderSpec m r
  -> Int
  -> (act -> IO ())
  -> IORef (Slot.SlotStorage ps (DriverStateRef m r))
  -> IORef (Slot.SlotStorage ps (DriverStateRef m r))
  -> IORef [IO ()]
  -> ComponentSlotBox ps m act
  -> IO (RenderStateX r)
slotChild tree spec parent handler childrenIn childrenOut created ComponentSlotBox {..} = do
  ref <-
    pop <$> readIORef childrenIn >>= \case
      Just (existing@(DriverStateRef child), rest) -> do
        writeIORef childrenIn rest
        cds <- readIORef child
        writeIORef cds.handlerRef (maybe pass handler . output)
        -- A fiber of the child's: it runs until it suspends, and the render
        -- goes on then. (Awaited here, a Receive that waits for its parent
        -- would wait for this render, which would wait for it.)
        launchOwned child False (const pass) (evalM child (runNT (unsafeCoerce cds.component.eval) (HQ.Receive input ()))) (report "A child's Receive")
        pure existing
      Nothing -> do
        new@(DriverStateRef child) <- runComponent tree spec (Just parent) (maybe pass handler . output) input component
        -- Taken back if the pass fails.
        modifyIORef' created ((readIORef child >>= discard tree spec . DriverStateX) :)
        pure new
  isDuplicate <- isJust . get <$> readIORef childrenOut
  when isDuplicate
    $ hPutStrLn stderr ("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur" :: Text)
  modifyIORef' childrenOut (set ref)
  readRef ref >>= \(DriverStateX cds) -> case cds.rendering of
    Nothing -> throwIO (ErrorCall "Halogen internal error: child was not initialized in renderChild")
    Just r -> pure (RenderStateX (specChild spec r))

-- | A component made by a render pass that failed: no program of it has run
-- (its initializer waits for the render transaction), so it goes without
-- one, its children with it.
discard :: Tree m r -> RenderSpec m r -> DriverStateX m r f o -> IO ()
discard tree spec (DriverStateX ds0) = do
  ds <- readIORef ds0.selfRef
  writeIORef ds.life Dead
  Trace.deregister ds.tag.id
  Trace.emit ds.tag Trace.Finalized
  traverse_ cancelFiber =<< readIORef ds.fibers
  traverse_ HS.unsubscribe =<< atomicModifyIORef' ds.subscriptions (mempty,)
  Slot.foreachSlot ds.children $ \(DriverStateRef child) ->
    readIORef child >>= discard tree spec . DriverStateX
  -- Taking it off the page may fail too (it does in the pass it is taken
  -- back from, maybe): that is reported, and the rest is taken back still.
  for_ ds.rendering $ \rendering ->
    (try (treeRunM tree (specRemove spec rendering)) :: IO (Either SomeException ())) >>= \case
      Left e -> hPutStrLn stderr ("Halogen: removing a component a failed render made failed: " <> show e :: Text)
      Right () -> pass

-- | A component goes away, with its children: it is closed to everything
-- but its finalizer (its programs are cancelled, its subscriptions ended),
-- and the render transaction runs the finalizer. Once that is over the
-- component is dead, and whatever of it still runs is cancelled.
finalize :: Tree m r -> DriverStateX m r f o -> IO ()
finalize tree (DriverStateX ds0) = withBatch tree $ do
  let var = ds0.selfRef
  ds <- readIORef var
  life <- readIORef ds.life
  when (life == Alive) $ do
    writeIORef ds.life Closing
    Trace.deregister ds.tag.id
    Trace.emit ds.tag Trace.Finalized
    traverse_ cancelFiber =<< readIORef ds.fibers
    writeIORef ds.forks mempty
    traverse_ HS.unsubscribe =<< atomicModifyIORef' ds.subscriptions (mempty,)
    b <- currentBatch tree
    let finalizer =
          launchOwned var True (const pass) (evalM var (runNT ds.component.eval (HQ.Finalize ()))) $ \outcome -> do
            report "A finalizer" outcome
            writeIORef ds.life Dead
            traverse_ cancelFiber =<< readIORef ds.fibers
    modifyIORef' b.finalizers (finalizer :)
    Slot.foreachSlot ds.children $ \(DriverStateRef child) ->
      readIORef child >>= finalize tree . DriverStateX

-- | 'Halogen.Trace.restoreState' for one component: through its tree's
-- loop, as any of the tree's work, the state is put in and rendered, as a
-- program's 'Control.Monad.State.put' would be. A component that has gone,
-- or whose state's type is not this or is not known, is left alone.
restoreWith :: (MonadIO m) => Tree m r -> RenderSpec m r -> IORef (DriverState m r s f act ps i o) -> Dynamic -> IO Bool
restoreWith tree spec var (Dynamic given value) = do
  ds0 <- readIORef var
  case ds0.info.stateType of
    Just stateType
      | Just HRefl <- eqTypeRep given stateType -> do
          done <- newEmptyMVar
          -- Whichever thread runs this, a failure goes back to the caller.
          enter tree.loop $ putMVar done <=< (try :: IO Bool -> IO (Either SomeException Bool)) $ do
            ds <- readIORef var
            life <- readIORef ds.life
            if life /= Alive
              then pure False
              else do
                writeIORef var ds {state = value}
                Trace.emit ds.tag (Trace.StateChanged (Just (Dynamic stateType value)))
                renderComponent tree spec var
                pure True
          takeMVar done >>= either throwIO pure
    _ -> pure False

readRef :: DriverStateRef m r f o -> IO (DriverStateX m r f o)
readRef (DriverStateRef var) = DriverStateX <$> readIORef var

-- The fields are polymorphic, so not reached with a dot.
specRender
  :: RenderSpec m r
  -> (Input act -> m ())
  -> (ComponentSlotBox ps m act -> m (RenderStateX r))
  -> HC.HTML (ComponentSlot ps m act) act
  -> Maybe (r s act ps o)
  -> m (r s act ps o)
specRender RenderSpec {render} = render

specRemove :: RenderSpec m r -> r s act ps o -> m ()
specRemove RenderSpec {removeChild} = removeChild

specSameElement :: RenderSpec m r -> Element -> Element -> m Bool
specSameElement RenderSpec {sameElement} = sameElement

specChild :: RenderSpec m r -> r s act ps o -> r s act ps o
specChild RenderSpec {renderChild = child} = child

specRootNode :: RenderSpec m r -> r s act ps o -> m (Maybe Node)
specRootNode RenderSpec {rootNode} = rootNode
