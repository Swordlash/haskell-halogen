{-# LANGUAGE MultiWayIf #-}

module Halogen.IO.Driver
  ( RenderSpec (..)
  , runUI
  , HalogenSocket (..)
  )
where

import Control.Exception (onException, throwIO, try)
import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Monad.UUID
import Data.Map.Strict qualified as M
import Data.NT
import Data.Row
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
import Unsafe.Coerce (unsafeCoerce)

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
  }

-- | Mount a component. The tree runs on a loop of its own (see
-- "Halogen.IO.Driver.Runtime"): the component's state, children, renders
-- and lifecycle are only ever touched there, one piece of work at a time,
-- and its programs wait only in 'Halogen.Query.HalogenM.Lift' ('liftIO').
runUI
  :: forall m r f i o
   . (MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)
  => RenderSpec m r
  -> Component f i o m
  -> i
  -> m (HalogenSocket f o m)
runUI spec@RenderSpec {dispose} c i = withRunInIO $ \runInIO -> do
  loop <- newLoop
  batch <- newIORef Nothing
  let tree = Tree {loop, runM = runInIO, batch, render = renderComponent tree spec}
  sio <- runInIO HS.create
  mounted <- newEmptyMVar
  initialized <- newEmptyMVar
  -- The tree's outputs reach its listeners at once, on the tree's loop, as
  -- in purescript-halogen: a listener must not wait for the tree itself.
  let emit o = runInIO (HS.notify sio.listener o)
  -- As in purescript-halogen, the tree is handed over once its components
  -- are initialized (an initializer may wait; the loop does not).
  enter loop $ do
    (root, initializers) <- withBatch tree (runComponent tree spec emit i c)
    putMVar mounted root
    whenAllOver initializers (putMVar initialized ())
  root <- takeMVar mounted
  takeMVar initialized
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
        unless wasDisposed $ void $ withBatch tree $ do
          ds <- readIORef var
          finalize tree (DriverStateX ds)
          for_ ds.rendering (treeRunM tree . dispose)
      takeMVar done >>= either throwIO pure

-- | Run the lifecycle work that a render transaction collects once the
-- outermost one is over: finalizers, then initializers (a parent's after
-- its children's). Also when it fails: what it did before failing stays
-- done. The initializers it started are handed back, to be waited for; a
-- nested transaction hands back none (the outermost one runs them).
withBatch :: Tree m r -> IO a -> IO (a, [Fiber])
withBatch tree act =
  readIORef tree.batch >>= \case
    Just _ -> (,[]) <$> act
    Nothing -> do
      b <- Batch <$> newIORef [] <*> newIORef []
      writeIORef tree.batch (Just b)
      result <- act `onException` carryOut b
      (result,) <$> carryOut b
  where
    carryOut b = do
      writeIORef tree.batch Nothing
      sequence_ . reverse =<< readIORef b.finalizers
      traverse (launchFree tree.loop) . reverse =<< readIORef b.initializers

-- | Run this (on the loop) once all these fibers are over.
whenAllOver :: [Fiber] -> IO () -> IO ()
whenAllOver fibers k = do
  left <- newIORef (length fibers)
  let ended = do
        n <- atomicModifyIORef' left (\x -> (x - 1, x - 1))
        when (n == 0) k
  when (null fibers) k
  for_ fibers $ \fb -> do
    waiting <- onFiberOver fb (const ended)
    unless waiting ended

-- | Create a component, render it, and have the render transaction
-- initialize it once its children are.
runComponent
  :: forall m r f' i' o'
   . (MonadIO m)
  => Tree m r
  -> RenderSpec m r
  -> (o' -> IO ())
  -> i'
  -> Component f' i' o' m
  -> IO (DriverStateRef m r f' o')
runComponent tree spec handler input (Component cs) = do
  initial <- treeRunM tree (cs.initialState input)
  ds <- initDriverState cs initial handler tree
  b <- currentBatch tree
  before <- atomicModifyIORef' b.initializers ([],)
  void $ renderComponent tree spec ds.selfRef
  children <- readIORef b.initializers
  let var = ds.selfRef
      initializer = do
        -- The children's initializers side by side, then this one's, then
        -- what it was sent before it was ready for it.
        sequentialTurn (traverse_ ParTurn (reverse children))
        spawnAwait var False (evalM var (runNT cs.eval (HQ.Initialize ())))
        sync $ handlePending ds.pendingQueries >> handlePending ds.pendingOuts
  writeIORef b.initializers (initializer : before)
  pure (DriverStateRef var)

currentBatch :: Tree m r -> IO Batch
currentBatch tree =
  readIORef tree.batch
    >>= maybe (throwIO (ErrorCall "Halogen internal error: no render transaction")) pure

-- | Render a component (if it is alive), in a render transaction: the
-- actions raised meanwhile start once it is over.
renderComponent :: (MonadIO m) => Tree m r -> RenderSpec m r -> IORef (DriverState m r s f act ps i o) -> IO [Fiber]
renderComponent tree spec var = do
  ds <- readIORef var
  life <- readIORef ds.life
  inPass <- readIORef ds.inPass
  if
    | life /= Alive -> pure []
    | inPass -> writeIORef ds.renderAgain True $> []
    | otherwise -> fmap snd $ withBatch tree $ do
        outermost <- isNothing <$> readIORef ds.pendingHandlers
        when outermost $ writeIORef ds.pendingHandlers (Just [])
        let passes = do
              writeIORef ds.renderAgain False
              renderPass tree spec var
              again <- readIORef ds.renderAgain
              when again passes
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
-- of their code has run: they go without a finalizer). It is not tried again by itself (the renderer may have changed
-- part of the DOM before failing); the next state change renders again.
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
        Input.RefUpdate (Input.RefLabel p) el -> modifyIORef' var $ \d -> d {refs = M.alter (const el) p d.refs}
        Input.Action act -> enter tree.loop (runHandler var (launchAction var act))
      childHandler :: act -> IO ()
      childHandler act = queueOrRun ds.pendingQueries (runHandler var (launchAction var act))
  rendering <-
    treeRunM tree
      ( specRender spec
          (liftIO . handler)
          (liftIO . slotChild tree spec childHandler childrenIn childrenOut created)
          (ds.component.render ds.state)
          ds.rendering
      )
      `onException` (sequence_ =<< readIORef created)
  children <- readIORef childrenOut
  gone <- readIORef childrenIn
  Slot.foreachSlot gone $ \(DriverStateRef child) -> do
    cds <- readIORef child
    for_ cds.rendering (treeRunM tree . specRemove spec)
    finalize tree (DriverStateX cds)
  modifyIORef' var $ \d -> d {rendering = Just rendering, children}

-- | A child slot of a render pass: the child already there (which gets its
-- new input), or a new one.
slotChild
  :: forall m r ps act
   . (MonadIO m)
  => Tree m r
  -> RenderSpec m r
  -> (act -> IO ())
  -> IORef (Slot.SlotStorage ps (DriverStateRef m r))
  -> IORef (Slot.SlotStorage ps (DriverStateRef m r))
  -> IORef [IO ()]
  -> ComponentSlotBox ps m act
  -> IO (RenderStateX r)
slotChild tree spec handler childrenIn childrenOut created ComponentSlotBox {..} = do
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
        new@(DriverStateRef child) <- runComponent tree spec (maybe pass handler . output) input component
        -- Taken back if the pass fails.
        modifyIORef' created (<> [readIORef child >>= discard tree spec . DriverStateX])
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
  traverse_ cancelFiber =<< readIORef ds.fibers
  traverse_ HS.unsubscribe =<< atomicModifyIORef' ds.subscriptions (mempty,)
  for_ ds.rendering (treeRunM tree . specRemove spec)
  Slot.foreachSlot ds.children $ \(DriverStateRef child) ->
    readIORef child >>= discard tree spec . DriverStateX

-- | A component goes away, with its children: it is closed to everything
-- but its finalizer (its programs are cancelled, its subscriptions ended),
-- and the render transaction runs the finalizer. Once that is over the
-- component is dead, and whatever of it still runs is cancelled.
finalize :: Tree m r -> DriverStateX m r f o -> IO ()
finalize tree (DriverStateX ds0) = fmap fst $ withBatch tree $ do
  let var = ds0.selfRef
  ds <- readIORef var
  life <- readIORef ds.life
  when (life == Alive) $ do
    writeIORef ds.life Closing
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

specChild :: RenderSpec m r -> r s act ps o -> r s act ps o
specChild RenderSpec {renderChild = child} = child
