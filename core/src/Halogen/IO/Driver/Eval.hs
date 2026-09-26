-- | A component's programs, run as fibers on its tree's loop (see
-- "Halogen.IO.Driver.Runtime").
module Halogen.IO.Driver.Eval
  ( evalM
  , evalQ
  , launchOwned
  , launchAction
  , launchFree
  , spawnAwait
  , awaitAll
  , queueOrRun
  , runHandler
  , handlePending
  , report
  , ComponentGone (..)
  )
where

import Control.Applicative.Free.Fast (runAp)
import Control.Exception (throwIO)
import Control.Monad.Free.Church (foldF)
import Data.Foreign
import Data.Functor.Coyoneda
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as M
import Data.NT
import HPrelude hiding (Concurrently, finally, join, runConcurrently, state, throwIO)
import Halogen.Component
import Halogen.IO.Driver.Runtime
import Halogen.IO.Driver.State
import Halogen.Query.ChildQuery qualified as CQ
import Halogen.Query.HalogenM hiding (fork, join, kill, query, unsubscribe)
import Halogen.Query.HalogenQ qualified as HQ
import Halogen.Query.Input qualified as Input
import Halogen.Subscription qualified as HS

-- | Thrown by the runner 'withRunInIO' hands out when the component went
-- away before the program it was given could finish.
data ComponentGone = ComponentGone
  deriving stock (Show)

instance Exception ComponentGone

-- | A program of the component, as a fiber's steps. Everything but 'Lift'
-- (and 'Unlift', whose body is arbitrary 'IO') is synchronous work on the
-- loop.
evalM
  :: forall m r s f act ps i o a
   . IORef (DriverState m r s f act ps i o)
  -> HalogenM s act ps o m a
  -> Turn a
evalM var (HalogenM hm) = foldF go hm
  where
    go :: forall x. HalogenF s act ps o m x -> Turn x
    go = \case
      State f -> do
        (a, initializers) <- sync (update f)
        -- As in purescript-halogen, the program goes on once the children
        -- the render made are initialized (they may wait, and it with them).
        awaitAll initializers
        pure a
      Subscribe fes k -> sync $ do
        ds <- readIORef var
        sid <- fresh SubscriptionId ds
        life <- readIORef ds.life
        -- None while the component is being finalized.
        when (life == Alive) $ do
          sub <- HS.subscribe (fes sid) $ \act -> enter ds.tree.loop (launchAction var act)
          modifyIORef' ds.subscriptions (M.insert sid sub)
        pure (k sid)
      Unsubscribe sid next -> sync $ do
        ds <- readIORef var
        sub <- atomicModifyIORef' ds.subscriptions (\subs -> (M.delete sid subs, M.lookup sid subs))
        traverse_ HS.unsubscribe sub
        pure next
      Lift aff -> do
        ds <- sync (readIORef var)
        await (treeRunM ds.tree aff)
      LiftEffect eff -> do
        ds <- sync (readIORef var)
        sync (withinEffect ds.tree.loop (treeRunM ds.tree eff))
      Unlift q -> do
        ds <- sync (readIORef var)
        -- The body is arbitrary IO, run on a worker; each program it runs
        -- is a fiber of this component, handed to the loop, and waited for.
        await $ q $ UnliftIO $ \program -> do
          result <- newEmptyMVar
          post ds.tree.loop $ launchOwned var False (const pass) (evalM var program) (putMVar result)
          takeMVar result >>= \case
            Done a -> pure a
            Failed e -> throwIO e
            Cancelled -> throwIO ComponentGone
      ChildQuery (CQ.ChildQuery unpack query reply) -> do
        ds <- sync (readIORef var)
        reply <$> sequentialTurn (unpack (\(DriverStateRef child) -> ParTurn (queryChild child query)) ds.children)
      Raise o a -> sync $ do
        ds <- readIORef var
        handler <- readIORef ds.handlerRef
        queueOrRun ds.pendingOuts (handler o)
        pure a
      Par (HalogenAp p) -> sequentialTurn $ runAp (ParTurn . evalM var) p
      Fork hmu k -> do
        me <- currentFiber
        sync $ do
          ds <- readIORef var
          fid <- fresh ForkId ds
          -- Registered before it starts: it may end, and strike itself off,
          -- before 'launchOwned' returns.
          launchOwned
            var
            (fiberClosingOk me)
            (\fb -> modifyIORef' ds.forks (M.insert fid fb))
            (evalM var hmu)
            (\outcome -> modifyIORef' ds.forks (M.delete fid) >> report "A fork" outcome)
          pure (k fid)
      Join fid a -> do
        ds <- sync (readIORef var)
        running <- sync (M.lookup fid <$> readIORef ds.forks)
        for_ running $ \fb -> do
          outcome <- suspend $ \resume -> do
            waiting <- onFiberOver fb (resume . Right)
            unless waiting $ resume (Right (Done ()))
          case outcome of
            Done () -> pure ()
            Failed e -> sync (throwIO e)
            -- Joining a killed fork ends the program that joins.
            Cancelled -> Turn $ \self _ _ -> cancelFiber self
        pure a
      Kill fid a -> sync $ do
        ds <- readIORef var
        traverse_ cancelFiber . M.lookup fid =<< readIORef ds.forks
        pure a
      Throw e -> throwTurn e
      Catch body handler k -> k <$> catchTurn (evalM var body) (evalM var . handler)
      GetRef (Input.RefLabel p) k -> sync $ do
        DriverState {refs} <- readIORef var
        pure $ k $ M.lookup p refs


    update :: forall x. (s -> (x, s)) -> IO (x, [Fiber])
    update f = do
      -- The field is taken by a pattern, not `ds.state`: a selector thunk
      -- is a new pointer, and `unsafeRefEq` would then never see an
      -- unchanged state and render after every no-op update.
      ds@DriverState {state} <- readIORef var
      case f state of
        (a, state')
          | unsafeRefEq state state' -> pure (a, [])
          | otherwise -> do
              writeIORef var ds {state = state'}
              -- Rendered before the program goes on, so that what it reads
              -- next (a ref, say) is the new state's.
              initializers <- treeRender ds.tree var
              pure (a, initializers)

-- | Wait for fibers to end, however they end.
awaitAll :: [Fiber] -> Turn ()
awaitAll = traverse_ $ \fb -> suspend $ \resume -> do
  waiting <- onFiberOver fb (const (resume (Right ())))
  unless waiting $ resume (Right ())

-- | A query answered by a component: 'Nothing' if it has gone away, or goes
-- away before answering. Its own fiber, owned by that component, which the
-- asking program waits for.
queryChild :: IORef (DriverState m r s f act ps i o) -> f b -> Turn (Maybe b)
queryChild var q = suspend $ \resume -> do
  ds <- readIORef var
  life <- readIORef ds.life
  if life /= Alive
    then resume (Right Nothing)
    else launchOwned var False (const pass) (evalQ var q) $ \case
      Done b -> resume (Right b)
      Failed e -> resume (Left e)
      Cancelled -> resume (Right Nothing)

evalQ :: IORef (DriverState m r s f act ps i o) -> f a -> Turn (Maybe a)
evalQ var q = do
  ds <- sync (readIORef var)
  evalM var (runNT ds.component.eval (HQ.Query (Just <$> liftCoyoneda q) (const Nothing)))

-- | Start a program as a fiber of the component: it runs until it first
-- suspends before this returns. It runs only while the component is alive
-- (or, if it may, while it is being finalized). @register@ sees the fiber
-- before it starts.
launchOwned
  :: IORef (DriverState m r s f act ps i o)
  -> Bool
  -> (Fiber -> IO ())
  -> Turn a
  -> (Outcome a -> IO ())
  -> IO ()
launchOwned var closingOk register program outcome = do
  ds <- readIORef var
  n <- fresh identity ds
  let permitted =
        readIORef ds.life <&> \case
          Alive -> True
          Closing -> closingOk
          Dead -> False
  (fb, start) <- fiberFor ds.tree.loop permitted closingOk $ \o -> do
    modifyIORef' ds.fibers (IntMap.delete n)
    outcome o
  modifyIORef' ds.fibers (IntMap.insert n fb)
  register fb
  start program

-- | An action of the component, as a fiber of its own.
launchAction :: IORef (DriverState m r s f act ps i o) -> act -> IO ()
launchAction var act = do
  ds <- readIORef var
  launchOwned var False (const pass) (evalM var (runNT ds.component.eval (HQ.Action act ()))) (report "An action")

-- | A program no component owns (the lifecycle work of a render
-- transaction, which waits on programs that are owned).
launchFree :: Loop -> Turn () -> IO Fiber
launchFree loop program = do
  (fb, start) <- fiberFor loop (pure True) True (report "Lifecycle work")
  start program
  pure fb

-- | Start a program owned by a component and wait for it to end, however it
-- ends.
spawnAwait :: IORef (DriverState m r s f act ps i o) -> Bool -> Turn () -> Turn ()
spawnAwait var closingOk program = suspend $ \resume ->
  launchOwned var closingOk (const pass) program $ \outcome -> do
    report "A lifecycle handler" outcome
    resume (Right ())

-- | Run now, or keep for later if the queue is open.
queueOrRun :: IORef (Maybe [IO ()]) -> IO () -> IO ()
queueOrRun ref io =
  readIORef ref >>= \case
    Nothing -> io
    Just p -> writeIORef ref (Just (io : p))

-- | Start an action, or keep it until the component's render transaction in
-- progress is over.
runHandler :: IORef (DriverState m r s f act ps i o) -> IO () -> IO ()
runHandler var io = do
  ds <- readIORef var
  queueOrRun ds.pendingHandlers io

-- | Run what was kept, and close the queue.
handlePending :: IORef (Maybe [IO ()]) -> IO ()
handlePending ref = do
  queue <- atomicModifyIORef' ref (Nothing,)
  for_ queue (sequence_ . reverse)

-- | What no one waits for goes to the console when it fails, as an
-- uncaught error does in the browser.
report :: Text -> Outcome a -> IO ()
report what = \case
  Failed e -> hPutStrLn stderr ("Halogen: " <> what <> " failed: " <> show e)
  _ -> pass

fresh :: (Int -> a) -> DriverState m r s f act ps i o -> IO a
fresh f ds = atomicModifyIORef' ds.fresh (\i -> (i + 1, f i))
