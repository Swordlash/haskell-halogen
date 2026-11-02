-- | A computation whose independent parts run on a pool's workers at once,
-- and whose calls of one job go to a worker in batches: the shape of Haxl,
-- without its cache.
--
-- A 'task' is one call of a job. 'Par' is an 'Applicative' whose '<*>'
-- runs both sides at once, so that their tasks are collected together, and
-- a 'Monad' whose '>>=' waits for the left side before it runs the right,
-- whose tasks depend on what the left gave. With @ApplicativeDo@ a @do@
-- block is taken apart where its statements do not depend on one another:
--
-- > {-# LANGUAGE ApplicativeDo #-}
-- > report = do
-- >   a <- task primes 1000000      -- these two are sent at once,
-- >   b <- task primes 2000000      -- to the pool's workers
-- >   c <- task primes (a + b)      -- this one after both
-- >   pure (a, b, c)
--
-- and @traverse (task fibs) chunks@ is a map of a job over a list on all
-- the pool's workers, the results in the list's order:
--
-- > counts <- liftIO (runPar pool (traverse (task primesBetween) parts))
--
-- 'runPar' runs the computation until every part of it waits for a task,
-- groups the tasks it has by job, cuts each job's into batches (several for
-- each worker, see 'Halogen.Worker.Internal.plan') and hands them out from a
-- queue, the next batch to whichever worker has just answered its last, so
-- that a worker given the quick calls is not left idle while another
-- finishes the slow ones. A batch is one request: the worker makes its
-- calls one after another, and a batch is worth its request when its calls
-- are seconds of work together.
module Halogen.Worker.Par
  ( Par
  , task
  , runPar
  )
where

import Control.Concurrent (forkIO, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception (..), SomeException, mask, mask_, onException, throwIO, try)
import Control.Monad (forM, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Foldable (for_, toList, traverse_)
import Data.IORef (atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.StaticPtr (StaticKey, StaticPtr, deRefStaticPtr, staticKey)
import Halogen.JSBits.Promise (JSError (..), waitCatch)
import Halogen.Worker (Pool, WorkerError (..), available, cancel, sendTo, size)
import Halogen.Worker.Internal (Batch (..), Job (..), Reply (..), Request (..), decodeReply, encodeRequest, jobName, plan, runCall)

-- | A computation that runs tasks on a pool, run with 'runPar'.
newtype Par a = Par (IO (Result a))

-- | What running a 'Par' gives: its value, or the tasks it waits for and
-- what to run once they have answered.
data Result a
  = Done a
  | Blocked (Seq Call) (Par a)

-- | One task: its job, by key and by name, its argument's bytes, and what
-- to do with its result's.
data Call = Call
  { key :: StaticKey
  , name :: Text
  , function :: ByteString -> IO ByteString
  -- ^ The job's function, for a call made in the program itself.
  , argument :: ByteString
  , deliver :: ByteString -> IO (Either Text ())
  -- ^ Decode the result and keep it for the task, or say why it does not
  -- decode.
  }

instance Functor Par where
  fmap f (Par run) =
    Par $
      run >>= \case
        Done a -> pure (Done (f a))
        Blocked calls rest -> pure (Blocked calls (fmap f rest))

-- | Both sides are run before either's tasks are sent, so that their tasks
-- go out together.
instance Applicative Par where
  pure = Par . pure . Done
  Par runF <*> Par runX = Par $ do
    f <- runF
    x <- runX
    pure $ case (f, x) of
      (Done g, Done a) -> Done (g a)
      (Done g, Blocked calls rest) -> Blocked calls (g <$> rest)
      (Blocked calls rest, Done a) -> Blocked calls (($ a) <$> rest)
      (Blocked calls rest, Blocked others more) -> Blocked (calls <> others) (rest <*> more)

-- | The right side runs once the left has its value, so its tasks are sent
-- after the left's have answered.
instance Monad Par where
  Par run >>= f =
    Par $
      run >>= \case
        Done a -> let Par next = f a in next
        Blocked calls rest -> pure (Blocked calls (rest >>= f))

-- | A call of the job, made on one of the pool's workers when 'runPar' sends
-- the tasks it has collected. Each 'task' is a call of its own: there is no
-- cache, and the same task twice is two calls.
task :: StaticPtr (Job a b) -> a -> Par b
task pointer argument = Par $ do
  box <- newIORef Nothing
  let theJob = deRefStaticPtr pointer
      deliver bytes = case theJob.decodeResult bytes of
        Left problem -> pure (Left ("its result does not decode: " <> Text.pack problem))
        Right value -> Right <$> writeIORef box (Just value)
      call =
        Call
          { key = staticKey pointer
          , name = jobName pointer
          , function = theJob.call
          , argument = theJob.encodeArgument argument
          , deliver
          }
  pure (Blocked (Seq.singleton call) (Par (Done <$> (readIORef box >>= maybe (throwIO Unanswered) pure))))

-- | A task read before its answer came, which 'runPar' never lets happen.
data Unanswered = Unanswered
  deriving stock (Show)

instance Exception Unanswered where
  displayException Unanswered = "Halogen.Worker.Par: a task was read before its answer came"

-- | Run the computation on the pool, waiting for its tasks, and give its
-- value. In a component this is a @liftIO@, as 'Halogen.Worker.run' is.
--
-- A task that fails fails the whole computation with a 'WorkerError' naming
-- its job, whose message says which of the job's calls it was ("its call at
-- index 3", counting from 0, among that job's calls sent together, which for
-- a @traverse@ is the list's index) and why. The batches still running are
-- then cancelled ('Halogen.Worker.cancel'), as they are when 'runPar''s own
-- thread is interrupted (a killed fiber, a @timeout@); a job that calls
-- 'Halogen.Worker.checkpoint' stops soon after.
--
-- Where 'available' is 'False' the batches run here, on the calling thread,
-- one after the other, through the same codecs: the same results and the
-- same failures, with the page's thread held while they compute.
runPar :: (MonadIO m) => Pool -> Par a -> m a
runPar pool = liftIO . go
  where
    go (Par run) =
      run >>= \case
        Done a -> pure a
        Blocked calls rest -> perform pool calls >> go rest

-- | Make the calls and deliver their results, or throw the first failure.
perform :: Pool -> Seq Call -> IO ()
perform pool calls = do
  let batches = plan (size pool) (map (.key) (toList calls))
  inWorkers <- available
  if inWorkers
    then dispatch pool calls batches
    else for_ batches $ \batch -> runHere (callsOf calls batch) batch.offset

callsOf :: Seq Call -> Batch StaticKey -> [Call]
callsOf calls batch = map (Seq.index calls) batch.positions

-- | A batch's calls made in the program itself, in order, stopping at the
-- first that fails.
runHere :: [Call] -> Int -> IO ()
runHere these offset = for_ (zip [offset ..] these) $ \(index, call) ->
  runCall call.function call.argument >>= \case
    Left problem -> throwIO (callFailure call index problem)
    Right bytes -> call.deliver bytes >>= either (throwIO . callFailure call index) pure

callFailure :: Call -> Int -> Text -> WorkerError
callFailure call index problem = WorkerError {job = call.name, message = "its call at index " <> Text.pack (show index) <> ": " <> problem}

-- | Hand the batches out to the pool's workers: one thread for each worker
-- (no more than there are batches), which takes the next batch from the
-- queue, sends it to its worker, delivers the reply and takes another,
-- until the queue is empty. The first failure empties the queue and
-- cancels the batches the other workers are running; so does an exception
-- that interrupts the wait for them, which is then rethrown.
dispatch :: Pool -> Seq Call -> [Batch StaticKey] -> IO ()
dispatch pool calls batches = do
  queue <- newIORef batches
  inFlight <- newIORef Map.empty
  failure <- newIORef Nothing
  halted <- newIORef False
  let halt = do
        atomicWriteIORef halted True
        atomicWriteIORef queue []
        flying <- atomicModifyIORef' inFlight (Map.empty,)
        traverse_ (cancel pool) flying
      failWith e = do
        first <- atomicModifyIORef' failure (\case Nothing -> (Just e, True); earlier -> (earlier, False))
        when first halt
      feed slot = do
        next <- atomicModifyIORef' queue (\case [] -> ([], Nothing); b : bs -> (bs, Just b))
        for_ next $ \batch -> do
          let these = callsOf calls batch
          -- Sent and registered in one piece, with the halt flag read
          -- after: a halt before the registration is seen by the flag,
          -- which halt sets first, and one after it finds the batch in
          -- flight, so either way the batch is cancelled; and a kill from
          -- runPar's own interruption lands on the wait below, not between
          -- the send and the registration.
          replied <- mask_ $ do
            replied <- sendTo pool slot (encodeRequest Request {key = batch.key, arguments = map (.argument) these})
            atomicModifyIORef' inFlight (\flying -> (Map.insert slot replied flying, ()))
            stopped <- readIORef halted
            when stopped (cancel pool replied)
            pure replied
          outcome <- waitCatch replied
          atomicModifyIORef' inFlight (\flying -> (Map.delete slot flying, ()))
          stoppedSince <- readIORef halted
          unless stoppedSince $
            settle these batch.offset outcome >>= either failWith (const (feed slot))
  mask $ \restore -> do
    feeders <- forM [0 .. min (size pool) (length batches) - 1] $ \slot -> do
      finished <- newEmptyMVar
      thread <- forkIOWithUnmask $ \unmask -> try @SomeException (unmask (feed slot)) >>= putMVar finished
      pure (thread, finished)
    outcomes <-
      restore (traverse (takeMVar . snd) feeders) `onException` do
        halt
        for_ feeders $ \(thread, _) -> void (forkIO (killThread thread))
    readIORef failure >>= traverse_ throwIO
    for_ outcomes (either throwIO pure)

-- | A batch's reply: its results delivered to its calls, or the batch's
-- failure, named after the call that failed (or the batch's first, for a
-- request that failed as a whole).
settle :: [Call] -> Int -> Either JSError ByteString -> IO (Either WorkerError ())
settle these offset outcome = case (these, outcome) of
  ([], _) -> pure (Right ())
  (first : _, Left e) -> pure (Left WorkerError {job = first.name, message = e.message})
  (first : _, Right bytes) -> case decodeReply bytes of
    Left problem -> pure (Left WorkerError {job = first.name, message = "the reply does not decode: " <> Text.pack problem})
    Right reply -> deliverAll (zip3 [offset ..] these (map Just reply.results <> repeat Nothing)) reply.failure
  where
    deliverAll [] _ = pure (Right ())
    deliverAll ((index, call, result) : rest) failed = case (result, failed) of
      (Just bytes, _) ->
        call.deliver bytes >>= \case
          Left problem -> pure (Left (callFailure call index problem))
          Right () -> deliverAll rest failed
      (Nothing, Just problem) -> pure (Left (callFailure call index problem))
      (Nothing, Nothing) -> pure (Left (callFailure call index "the reply has no result for it"))
