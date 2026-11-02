{-# LANGUAGE TemplateHaskell #-}

-- | Run expensive Haskell functions in Web Workers while the page handles
-- input and draws. Each worker loads a separate copy of the program and has
-- its own heap. Workers have no DOM and cannot access component state.
--
-- A job is a function made with 'job' under GHC's @static@ (the
-- @StaticPointers@ extension), which gives it a key that is the same in
-- every copy of the program: that key is all a worker needs to find the
-- function in its own copy. The argument and the result cross as
-- "Data.Binary" bytes, in an @ArrayBuffer@ transferred rather than copied.
--
-- > {-# LANGUAGE StaticPointers #-}
-- > import GHC.StaticPtr (StaticPtr)
-- >
-- > primes :: StaticPtr (Worker.Job Int Int)
-- > primes = static (Worker.job (\n -> pure (countPrimes n)))
-- >
-- > main = Worker.program $ runBrowserDOM $ do
-- >   pool <- Worker.newPool 2
-- >   ...
-- > -- and in a component:
-- >   n <- liftIO (Worker.run pool primes 2000000)
--
-- 'start' gives a 'Promise' of the result at once instead of waiting, so
-- that "Halogen.JSBits.Promise"'s vocabulary ('Halogen.JSBits.Promise.mapConcurrently',
-- 'Halogen.JSBits.Promise.concurrently', 'Halogen.JSBits.Promise.race')
-- spreads work over the pool's workers. A job can be stopped: 'cancel' asks
-- it to stop at its next 'checkpoint', 'kill' ends the worker it runs in, and
-- 'run' cancels its job when its wait is interrupted.
--
-- The page's loader must configure @globalThis.halogenWorker@ with the
-- program's URLs before starting Haskell. Without this configuration, or
-- outside a browser, a job runs in the program itself, on a thread 'start'
-- forks. See the README for the loader, a complete example and the
-- fallback's limitations.
module Halogen.Worker
  ( -- * Jobs
    Job
  , job
  , whichWorker

    -- * Serving them
  , serve
  , program

    -- * Pools
  , Pool
  , newPool
  , size
  , terminate
  , available

    -- * Running jobs
  , start
  , run
  , WorkerError (..)

    -- * Stopping them
  , cancel
  , kill
  , checkpoint
  , Cancelled (..)

    -- * For "Halogen.Worker.Par"
  , sendTo
  , startHere
  )
where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, myThreadId, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (Exception (..), SomeException, evaluate, finally, mask, mask_, onException, throwIO, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import Data.Binary (Binary, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (STM, atomically, newTVarIO, readTVar, retry, writeTVar)
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import Halogen.JSBits
import Halogen.JSBits.Promise (Promise (..), wait)
import Halogen.Worker.Internal (Job (..), Reply (..), Request (..), answer, decodeReply, decodeStrict, encodeRequest, jobName, runCall)
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified
import System.Mem.StableName (StableName, makeStableName)

$( browserJS
     ["jsbits/worker.js"]
     [ ("js_available", "halogen_worker_available", Safe, [t|JSText -> IO Bool|])
     , ("js_pool", "halogen_worker_pool", Safe, [t|Int -> JSText -> IO JSVal|])
     , ("js_run", "halogen_worker_run", Safe, [t|JSVal -> JSVal -> IO (Promise ByteString)|])
     , ("js_run_on", "halogen_worker_run_on", Safe, [t|JSVal -> Int -> JSVal -> IO (Promise ByteString)|])
     , ("js_terminate", "halogen_worker_terminate", Safe, [t|JSVal -> IO ()|])
     , ("js_in_worker", "halogen_worker_in_worker", Safe, [t|IO Bool|])
     , ("js_which_worker", "halogen_worker_which_worker", Safe, [t|IO Int|])
     , ("js_listen", "halogen_worker_listen", Safe, [t|Callback -> IO ()|])
     , ("js_request_id", "halogen_worker_request_id", Safe, [t|JSVal -> IO Int|])
     , ("js_request_bytes", "halogen_worker_request_bytes", Safe, [t|JSVal -> IO JSVal|])
     , ("js_reply", "halogen_worker_reply", Safe, [t|Int -> JSVal -> IO ()|])
     , ("js_reply_error", "halogen_worker_reply_error", Safe, [t|Int -> JSText -> IO ()|])
     , ("js_cancel", "halogen_worker_cancel", Safe, [t|JSVal -> JSVal -> IO ()|])
     , ("js_kill", "halogen_worker_kill", Safe, [t|JSVal -> JSVal -> IO ()|])
     , ("js_turn", "halogen_worker_turn", Safe, [t|IO (Promise ())|])
     , ("js_is_cancelled", "halogen_worker_is_cancelled", Safe, [t|Int -> IO Bool|])
     , ("js_forget", "halogen_worker_forget", Safe, [t|Int -> IO ()|])
     ]
 )

-- | A function from @a@ to @b@ that a worker can run, with the codecs of its
-- argument and result: a function of 'Binary' types (a tuple or a record
-- for several arguments, @deriving anyclass (Binary)@ with 'GHC.Generics.Generic'
-- for a type of the program's own).
--
-- A job is only of use under @static@, which is what gives it a key the
-- worker can find it by:
--
-- > fibs :: StaticPtr (Worker.Job [Int] [Integer])
-- > fibs = static (Worker.job (pure . map fib))
--
-- @static e@ needs @e@ to be closed: it may name top-level functions and
-- values, but no local variable, since what a local variable holds is in the
-- page's heap and not in the worker's. The compiler checks it. So
-- @Worker.job f@ can be static whenever @f@ is a top-level function or a
-- lambda that names nothing local, and the 'Binary' instances it needs are
-- resolved where it is written and kept inside the static value: there is no
-- table of jobs and no class of serialisable functions to keep up.
--
-- Export every binding that holds a @static@ from its module. GHC before
-- 10.0 (issue #24773) can fail to link a module whose static binding is not
-- exported and is used more than once: with optimisation the binding stays
-- internal to the module, and the entry of its static pointer table that
-- refers to it has nothing to link to.
--
-- The function runs in 'IO', for a job that reads a clock or uses an
-- 'Data.IORef.IORef' of its own (each copy of the program has its own); a
-- pure one is @\\x -> pure (f x)@. Its result is encoded completely before
-- the job counts as done, so a lazy error in it is the job's error.
job :: (Binary a, Binary b) => (a -> IO b) -> Job a b
job function =
  Job
    { call = \bytes -> case decodeStrict bytes of
        Left problem -> throwIO (BadArguments problem)
        Right argument -> Lazy.toStrict . encode <$> function argument
    , encodeArgument = Lazy.toStrict . encode
    , decodeResult = decodeStrict
    }

newtype BadArguments = BadArguments String
  deriving stock (Show)

instance Exception BadArguments where
  displayException (BadArguments problem) = "its argument does not decode: " <> problem

-- | Which of its pool's workers this copy of the program is, counting from
-- 0, or 'Nothing' on the page (and wherever a job runs in the program
-- itself). A job can read it to say where it ran; nothing else depends on
-- it.
whichWorker :: (MonadIO m) => m (Maybe Int)
whichWorker = liftIO $ do
  inWorker <- js_in_worker
  if inWorker then Just <$> js_which_worker else pure Nothing

-- | Whether 'serve' has been called, which 'available' waits for: a worker
-- whose program does not serve would never answer.
served :: IORef Bool
served = unsafePerformIO (newIORef False)
{-# NOINLINE served #-}

-- | What a program's @main@ does first. In a worker a pool started, it
-- answers the pool's requests and returns 'False': @main@ should then do
-- nothing else, since a worker has no page to mount on. Anywhere else it
-- returns 'True', and @main@ goes on.
--
-- A worker finds each job it is asked for in the program's static pointer
-- table, which GHC fills with every @static@ of the program, so there is
-- nothing to register.
serve :: IO Bool
serve = do
  writeIORef served True
  inWorker <- js_in_worker
  if not inWorker
    then pure True
    else do
      listener <- mkCallback $ \message -> do
        requestId <- js_request_id message
        request <- fromJSBytes =<< js_request_bytes message
        me <- myThreadId
        atomicModifyIORef' answering (\ids -> (Map.insert me requestId ids, ()))
        -- A turn of the worker's event loop first, so that a request
        -- cancelled while it waited behind another job is not started.
        outcome <-
          ( try checkpoint >>= \case
              Left Cancelled -> pure (Left "the job was cancelled before it started")
              Right () -> answer request
          )
            `finally` do
              atomicModifyIORef' answering (\ids -> (Map.delete me ids, ()))
              js_forget requestId
        case outcome of
          Left problem -> js_reply_error requestId (toJSText problem)
          Right reply -> toJSBytes reply >>= js_reply requestId
      js_listen listener
      pure False

-- | In a worker, the request each thread is answering, for 'checkpoint' to
-- know which request to look for among the cancelled ones. A job runs on
-- the thread of the callback its request came to; a thread a job forks has
-- no request, and its 'checkpoint' only yields.
answering :: IORef (Map ThreadId Int)
answering = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE answering #-}

-- | 'serve', then run the program's page side only where 'serve' says to:
--
-- > main = Worker.program $ runBrowserDOM $ do ...
program :: IO () -> IO ()
program onPage = do
  page <- serve
  when page onPage

-- | Workers to run jobs on, each a copy of the program started from the
-- program's own files, and the jobs it runs in the program itself where no
-- worker can be made.
data Pool = Pool
  { workers :: JSVal
  , count :: Int
  , local :: IORef [(StableName JSVal, Local)]
  -- ^ The jobs running in the program itself, by the 'rawPromise' of the
  -- promise 'start' gave for each, for 'cancel' and 'kill' to find. That
  -- value is a number made for the job, which keeps its identity through
  -- an @fmap@ of the promise, and its stable name is the key, since a
  -- 'JSVal' has no equality on every backend. The workers' jobs are found
  -- by the JavaScript, from the promise itself.
  }

-- | A job running in the program itself: its thread, and how to settle its
-- promise when it is stopped before it ends.
data Local = Local
  { thread :: ThreadId
  , settle :: JSError -> STM ()
  }

-- | A pool of this many workers (at least one), none started yet: each is
-- started when a job is first given to it, and takes the time the page took
-- to load the program (fetching and compiling the binary, which the browser
-- caches, and running @main@ to 'serve'). Natively, and wherever 'available'
-- is 'False', a pool starts nothing.
newPool :: (MonadIO m) => Int -> m Pool
newPool n = liftIO (Pool <$> js_pool n arch <*> pure (max 1 n) <*> newIORef [])

-- | How many workers the pool has (the number 'newPool' was given, or one),
-- whether or not they have started.
size :: Pool -> Int
size pool = pool.count

-- | Send a request's bytes to the pool's worker of this number (from 0, below
-- 'size'), starting it if it has not been, and give a promise of the reply's
-- bytes: what "Halogen.Worker.Par" hands each batch to the worker that is
-- free. Not for applications, which give jobs to the pool with 'start' and
-- 'run', and let it choose the worker. 'cancel' and 'kill' take the promise.
sendTo :: Pool -> Int -> ByteString -> IO (Promise ByteString)
sendTo pool index request = toJSBytes request >>= js_run_on pool.workers index

-- | End the pool's workers, and the jobs it runs in the program itself.
-- What they were running fails with a 'WorkerError'; a later job starts the
-- workers again.
terminate :: (MonadIO m) => Pool -> m ()
terminate pool = liftIO $ do
  js_terminate pool.workers
  running <- atomicModifyIORef' pool.local ([],)
  for_ running $ \(_, running') -> stopLocal running' JSError {name = "WorkerError", message = "the pool was terminated"}

-- | Whether a job would run in a worker here: in a browser that has
-- @Worker@, whose loader said where the program is (@globalThis.halogenWorker@,
-- for this program's backend), once 'serve' has been called, and not inside
-- a worker. Otherwise jobs run in the program itself.
available :: (MonadIO m) => m Bool
available = liftIO $ do
  serving <- readIORef served
  if serving then js_available arch else pure False

-- | Give the job to the next worker of the pool (they take turns), and a
-- 'Promise' of its result at once. Nothing waits: 'Halogen.JSBits.Promise.wait'
-- for the promise where a thread may wait (a component's @liftIO@), or start
-- several first, which is how the pool's workers compute side by side:
--
-- > counts <- liftIO (mapConcurrently (Worker.start pool primes) [1000000, 2000000])
--
-- The promise is rejected with a 'JSError' named @WorkerError@, whose message
-- is the 'WorkerError' 'run' would throw, for each of the failures 'run'
-- lists; with one named @Cancelled@ once 'cancel'led, and @Killed@ once
-- 'kill'ed. Waiting for it and being interrupted does not stop the job, as
-- it does in 'run': a promise can be waited for by several threads, or by
-- none, so stopping it is 'cancel''s business.
--
-- Where 'available' is 'False' the job runs here, on a thread of its own
-- that 'start' forks, through the same bytes, and the promise's
-- 'rawPromise' is a number that identifies the job rather than a JavaScript
-- promise. That thread holds the page's thread for as long as the
-- computation takes, between its waits and 'checkpoint's: the component's
-- loop is free meanwhile, but the page is not, since only a worker gives the
-- page its thread back.
start :: (MonadIO m) => Pool -> StaticPtr (Job a b) -> a -> m (Promise b)
start pool pointer argument = liftIO $ do
  (raw, outcome) <- launch pool pointer argument
  let rejection e = JSError {name = e.name, message = Text.pack (displayException (WorkerError (jobName pointer) e.message))}
  pure Promise {rawPromise = raw, settled = first rejection <$> outcome}

-- | 'start' the job and wait for its result. In a component this is a
-- @liftIO@ (or @lift@), which runs on a thread of its own while the tree
-- goes on; the page's thread is free meanwhile, since the work is in the
-- worker's. Consecutive calls in one thread wait for one another; 'start'
-- several to use several workers.
--
-- An exception in the job, an argument it cannot decode, a result the
-- caller cannot, a key the worker's program does not have, a worker that
-- could not start, and a pool terminated while the job ran are each thrown
-- here as a 'WorkerError'.
--
-- An exception that interrupts the wait (the component's fiber killed, the
-- losing side of @race@, a @timeout@) 'cancel's the job before it goes on,
-- so a job that calls 'checkpoint' stops soon after, and one that does not
-- is left to finish, its result unread.
run :: (MonadIO m) => Pool -> StaticPtr (Job a b) -> a -> m b
run pool pointer argument = liftIO $ mask $ \restore -> do
  (raw, outcome) <- launch pool pointer argument
  result <- restore (atomically outcome) `onException` stop Cancelling pool raw
  either (\e -> throwIO (WorkerError (jobName pointer) e.message)) pure result

-- | The work of 'start' and 'run': the job given to a worker, or forked to
-- run here, and the value that identifies it ('rawPromise'), with its
-- outcome, which blocks until it has settled. A failure is a 'JSError' with
-- what went wrong as its message, for the two to make their own error of,
-- and named @Cancelled@, @Killed@ or, for every other failure, @WorkerError@:
-- the worker's rejections and a reply that does not decode are one kind of
-- failure to whoever waits.
launch :: Pool -> StaticPtr (Job a b) -> a -> IO (JSVal, STM (Either JSError b))
launch pool pointer argument = do
  let theJob = deRefStaticPtr pointer
      bytes = theJob.encodeArgument argument
  inWorkers <- available
  if inWorkers
    then do
      request <- toJSBytes (encodeRequest Request {key = staticKey pointer, arguments = [bytes]})
      replied <- js_run pool.workers request
      let outcome = (\case Left e -> Left (named e); Right reply -> first failed (fromReply theJob reply)) <$> replied.settled
      pure (replied.rawPromise, outcome)
    else do
      replied <- startHere pool (either (Left . failed) (first failed . decoded theJob) <$> runCall theJob.call bytes)
      pure (replied.rawPromise, replied.settled)
  where
    named e
      | e.name `elem` ["Cancelled", "Killed"] = e
      | otherwise = JSError {name = "WorkerError", message = e.message}

-- | Run the action as a job in the program itself, on a thread of its own,
-- and give a promise of its outcome: what 'start' and 'run' do with a job
-- where 'available' is 'False', and what "Halogen.Worker.Par" does with each
-- batch there. The promise's 'rawPromise' is a number that identifies the
-- job rather than a JavaScript promise, by which 'cancel', 'kill' and
-- 'terminate' find the thread; an exception the action lets through is the
-- job's failure. Not for applications.
startHere :: Pool -> IO (Either JSError r) -> IO (Promise r)
startHere pool action = do
  box <- newTVarIO Nothing
  raw <- evaluate . toJSInt =<< atomicModifyIORef' tokens (\n -> (n + 1, n))
  name <- makeStableName raw
  let fill outcome = readTVar box >>= maybe (writeTVar box (Just outcome)) (const (pure ()))
      forget = atomicModifyIORef' pool.local (\running -> (filter ((/= name) . fst) running, ()))
  -- The thread waits for its entry to be made before it begins, so that
  -- its 'forget' cannot come first and leave the entry behind; and it
  -- begins masked, so that a 'cancel' before it has begun still finds
  -- the 'finally' in place.
  --
  -- Whatever ends the thread settles the promise: the action gives the
  -- job's own outcome, and an exception it lets through (one thrown to the
  -- thread from elsewhere than 'cancel', 'kill' and 'terminate', which
  -- settle the promise before they throw, or a 'ThreadKilled' the job
  -- throws itself) is the job's failure too, so that nobody waits for a
  -- thread that is gone. An outcome a stop supplied first is kept, since
  -- 'fill' writes only once.
  registered <- newEmptyMVar
  mask_ $ do
    thread <- forkIOWithUnmask $ \unmask -> do
      readMVar registered
      ended <- try (unmask action)
      atomically
        ( fill $ case ended of
            Left (e :: SomeException) -> Left (failed (Text.pack (displayException e)))
            Right outcome -> outcome
        )
        `finally` forget
    atomicModifyIORef' pool.local (\running -> ((name, Local {thread, settle = fill . Left}) : running, ()))
    putMVar registered ()
  pure Promise {rawPromise = raw, settled = readTVar box >>= maybe retry pure}

-- | A failure of a job run here, as the promise is rejected with it.
failed :: Text -> JSError
failed message = JSError {name = "WorkerError", message}

-- | The numbers that identify the jobs run in the program itself.
tokens :: IORef Int
tokens = unsafePerformIO (newIORef 0)
{-# NOINLINE tokens #-}

-- | Ask the job behind the promise to stop: its promise is rejected at once,
-- with a 'JSError' named @Cancelled@ ('run' throws a 'WorkerError' with its
-- message), and its result, should it come, is dropped.
--
-- A job the worker has not begun is taken out of its queue, and never runs.
-- One that is running learns of the cancel at its next 'checkpoint', which
-- throws 'Cancelled' there; the worker is then free for the next job. A job
-- that never calls 'checkpoint' cannot be stopped this way: it runs to the
-- end, holding its worker, and 'kill' is what stops it. Where no worker can
-- be made the job's thread is killed, which stops it at its next wait or
-- 'checkpoint' (or sooner, when the runtime switches threads).
--
-- A promise the pool did not start, or one that has settled, is left alone.
cancel :: (MonadIO m) => Pool -> Promise b -> m ()
cancel pool p = liftIO (stop Cancelling pool p.rawPromise)

-- | Stop the job behind the promise at once, by ending the worker that runs
-- it: its promise is rejected with a 'JSError' named @Killed@. The other
-- requests that worker had are sent again, in order, to a new worker in its
-- place, so only the killed job is lost. The new worker loads the program
-- as any worker does when it starts (the binary from the browser's cache,
-- @main@ run up to 'serve'), which is what makes this the dearer way to stop
-- a job, and the only one for a job that does not 'checkpoint'. Where no
-- worker can be made it is 'cancel'.
--
-- A promise the pool did not start, or one that has settled, is left alone.
kill :: (MonadIO m) => Pool -> Promise b -> m ()
kill pool p = liftIO (stop Killing pool p.rawPromise)

data Stopping = Cancelling | Killing

-- | 'cancel' or 'kill' the job a 'rawPromise' stands for: one running in the
-- program itself, found by the value's stable name, or else one the
-- worker's JavaScript finds from the promise. Neither waits for the job to
-- stop.
stop :: Stopping -> Pool -> JSVal -> IO ()
stop how pool raw = do
  name <- makeStableName =<< evaluate raw
  found <- atomicModifyIORef' pool.local (\running -> let (these, others) = partition ((== name) . fst) running in (others, these))
  case (found, how) of
    ((_, here) : _, Cancelling) -> stopLocal here JSError {name = "Cancelled", message = "the job was cancelled"}
    ((_, here) : _, Killing) -> stopLocal here JSError {name = "Killed", message = "the job was killed"}
    ([], Cancelling) -> js_cancel pool.workers raw
    ([], Killing) -> js_kill pool.workers raw

-- | Settle a job's promise with the error, then kill its thread from
-- another, since 'killThread' waits until the exception has been delivered,
-- which for a computation that does not allocate can be a while.
stopLocal :: Local -> JSError -> IO ()
stopLocal here e = do
  atomically (here.settle e)
  void (forkIO (killThread here.thread))

-- | What a job calls between pieces of its work, to be stoppable by
-- 'cancel'. In a worker it hands the worker's event loop a turn, which is
-- when a cancel the page sent arrives, then throws 'Cancelled' if this job
-- was cancelled. A computation keeps the worker's only thread until it
-- waits, so a job that never waits nor checkpoints hears of no cancel.
--
-- A turn of the event loop is a macrotask (a @setTimeout@ of 0), which costs
-- a fraction of a millisecond or more: call it once for each chunk of the
-- work, not for each element. Where the job runs in the program itself it
-- only 'yield's, which is where the thread 'cancel' kills stops.
checkpoint :: IO ()
checkpoint = do
  me <- myThreadId
  requestId <- Map.lookup me <$> readIORef answering
  case requestId of
    Nothing -> yield
    Just this -> do
      wait =<< js_turn
      cancelled <- js_is_cancelled this
      when cancelled (throwIO Cancelled)

-- | What 'checkpoint' throws in a job that was cancelled. The worker catches
-- it like any exception of the job's, and the page, which dropped the job
-- when it cancelled it, ignores the answer.
data Cancelled = Cancelled
  deriving stock (Eq, Show)

instance Exception Cancelled where
  displayException Cancelled = "the job was cancelled"

-- | A reply's one result, decoded.
fromReply :: Job a b -> ByteString -> Either Text b
fromReply theJob reply = case decodeReply reply of
  Left problem -> Left ("the reply does not decode: " <> Text.pack problem)
  Right Reply {failure = Just problem} -> Left problem
  Right Reply {results = [result]} -> decoded theJob result
  Right Reply {results} -> Left ("the reply has " <> Text.pack (show (length results)) <> " results for one argument")

decoded :: Job a b -> ByteString -> Either Text b
decoded theJob = first (\problem -> "its result does not decode: " <> Text.pack problem) . theJob.decodeResult

-- | A job that failed, by where its @static@ is (the module, line and
-- column), and what went wrong.
data WorkerError = WorkerError
  { job :: Text
  , message :: Text
  }
  deriving stock (Eq, Show)

instance Exception WorkerError where
  displayException e = Text.unpack ("the job at " <> e.job <> " failed: " <> e.message)

-- | The backend, for the pool to know how to start a copy of the program.
arch :: JSText
arch = toJSText (Text.pack System.Info.arch)
