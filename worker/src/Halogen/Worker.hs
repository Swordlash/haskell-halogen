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
-- spreads work over the pool's workers.
--
-- The page's loader must configure @globalThis.halogenWorker@ with the
-- program's URLs before starting Haskell. Without this configuration, or
-- outside a browser, a job runs on the caller's thread. See the README for
-- the loader, a complete example and the fallback's limitations.
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
  , terminate
  , available

    -- * Running jobs
  , start
  , run
  , WorkerError (..)
  )
where

import Control.Exception (Exception (..), throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import Data.Binary (Binary, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (STM, atomically)
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import Halogen.JSBits
import Halogen.JSBits.Promise (Promise (..))
import Halogen.Worker.Internal (Job (..), Request (..), answer, decodeReply, decodeStrict, encodeRequest, jobName, runCall)
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified

$( browserJS
     ["jsbits/worker.js"]
     [ ("js_available", "halogen_worker_available", Safe, [t|JSText -> IO Bool|])
     , ("js_pool", "halogen_worker_pool", Safe, [t|Int -> JSText -> IO JSVal|])
     , ("js_run", "halogen_worker_run", Safe, [t|JSVal -> JSVal -> IO (Promise ByteString)|])
     , ("js_terminate", "halogen_worker_terminate", Safe, [t|JSVal -> IO ()|])
     , ("js_in_worker", "halogen_worker_in_worker", Safe, [t|IO Bool|])
     , ("js_which_worker", "halogen_worker_which_worker", Safe, [t|IO Int|])
     , ("js_listen", "halogen_worker_listen", Safe, [t|Callback -> IO ()|])
     , ("js_request_id", "halogen_worker_request_id", Safe, [t|JSVal -> IO Int|])
     , ("js_request_bytes", "halogen_worker_request_bytes", Safe, [t|JSVal -> IO JSVal|])
     , ("js_reply", "halogen_worker_reply", Safe, [t|Int -> JSVal -> IO ()|])
     , ("js_reply_error", "halogen_worker_reply_error", Safe, [t|Int -> JSText -> IO ()|])
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
-- Export the binding of each @static@ from its module. With optimisation,
-- GHC 9.14 can give a binding its module keeps to itself a new name while
-- the module's static pointer table still refers to the old one, and the
-- program then fails to link, with an undefined reference to the binding's
-- closure; an exported binding keeps its name.
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
        answer request >>= \case
          Left problem -> js_reply_error requestId (toJSText problem)
          Right reply -> toJSBytes reply >>= js_reply requestId
      js_listen listener
      pure False

-- | 'serve', then run the program's page side only where 'serve' says to:
--
-- > main = Worker.program $ runBrowserDOM $ do ...
program :: IO () -> IO ()
program onPage = do
  page <- serve
  when page onPage

-- | Workers to run jobs on, each a copy of the program started from the
-- program's own files.
newtype Pool = Pool JSVal

-- | A pool of this many workers (at least one), none started yet: each is
-- started when a job is first given to it, and takes the time the page took
-- to load the program (fetching and compiling the binary, which the browser
-- caches, and running @main@ to 'serve'). Natively, and wherever 'available'
-- is 'False', a pool starts nothing.
newPool :: (MonadIO m) => Int -> m Pool
newPool size = liftIO (Pool <$> js_pool size arch)

-- | End the pool's workers. What they were running fails with a
-- 'WorkerError'; a later job starts the workers again.
terminate :: (MonadIO m) => Pool -> m ()
terminate (Pool pool) = liftIO (js_terminate pool)

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
-- lists.
--
-- Where 'available' is 'False' the job runs here, before 'start' returns,
-- through the same bytes, and the promise has settled by then. That holds
-- the page's thread for as long as the computation takes: in a component's
-- @liftIO@ the component's loop is free meanwhile, but the page is not,
-- since only a worker gives the page its thread back.
start :: (MonadIO m) => Pool -> StaticPtr (Job a b) -> a -> m (Promise b)
start pool pointer argument = liftIO $ do
  (raw, outcome) <- launch pool pointer argument
  let rejection message = JSError {name = "WorkerError", message = Text.pack (displayException (WorkerError (jobName pointer) message))}
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
-- Killing the thread that waits (the component's fiber) stops the wait,
-- not the job: there is no cancelling a job in this version, and the worker
-- finishes it, its result unread. 'terminate' ends a worker and everything
-- in it.
run :: (MonadIO m) => Pool -> StaticPtr (Job a b) -> a -> m b
run pool pointer argument = liftIO $ do
  (_, outcome) <- launch pool pointer argument
  atomically outcome >>= either (throwIO . WorkerError (jobName pointer)) pure

-- | The work of 'start' and 'run': the job given to a worker, or run here,
-- and the JavaScript promise of its reply (@null@ for a job run here) with
-- its outcome, which blocks until it has settled, a failure as a message
-- for the two to make their own error of. The worker's rejections and a
-- reply that does not decode both become that message, so that whoever
-- waits sees one kind of failure.
launch :: Pool -> StaticPtr (Job a b) -> a -> IO (JSVal, STM (Either Text b))
launch (Pool pool) pointer argument = do
  let theJob = deRefStaticPtr pointer
      bytes = theJob.encodeArgument argument
  workers <- available
  if workers
    then do
      request <- toJSBytes (encodeRequest Request {key = staticKey pointer, arguments = [bytes]})
      replied <- js_run pool request
      let outcome = (\case Left e -> Left e.message; Right reply -> fromReply theJob reply) <$> replied.settled
      pure (replied.rawPromise, outcome)
    else do
      outcome <- runCall theJob.call bytes
      pure (jsNull, pure (outcome >>= decoded theJob))

-- | A reply's one result, decoded.
fromReply :: Job a b -> ByteString -> Either Text b
fromReply theJob reply = case decodeReply reply of
  Left problem -> Left ("the reply does not decode: " <> Text.pack problem)
  Right [result] -> decoded theJob result
  Right results -> Left ("the reply has " <> Text.pack (show (length results)) <> " results for one argument")

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
