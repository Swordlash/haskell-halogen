{-# LANGUAGE TemplateHaskell #-}

-- | Run expensive Haskell functions in Web Workers while the page handles
-- input and draws. Each worker loads a separate copy of the program and has
-- its own heap. Workers have no DOM and cannot access component state.
--
-- Register named 'Job's with 'program' in @main@, create a 'Pool', and call
-- 'run' from a component's @liftIO@. Arguments and results use aeson JSON;
-- pass the calculation's inputs explicitly, since closures and IORefs do
-- not cross between heaps. Concurrent calls can use multiple workers;
-- consecutive calls in one thread wait for one another.
--
-- > primes = Worker.job "primes" (\n -> pure (countPrimes n))
-- > main = Worker.program [Worker.some primes] $ runBrowserDOM $ do
-- >   pool <- Worker.newPool 2
-- >   ...
--
-- The page's loader must configure @globalThis.halogenWorker@ with the
-- program's URLs before starting Haskell. Without this configuration, or
-- outside a browser, 'run' executes the job on the caller's thread. See the
-- README for the loader, a complete example and the fallback's limitations.
module Halogen.Worker
  ( -- * Jobs
    Job
  , job
  , SomeJob
  , some

    -- * Serving them
  , serve
  , program

    -- * Running them
  , Pool
  , newPool
  , terminate
  , run
  , available
  , WorkerError (..)
  )
where

import Control.Exception (Exception (..), SomeException (..), evaluate, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Halogen.JSBits
import Halogen.JSBits.Promise (JSError (..), Promise (..), await)
import System.IO.Unsafe (unsafePerformIO)
import System.Info qualified
import UnliftIO.Exception (tryAny)

$( browserJS
     ["jsbits/worker.js"]
     [ ("js_available", "halogen_worker_available", Unsafe, [t|JSText -> IO Bool|])
     , ("js_pool", "halogen_worker_pool", Unsafe, [t|Int -> JSText -> IO JSVal|])
     , ("js_run", "halogen_worker_run", Unsafe, [t|JSVal -> JSText -> JSText -> IO Promise|])
     , ("js_terminate", "halogen_worker_terminate", Unsafe, [t|JSVal -> IO ()|])
     , ("js_in_worker", "halogen_worker_in_worker", Unsafe, [t|IO Bool|])
     , ("js_listen", "halogen_worker_listen", Unsafe, [t|Callback -> IO ()|])
     , ("js_request_id", "halogen_worker_request_id", Unsafe, [t|JSVal -> IO Int|])
     , ("js_request_job", "halogen_worker_request_job", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_request_args", "halogen_worker_request_args", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_reply", "halogen_worker_reply", Unsafe, [t|Int -> Bool -> JSText -> IO ()|])
     ]
 )

-- | A function the program offers to its workers, from @a@ to @b@, under a
-- name. The name is all that crosses to the worker, which finds the function
-- in its own copy of the table, so names must be unique in the program.
data Job a b = Job
  { name :: Text
  , call :: Text -> IO Text
  -- ^ The function as the worker runs it, from the arguments' JSON to the
  -- result's, throwing what the function throws.
  }

-- | A job under a name, of a function whose argument and result have JSON
-- instances (a tuple or a record for several arguments).
--
-- The function runs in 'IO', for a job that reads a clock or uses an
-- 'Data.IORef.IORef' of its own; a pure one is @\\x -> pure (f x)@. Its
-- result is evaluated completely, when it is encoded, before the job counts
-- as done, so a lazy error in it is the job's error.
job :: (FromJSON a, ToJSON b) => Text -> (a -> IO b) -> Job a b
job name function =
  Job
    { name
    , call = \arguments -> case eitherDecodeStrictText arguments of
        Left problem -> throwIO (BadArguments problem)
        Right argument -> encodeText <$> function argument
    }

-- | A job of any type, for the table.
data SomeJob = SomeJob Text (Text -> IO Text)

some :: Job a b -> SomeJob
some j = SomeJob j.name j.call

newtype BadArguments = BadArguments String
  deriving stock (Show)

instance Exception BadArguments where
  displayException (BadArguments problem) = "its arguments do not decode: " <> problem

-- | The table 'serve' was last given, which a job run in the program itself
-- is found in, as a worker would find it in its own.
table :: IORef (Maybe (Map Text SomeJob))
table = unsafePerformIO (newIORef Nothing)
{-# NOINLINE table #-}

-- | What a program's @main@ does first. In a worker a pool started, it
-- answers the pool's messages with the jobs given here and returns 'False':
-- @main@ should then do nothing else, since a worker has no page to mount
-- on. Anywhere else it returns 'True', and @main@ goes on.
--
-- The list is the program's table, the same in both roles: a job that is
-- not in it is an error, in a worker and in the program alike.
serve :: [SomeJob] -> IO Bool
serve jobs = do
  let jobTable = Map.fromList [(name, j) | j@(SomeJob name _) <- jobs]
  writeIORef table (Just jobTable)
  inWorker <- js_in_worker
  if not inWorker
    then pure True
    else do
      listener <- mkCallback $ \request -> do
        requestId <- js_request_id request
        name <- fromJSText <$> js_request_job request
        arguments <- fromJSText <$> js_request_args request
        answer jobTable name arguments >>= \case
          Left problem -> js_reply requestId False (toJSText problem)
          Right result -> js_reply requestId True (toJSText result)
      js_listen listener
      pure False

-- | 'serve' the jobs, then run the program's page side only where 'serve'
-- says to:
--
-- > main = Worker.program jobs $ runBrowserDOM $ do ...
program :: [SomeJob] -> IO () -> IO ()
program jobs onPage = do
  page <- serve jobs
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
-- 'WorkerError'; a later 'run' starts the workers again.
terminate :: (MonadIO m) => Pool -> m ()
terminate (Pool pool) = liftIO (js_terminate pool)

-- | Run a job in the next worker of the pool (they take turns), waiting for
-- its result. In a component this is a 'liftIO' (or @lift@), which runs on
-- a thread of its own while the tree goes on; the page's thread is free
-- meanwhile, since the work is in the worker's.
--
-- An exception in the job, arguments it cannot decode, a result the caller
-- cannot, a name not in the table, a worker that could not start, and a
-- pool terminated while the job ran are each thrown here as a
-- 'WorkerError'.
--
-- Killing the thread that waits (the component's fiber) stops the wait,
-- not the job: there is no cancelling a job in this version, and the worker
-- finishes it, its result unread. 'terminate' ends a worker and everything
-- in it.
--
-- Where 'available' is 'False' the job runs here, on the calling thread,
-- through the same JSON and the same table. In a component's 'liftIO' that
-- leaves the component's loop free, but not the page's thread, which a
-- computation holds until it is done: only a worker gives the page its
-- thread back.
run :: (ToJSON a, FromJSON b, MonadIO m) => Pool -> Job a b -> a -> m b
run (Pool pool) j argument = liftIO $ do
  let arguments = encodeText argument
  inWorker <- available
  outcome <-
    if inWorker
      then
        js_run pool (toJSText j.name) (toJSText arguments) >>= await >>= \case
          Left failure -> pure (Left failure.message)
          Right result -> pure (Right (jsValText result))
      else do
        jobTable <- fromMaybe (Map.singleton j.name (some j)) <$> readIORef table
        answer jobTable j.name arguments
  case outcome >>= decodeResult of
    Left message -> throwIO WorkerError {job = j.name, message}
    Right result -> pure result
  where
    decodeResult text = case eitherDecodeStrictText text of
      Left problem -> Left ("its result does not decode: " <> Text.pack problem)
      Right result -> Right result

-- | Whether 'run' would use a worker here: in a browser that has @Worker@,
-- whose loader said where the program is (@globalThis.halogenWorker@, for
-- this program's backend), once 'serve' has been called, and not inside a
-- worker. Otherwise jobs run in the program itself.
available :: (MonadIO m) => m Bool
available = liftIO $ do
  served <- readIORef table
  case served of
    Nothing -> pure False
    Just _ -> js_available arch

-- | A job that failed, by its name, and what went wrong.
data WorkerError = WorkerError
  { job :: Text
  , message :: Text
  }
  deriving stock (Eq, Show)

instance Exception WorkerError where
  displayException e = Text.unpack ("the job " <> e.job <> " failed: " <> e.message)

-- | A worker's answer to a request, and the program's own when it runs a
-- job itself: the result's JSON, or what went wrong. Only the function's
-- synchronous exceptions are caught; one thrown to the thread (it was
-- killed) goes on.
answer :: Map Text SomeJob -> Text -> Text -> IO (Either Text Text)
answer jobTable name arguments = case Map.lookup name jobTable of
  Nothing -> pure (Left ("there is no job named " <> name <> " in the program's table (the list given to serve)"))
  Just (SomeJob _ function) -> do
    outcome <- tryAny (function arguments >>= evaluate)
    pure $ case outcome of
      Left (SomeException e) -> Left (Text.pack (displayException e))
      Right result -> Right result

encodeText :: (ToJSON a) => a -> Text
encodeText = Lazy.toStrict . encodeToLazyText

-- | The backend, for the pool to know how to start a copy of the program.
arch :: JSText
arch = toJSText (Text.pack System.Info.arch)
