{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StaticPointers #-}

-- | Jobs run where no worker can be made: natively, and under Node on both
-- browser backends, where the page's configuration (and @Worker@) is
-- missing. That is the fallback, through the same bytes as across
-- @postMessage@: the argument encoded, the job's function run on its
-- bytes, the result's bytes decoded. And the worker's own side,
-- 'Internal.answer', which finds a job by its static key in the program's
-- static pointer table, is run here on requests made as a page makes them,
-- and on one no page would send. And stopping a job there, which in the
-- fallback is killing the thread 'Worker.start' forked for it.
--
-- The jobs are exported, as "Halogen.Worker"'s documentation advises: GHC
-- before 10.0 (issue #24773) can fail to link a module whose static binding
-- is not exported and is used more than once, as several of these are.
module Test.Run
  ( spec
  , double
  , flipTile
  , fails
  , lazyError
  , broken
  , where_
  , sleeper
  , spin
  , selfKilled
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException (..), throwIO)
import Control.Monad (forever)
import Data.Binary (Binary (..), encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import Halogen.JSBits.Promise (JSError (..), mapConcurrently, poll, resolved, wait, waitCatch)
import Halogen.Worker (Job, WorkerError (..))
import Halogen.Worker qualified as Worker
import Halogen.Worker.Internal qualified as Internal
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec

data Tile = Tile {x :: Int, y :: Int, rows :: [[Double]], label :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | A value whose bytes never decode, as a result whose type the page and
-- the worker disagree about would not.
data Broken = Broken
  deriving stock (Eq, Show)

instance Binary Broken where
  put Broken = pure ()
  get = fail "a Broken never decodes"

double :: StaticPtr (Job Int Int)
double = static (Worker.job (\n -> pure (n * 2)))

flipTile :: StaticPtr (Job Tile Tile)
flipTile = static (Worker.job (\t -> pure t {x = t.y, y = t.x, rows = reverse t.rows}))

fails :: StaticPtr (Job Int Int)
fails = static (Worker.job (\_ -> throwIO (userError "no luck")))

lazyError :: StaticPtr (Job Int Int)
lazyError = static (Worker.job (\n -> pure (if n > 0 then error "found too late" else n)))

broken :: StaticPtr (Job () Broken)
broken = static (Worker.job (\() -> pure Broken))

-- A job that ends its thread with an exception 'runCall' does not catch:
-- ThreadKilled counts as asynchronous, whoever throws it.
selfKilled :: StaticPtr (Job () Int)
selfKilled = static (Worker.job (\() -> throwIO ThreadKilled))

where_ :: StaticPtr (Job () (Maybe Int))
where_ = static (Worker.job (\() -> Worker.whichWorker))

-- | How many of 'sleeper''s sleeps have ended, and how many turns 'spin''s
-- loop has made: what a test reads to see that a stopped job stopped.
sleeps, spins :: IORef Int
sleeps = unsafePerformIO (newIORef 0)
{-# NOINLINE sleeps #-}
spins = unsafePerformIO (newIORef 0)
{-# NOINLINE spins #-}

bump :: IORef Int -> IO ()
bump ref = atomicModifyIORef' ref (\n -> (n + 1, ()))

-- | Sleeps for this many milliseconds, then counts the sleep.
sleeper :: StaticPtr (Job Int ())
sleeper = static (Worker.job (\ms -> threadDelay (ms * 1000) >> bump sleeps))

-- | Counts its turns for ever, a checkpoint after each.
spin :: StaticPtr (Job () ())
spin = static (Worker.job (\() -> forever (bump spins >> Worker.checkpoint)))

-- | Whether the counter has stopped: what it reads now, and again a while
-- later.
stopped :: IORef Int -> IO Bool
stopped ref = do
  threadDelay 20000
  earlier <- readIORef ref
  threadDelay 100000
  (== earlier) <$> readIORef ref

rejectedAs :: Text -> Either JSError a -> Expectation
rejectedAs name = \case
  Left e -> e.name `shouldBe` name
  Right _ -> expectationFailure ("settled rather than rejected as " <> Text.unpack name)

-- | A request for one call, as the page makes it.
request :: StaticPtr (Job a b) -> a -> ByteString
request pointer argument =
  Internal.encodeRequest
    Internal.Request {key = staticKey pointer, arguments = [(deRefStaticPtr pointer).encodeArgument argument]}

failsWith :: Text -> Selector WorkerError
failsWith fragment e = "Test.Run:" `Text.isPrefixOf` e.job && fragment `Text.isInfixOf` e.message

spec :: Spec
spec = describe "Halogen.Worker" $ do
  it "serves on the page: serve returns True outside a worker" $
    Worker.serve `shouldReturn` True

  it "runs the page's side of program, which serve says to" $ do
    ran <- newIORef False
    Worker.program (writeIORef ran True)
    readIORef ran `shouldReturn` True

  it "has no worker without the page's configuration, and is in none" $ do
    _ <- Worker.serve
    Worker.available `shouldReturn` False
    Worker.whichWorker `shouldReturn` Nothing

  it "runs a job in the program, through its bytes" $ do
    pool <- Worker.newPool 2
    Worker.run pool double 21 `shouldReturn` 42
    let tile = Tile {x = 1, y = 2, rows = [[0.5, 1], [2]], label = "a \"quoted\" ünïcode label"}
    Worker.run pool flipTile tile `shouldReturn` Tile {x = 2, y = 1, rows = [[2], [0.5, 1]], label = tile.label}
    Worker.run pool where_ () `shouldReturn` Nothing

  it "gives a job's exception back as a WorkerError naming where its static is" $ do
    pool <- Worker.newPool 1
    Worker.run pool fails 1 `shouldThrow` failsWith "no luck"

  it "settles the promise of a job whose thread ends by an exception it does not catch" $ do
    pool <- Worker.newPool 1
    Worker.run pool selfKilled () `shouldThrow` failsWith "thread killed"
    p <- Worker.start pool selfKilled ()
    outcome <- waitCatch p
    either (.name) (const "resolved") outcome `shouldBe` "WorkerError"

  it "counts an error in the result, found as it is encoded, as the job's" $ do
    pool <- Worker.newPool 1
    Worker.run pool lazyError 1 `shouldThrow` failsWith "found too late"
    Worker.run pool lazyError 0 `shouldReturn` 0

  it "says when the result does not decode" $ do
    pool <- Worker.newPool 1
    Worker.run pool broken () `shouldThrow` failsWith "its result does not decode: a Broken never decodes"

  it "gives a promise from start, rejected with a JSError named WorkerError" $ do
    pool <- Worker.newPool 1
    (wait =<< Worker.start pool double 4) `shouldReturn` 8
    failing <- Worker.start pool fails 1
    _ <- waitCatch failing
    poll failing >>= \case
      Just (Left e) -> do
        e.name `shouldBe` "WorkerError"
        Text.unpack e.message `shouldStartWith` "the job at Test.Run:"
        Text.unpack e.message `shouldEndWith` "failed: user error (no luck)"
      other -> expectationFailure ("a rejection, not " <> show other)

  it "maps a job over a list with mapConcurrently over start" $ do
    pool <- Worker.newPool 2
    mapConcurrently (Worker.start pool double) [1 .. 10] `shouldReturn` map (* 2) [1 .. 10]

  it "answers a request by the job's key, as a worker does" $ do
    let replied bytes = Internal.answer bytes >>= either (\problem -> expectationFailure (Text.unpack problem) >> pure Nothing) (pure . either (const Nothing) Just . Internal.decodeReply)
    replied (request double 21) `shouldReturn` Just Internal.Reply {results = [Lazy.toStrict (encode (42 :: Int))], failure = Nothing}
    replied (request fails 1) `shouldReturn` Just Internal.Reply {results = [], failure = Just "user error (no luck)"}

  it "answers a batch up to its first failing call, and says why that one failed" $ do
    let arguments = map (Lazy.toStrict . encode @Int) [1, 2, 0, 3]
        batch = Internal.encodeRequest Internal.Request {key = staticKey lazyError, arguments}
    reply <- Internal.answer batch
    (Internal.decodeReply <$> reply) `shouldSatisfy` \case
      Right (Right Internal.Reply {results = [], failure = Just problem}) -> "found too late" `Text.isInfixOf` problem
      _ -> False
    let fine = Internal.encodeRequest Internal.Request {key = staticKey double, arguments}
    Internal.answer fine >>= \case
      Right bytes -> Internal.decodeReply bytes `shouldBe` Right Internal.Reply {results = map (Lazy.toStrict . encode @Int) [2, 4, 0, 6], failure = Nothing}
      Left problem -> expectationFailure (Text.unpack problem)

  it "says when a request's key is not in the program's static pointer table" $ do
    let made = Internal.encodeRequest Internal.Request {key = Fingerprint 1 2, arguments = [Lazy.toStrict (encode (1 :: Int))]}
    Internal.answer made >>= \case
      Left problem -> Text.unpack problem `shouldContain` "there is no job with the key"
      Right _ -> expectationFailure "a key the table does not have was answered"

  it "says when a job's argument does not decode" $ do
    let wrong = Internal.encodeRequest Internal.Request {key = staticKey double, arguments = ["\1"]}
    (fmap Internal.decodeReply <$> Internal.answer wrong) >>= \case
      Right (Right Internal.Reply {results = [], failure = Just problem}) -> Text.unpack problem `shouldContain` "its argument does not decode"
      other -> expectationFailure ("an argument that does not decode was answered: " <> show other)

  it "runs the job on a thread of its own: start returns before the job ends" $ do
    pool <- Worker.newPool 1
    earlier <- readIORef sleeps
    p <- Worker.start pool sleeper 100
    poll p >>= (`shouldSatisfy` (\case Nothing -> True; Just _ -> False))
    wait p
    readIORef sleeps `shouldReturn` earlier + 1

  it "cancels a job while it sleeps, which then never ends its sleep" $ do
    pool <- Worker.newPool 1
    earlier <- readIORef sleeps
    p <- Worker.start pool sleeper 200
    Worker.cancel pool p
    waitCatch p >>= rejectedAs "Cancelled"
    threadDelay 400000
    readIORef sleeps `shouldReturn` earlier

  it "cancels the job of a run whose wait a timeout interrupts" $ do
    pool <- Worker.newPool 1
    earlier <- readIORef sleeps
    timeout 20000 (Worker.run pool sleeper 200) `shouldReturn` Nothing
    threadDelay 400000
    readIORef sleeps `shouldReturn` earlier

  it "stops a loop that checkpoints, by cancelling or killing it" $ do
    pool <- Worker.newPool 1
    p <- Worker.start pool spin ()
    threadDelay 20000
    Worker.cancel pool p
    waitCatch p >>= rejectedAs "Cancelled"
    stopped spins `shouldReturn` True
    q <- Worker.start pool spin ()
    threadDelay 20000
    Worker.kill pool q
    waitCatch q >>= rejectedAs "Killed"
    stopped spins `shouldReturn` True

  it "names the job in a cancelled promise's rejection, and cancels only once" $ do
    pool <- Worker.newPool 1
    p <- Worker.start pool sleeper 100
    Worker.cancel pool p
    Worker.cancel pool p
    (wait p >> pure ()) `shouldThrow` (\(e :: JSError) -> e.name == "Cancelled" && "the job at Test.Run:" `Text.isPrefixOf` e.message)

  it "leaves alone a promise the pool did not start, or one that has settled" $ do
    pool <- Worker.newPool 1
    other <- Worker.newPool 1
    Worker.cancel pool (resolved (3 :: Int))
    Worker.kill pool (resolved (3 :: Int))
    p <- Worker.start pool double 2
    _ <- wait p
    Worker.cancel pool p
    Worker.kill pool p
    waitCatch p `shouldReturn` Right 4
    q <- Worker.start other sleeper 50
    Worker.cancel pool q
    waitCatch q `shouldReturn` Right ()

  it "ends the jobs it runs here when a pool is terminated" $ do
    pool <- Worker.newPool 1
    p <- Worker.start pool spin ()
    threadDelay 10000
    Worker.terminate pool
    waitCatch p >>= \case
      Left e -> Text.unpack e.message `shouldEndWith` "the pool was terminated"
      Right () -> expectationFailure "the job outlived the pool"
    stopped spins `shouldReturn` True

  it "has nothing to end when a pool is terminated, and runs after it" $ do
    pool <- Worker.newPool 1
    Worker.terminate pool
    Worker.run pool double 2 `shouldReturn` 4
