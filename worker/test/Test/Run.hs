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
-- and on one no page would send.
--
-- The jobs are exported, as "Halogen.Worker"'s documentation advises: with
-- optimisation GHC 9.14 can rename a static pointer's binding that its
-- module keeps to itself while still naming the old binding in the
-- module's static pointer table, which fails to link.
module Test.Run
  ( spec
  , double
  , flipTile
  , fails
  , lazyError
  , broken
  , where_
  )
where

import Control.Exception (throwIO)
import Data.Binary (Binary (..), encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import Halogen.JSBits.Promise (JSError (..), mapConcurrently, poll)
import Halogen.Worker (Job, WorkerError (..))
import Halogen.Worker qualified as Worker
import Halogen.Worker.Internal qualified as Internal
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

where_ :: StaticPtr (Job () (Maybe Int))
where_ = static (Worker.job (\() -> Worker.whichWorker))

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

  it "counts an error in the result, found as it is encoded, as the job's" $ do
    pool <- Worker.newPool 1
    Worker.run pool lazyError 1 `shouldThrow` failsWith "found too late"
    Worker.run pool lazyError 0 `shouldReturn` 0

  it "says when the result does not decode" $ do
    pool <- Worker.newPool 1
    Worker.run pool broken () `shouldThrow` failsWith "its result does not decode: a Broken never decodes"

  it "gives a settled promise from start, rejected with a JSError named WorkerError" $ do
    pool <- Worker.newPool 1
    (poll =<< Worker.start pool double 4) `shouldReturn` Just (Right 8)
    (poll =<< Worker.start pool fails 1) >>= \case
      Just (Left e) -> do
        e.name `shouldBe` "WorkerError"
        Text.unpack e.message `shouldStartWith` "the job at Test.Run:"
        Text.unpack e.message `shouldEndWith` "failed: user error (no luck)"
      other -> expectationFailure ("a rejection, not " <> show other)

  it "maps a job over a list with mapConcurrently over start" $ do
    pool <- Worker.newPool 2
    mapConcurrently (Worker.start pool double) [1 .. 10] `shouldReturn` map (* 2) [1 .. 10]

  it "answers a request by the job's key, as a worker does" $ do
    Internal.answer (request double 21) >>= \case
      Right reply -> Internal.decodeReply reply `shouldBe` Right [Lazy.toStrict (encode (42 :: Int))]
      Left problem -> expectationFailure (Text.unpack problem)
    Internal.answer (request fails 1) `shouldReturn` Left "user error (no luck)"

  it "says when a request's key is not in the program's static pointer table" $ do
    let made = Internal.encodeRequest Internal.Request {key = Fingerprint 1 2, arguments = [Lazy.toStrict (encode (1 :: Int))]}
    Internal.answer made >>= \case
      Left problem -> Text.unpack problem `shouldContain` "there is no job with the key"
      Right _ -> expectationFailure "a key the table does not have was answered"

  it "says when a job's argument does not decode" $ do
    let wrong = Internal.encodeRequest Internal.Request {key = staticKey double, arguments = ["\1"]}
    Internal.answer wrong >>= \case
      Left problem -> Text.unpack problem `shouldContain` "its argument does not decode"
      Right _ -> expectationFailure "an argument that does not decode was answered"

  it "has nothing to end when a pool is terminated, and runs after it" $ do
    pool <- Worker.newPool 1
    Worker.terminate pool
    Worker.run pool double 2 `shouldReturn` 4
