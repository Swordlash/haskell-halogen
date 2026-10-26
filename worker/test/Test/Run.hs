{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Jobs run where no worker can be made: natively, and under Node on both
-- browser backends, where the page's configuration (and @Worker@) is
-- missing. That is the fallback, and it is the protocol too: 'Worker.run'
-- encodes the arguments to JSON text, the job is found in the table by its
-- name and answers with its result's JSON or what went wrong, and the
-- caller decodes that, exactly as across @postMessage@, only without a
-- worker in between.
--
-- 'Worker.serve' keeps the table it was given for the whole program, so
-- each test that depends on it serves its own first.
module Test.Run (spec) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Halogen.Worker (Job, WorkerError (..))
import Halogen.Worker qualified as Worker
import Test.Hspec

data Tile = Tile {x :: Int, y :: Int, rows :: [[Double]], label :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

double :: Job Int Int
double = Worker.job "double" (\n -> pure (n * 2))

flipTile :: Job Tile Tile
flipTile = Worker.job "flip" (\t -> pure t {x = t.y, y = t.x, rows = reverse t.rows})

fails :: Job Int Int
fails = Worker.job "fails" (\_ -> throwIO (userError "no luck"))

lazyError :: Job Int Int
lazyError = Worker.job "lazy" (\n -> pure (if n > 0 then error "found too late" else n))

-- The same name as double's, but with arguments of another type: what a
-- page gets when its job and the worker's disagree.
doubleText :: Job Text Int
doubleText = Worker.job "double" (\_ -> pure 0)

-- And one whose result the caller reads as another type.
doubleAsText :: Job Int Text
doubleAsText = Worker.job "double" (\_ -> pure "")

missing :: Job Int Int
missing = Worker.job "missing" pure

table :: [Worker.SomeJob]
table = [Worker.some double, Worker.some flipTile, Worker.some fails, Worker.some lazyError]

failsWith :: Text -> Text -> Selector WorkerError
failsWith name fragment e = e.job == name && fragment `Text.isInfixOf` e.message

spec :: Spec
spec = describe "Halogen.Worker" $ do
  it "serves on the page: serve returns True outside a worker" $
    Worker.serve table `shouldReturn` True

  it "runs the page's side of program, which serve says to" $ do
    ran <- newIORef False
    Worker.program table (modifyIORef' ran (const True))
    readIORef ran `shouldReturn` True

  it "has no worker without the page's configuration" $ do
    _ <- Worker.serve table
    Worker.available `shouldReturn` False

  it "runs a job in the program, through its JSON" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 2
    Worker.run pool double 21 `shouldReturn` 42
    let tile = Tile {x = 1, y = 2, rows = [[0.5, 1], [2]], label = "a \"quoted\" ünïcode label"}
    Worker.run pool flipTile tile `shouldReturn` Tile {x = 2, y = 1, rows = [[2], [0.5, 1]], label = tile.label}

  it "runs the function in the calling program" $ do
    seen <- newIORef []
    let note = Worker.job "note" (\n -> modifyIORef' seen (n :) >> pure n) :: Job Int Int
    _ <- Worker.serve [Worker.some note]
    pool <- Worker.newPool 1
    Worker.run pool note 7 `shouldReturn` 7
    readIORef seen `shouldReturn` [7]

  it "gives a job's exception back as a WorkerError with its message" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 1
    Worker.run pool fails 1 `shouldThrow` failsWith "fails" "no luck"

  it "counts an error in the result, found as it is encoded, as the job's" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 1
    Worker.run pool lazyError 1 `shouldThrow` failsWith "lazy" "found too late"
    Worker.run pool lazyError 0 `shouldReturn` 0

  it "runs the job the table has under the name, and says when its arguments do not decode" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 1
    Worker.run pool doubleText "21" `shouldThrow` failsWith "double" "arguments do not decode"

  it "says when the result does not decode" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 1
    Worker.run pool doubleAsText 21 `shouldThrow` failsWith "double" "result does not decode"

  it "fails for a job that is not in the table" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 1
    Worker.run pool missing 1 `shouldThrow` failsWith "missing" "no job named missing"

  it "has nothing to end when a pool is terminated, and runs after it" $ do
    _ <- Worker.serve table
    pool <- Worker.newPool 1
    Worker.terminate pool
    Worker.run pool double 2 `shouldReturn` 4
