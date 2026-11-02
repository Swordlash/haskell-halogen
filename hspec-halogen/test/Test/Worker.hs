{-# LANGUAGE TemplateHaskell #-}

-- | Examples marked 'inWorker': with @HSPEC_HALOGEN_WORKERS@ the runner
-- gives them to Web Workers, each a copy of the suite with no page, and
-- without it they run on a page like any other. Each says which it is, so
-- that the suite checks the runner sent it where it should have.
module Test.Worker (spec) where

import Data.Text qualified as T
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, fromJSText, jsValText, toJSText)
import Halogen.JSBits.Promise (Promise, waitCatch)
import Protolude hiding (waitCatch)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen (Spec, describe, inWorker, it, runPage)

$( browserJS
     ["test/jsbits/worker.js"]
     [ ("js_shard", "halogen_worker_test_shard", Safe, [t|IO JSText|])
     , ("js_workers", "halogen_worker_test_workers", Safe, [t|IO Int|])
     , ("js_has_document", "halogen_worker_test_has_document", Safe, [t|IO Bool|])
     , ("js_later", "halogen_worker_test_later", Safe, [t|Int -> JSText -> IO (Promise JSVal)|])
     ]
 )

-- | Whether this copy of the suite is a worker's.
inAWorker :: IO Bool
inAWorker = T.isPrefixOf "worker/" . fromJSText <$> js_shard

spec :: Spec
spec = inWorker $ do
  it "checks a pure expectation" $
    fibonacci 25 `Hspec.shouldBe` (75025 :: Integer)

  it "computes in IO, on threads of its own" $ do
    results <- liftIO $ do
      vars <- forM [1 .. 4 :: Integer] $ \n -> do
        var <- newEmptyMVar
        _ <- forkIO $ putMVar var $! sum [1 .. n * 100000]
        pure var
      traverse takeMVar vars
    results `Hspec.shouldBe` [n * 100000 * (n * 100000 + 1) `div` 2 | n <- [1 .. 4]]

  it "waits for a promise" $ do
    reply <- js_later 50 (toJSText ("later" :: Text)) >>= waitCatch
    either (Left . show) (Right . jsValText) reply `Hspec.shouldBe` (Right "later" :: Either Text Text)

  it "runs where the runner sent it" $ do
    workers <- js_workers
    worker <- inAWorker
    document <- js_has_document
    -- With workers it is in one of them, which has no document; without, on
    -- a page, which has.
    (worker, document) `Hspec.shouldBe` (workers > 0, workers == 0)

  describe "runPage" $
    it "fails in a worker, which has no page, and saying so" $ do
      worker <- inAWorker
      result <- try (runPage (pure ()))
      case result of
        Right ()
          | worker -> Hspec.expectationFailure "runPage ran in a worker"
          | otherwise -> pass
        Left (HUnitFailure _ reason)
          | worker -> reasonText reason `Hspec.shouldBe` "hspec-halogen: this example runs in a worker, which has no page: take it out of inWorker"
          | otherwise -> Hspec.expectationFailure ("runPage failed on a page: " <> toS (reasonText reason))
  where
    fibonacci :: Int -> Integer
    fibonacci n = fst (foldl' (\(a, b) _ -> (b, a + b)) (0, 1) [1 .. n])
    reasonText = \case
      Reason message -> toS message :: Text
      other -> show other
