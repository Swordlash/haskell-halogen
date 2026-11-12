{-# LANGUAGE TemplateHaskell #-}

-- | Core's activity probe in a real page: the page sets it up before a tree
-- is mounted, and core gives it a function that counts the mounted trees
-- and the fibers waiting in theirs. Other examples on the page leave trees
-- of their own, so these compare readings rather than read absolute
-- numbers.
module Test.Probe (spec) where

import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText)
import Halogen.JSBits.Promise (Promise, wait)
import Protolude hiding (wait)
import Test.Hspec.Halogen

$( browserJS
     ["test/jsbits/probe.js"]
     [ ("js_probe_set", "halogen_probe_set", Safe, [t|IO ()|])
     , ("js_probe_ask", "halogen_probe_ask", Safe, [t|IO (Promise JSText)|])
     ]
 )

-- | Trees, running, waiting.
probe :: PageM s (Int, Int, Int)
probe = unsafeIOToPageM $ do
  answer <- fromJSText <$> (wait =<< js_probe_ask)
  case mapM (readMaybe . T.unpack) (T.words answer) of
    Just [t, r, w] -> pure (t, r, w)
    _ -> panic ("the probe answered " <> answer)

-- | Waits in 'liftIO' in its @Initialize@, signalling once it does, until
-- let go.
waiter :: (MonadIO m) => MVar () -> MVar () -> H.Component H.VoidF () Void m
waiter started gate =
  H.component
    (\() -> pure ())
    (\() -> HH.text "waiting")
    [H.onInitialize (liftIO (putMVar started () >> takeMVar gate))]

idleOne :: (Applicative m) => H.Component H.VoidF () Void m
idleOne = H.component (\() -> pure ()) (\() -> HH.text "idle") []

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m =
  it "counts what is mounted and what waits, for a page that asked" $ runPage $ do
    unsafeIOToPageM js_probe_set
    -- The first tree mounted once the probe is there gives it its function.
    plain <- mount m (idleOne :: H.Component H.VoidF () Void m) ()
    (trees0, running0, waiting0) <- probe
    running0 `shouldBe` 0
    started <- unsafeIOToPageM newEmptyMVar
    gate <- unsafeIOToPageM newEmptyMVar
    held <- mount m (waiter started gate :: H.Component H.VoidF () Void m) ()
    unsafeIOToPageM (takeMVar started)
    probe `shouldReturn` (trees0 + 1, 0, waiting0 + 1)
    unsafeIOToPageM (putMVar gate ())
    eventually (probe `shouldReturn` (trees0 + 1, 0, waiting0))
    unmount held
    probe `shouldReturn` (trees0, 0, waiting0)
    unmount plain
    probe `shouldReturn` (trees0 - 1, 0, waiting0)
