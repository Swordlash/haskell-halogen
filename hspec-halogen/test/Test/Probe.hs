{-# LANGUAGE TemplateHaskell #-}

-- | Core's activity probe in a real page: the page sets it up before a tree
-- is mounted, and core gives it a function that counts the mounted trees
-- and the fibers waiting in theirs. Other examples on the page leave trees
-- of their own, so these compare readings rather than read absolute
-- numbers.
module Test.Probe (spec) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText)
import Halogen.JSBits.Promise (Promise, wait)
import Protolude hiding (find, wait)
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

-- | Pauses with 'H.delay' in its @Initialize@ for as long as it is told,
-- then shows that it woke.
sleeper :: (Applicative m) => NominalDiffTime -> H.Component H.VoidF () Void m
sleeper duration =
  H.component
    (\() -> pure False)
    (\awake -> HH.span [HP.class_ (H.ClassName "sleeper")] [HH.text (if awake then "awake" else "asleep")])
    [H.onInitialize (H.delay duration >> put True)]

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
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

  -- A pause (H.delay) is the program's own business: the probe counts its
  -- fiber neither running nor waiting while it sleeps, so a driver that
  -- waits for a quiet page does not wait for it.
  it "counts a fiber pausing in delay as neither running nor waiting" $ runPage $ do
    unsafeIOToPageM js_probe_set
    plain <- mount m (idleOne :: H.Component H.VoidF () Void m) ()
    (trees0, _, waiting0) <- probe
    asleep <- mount m (sleeper 3600 :: H.Component H.VoidF () Void m) ()
    probe `shouldReturn` (trees0 + 1, 0, waiting0)
    find asleep ".sleeper" >>= (`shouldHaveText` "asleep")
    unmount asleep
    -- One that wakes renders what follows the pause.
    waking <- mount m (sleeper 0.05 :: H.Component H.VoidF () Void m) ()
    find waking ".sleeper" >>= (`shouldHaveText` "awake")
    eventually (probe `shouldReturn` (trees0 + 1, 0, waiting0))
    unmount waking
    unmount plain
