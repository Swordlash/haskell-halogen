-- | The demo library's component (demo/feature/Feature.hs), shown through
-- the wrapper as the tutorial's lesson shows it, with a loader that has it
-- at once, as the shared library's entry would hand it over.
module Test.Demo (spec) where

import Data.IORef
import Data.Row (type (.==))
import Feature (feature)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Lazy qualified as Lazy
import Halogen.Lazy.Demo qualified as Demo
import Protolude
import Test.Harness
import Test.Hspec (Spec, describe, it, shouldReturn)

data Ask a
  = WaveAtIt a
  | HowOften (Maybe Int -> a)

type Slots = ("feature" .== H.Slot Demo.Query Demo.Output ())

parent :: H.Component Ask () Demo.Output IO
parent = H.component (\_ -> pure ()) render [H.onAction H.raise, H.onQuery handleQuery]
  where
    render :: () -> H.ComponentHTML Demo.Output Slots IO
    render _ = HH.slot "feature" () (Lazy.lazy Lazy.defaultLoading (pure feature)) "Ada" identity
    handleQuery :: Ask a -> H.HalogenM () Demo.Output Slots Demo.Output IO (Maybe a)
    handleQuery = \case
      WaveAtIt next -> H.tell "feature" () Demo.Wave $> Just next
      HowOften reply -> Just . reply <$> H.request "feature" () Demo.GetWaves

spec :: Spec
spec = describe "the demo library's component, through the wrapper" $ do
  it "greets its input, waves when told to, raises it and says how often" $ do
    h <- start parent ()
    let greeting = lastMay <$> rendersOf "Hello" h
    eventually (Just "Hello, Ada! I was loaded from a library of my own.WaveWaved 0 times.") greeting
    query h (WaveAtIt ()) `shouldReturn` Just ()
    eventually (Just "Hello, Ada! I was loaded from a library of my own.WaveWaved 1 time.") greeting
    readIORef h.outputs `shouldReturn` [Demo.Waved 1]
    query h (HowOften identity) `shouldReturn` Just (Just 1)
    dispose h
