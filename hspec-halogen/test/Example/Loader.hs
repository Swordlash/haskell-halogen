-- | Loads something slowly: it shows that it is loading, then what it got,
-- which a forked action fetches (here, waits for) after it is initialised.
module Example.Loader (component, Input (..)) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)

data Input = Input
  { delayMs :: Int
  , result :: Text
  }

data State = Loading Input | Loaded Text

data Action = Initialize

component :: forall q o m. (MonadIO m) => H.Component q Input o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = pure . Loading
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render = \case
      Loading _ -> HH.p [HP.class_ (HH.ClassName "status")] [HH.text "Loading..."]
      Loaded result -> HH.p [HP.class_ (HH.ClassName "status")] [HH.text ("Loaded " <> result)]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction Initialize =
      get >>= \case
        Loading input -> void $ H.fork $ do
          liftIO $ threadDelay (input.delayMs * 1000)
          put (Loaded input.result)
        Loaded _ -> pass
