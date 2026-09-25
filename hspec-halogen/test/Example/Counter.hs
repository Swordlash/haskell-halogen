-- | The README's counter: a button that counts its clicks, shows the count,
-- raises each new one to its parent, and tells it when asked.
module Example.Counter (component, Query (..)) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude

newtype Query a = GetCount (Int -> a)

data Action = Increment

component :: forall i m. (Monad m) => H.Component Query i Int m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure 0)
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.handleQuery = handleQuery}
      }
  where
    render :: Int -> H.ComponentHTML Action Empty m
    render count =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "increment"), HE.onClick (const Increment)] [HH.text "+"]
        , HH.span [HP.class_ (HH.ClassName "count")] [HH.text (show count)]
        ]

    handleAction :: Action -> H.HalogenM Int Action Empty Int m ()
    handleAction Increment = do
      modify (+ 1)
      get >>= H.raise

    handleQuery :: Query a -> H.HalogenM Int Action Empty Int m (Maybe a)
    handleQuery (GetCount reply) = Just . reply <$> get
