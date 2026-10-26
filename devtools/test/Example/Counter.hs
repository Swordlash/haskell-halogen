-- | A counter to look at: a button that counts its clicks, labelled
-- "Counter" in the trace, whose state can be shown.
module Example.Counter (component, State (..)) where

import Control.Monad.State.Class (modify)
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP

newtype State = State {count :: Int}
  deriving stock (Show)

data Action = Increment

component :: forall q i o m. (Monad m) => H.Component q i o m
component =
  H.named "Counter" $
    H.component (\_ -> pure State {count = 0}) render [H.onAction (\Increment -> modify (\s -> State {count = s.count + 1}))]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render s =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "increment"), HE.onClick (const Increment)] [HH.text "+"]
        , HH.span [HP.class_ (HH.ClassName "count")] [HH.text (T.pack (show s.count))]
        ]
