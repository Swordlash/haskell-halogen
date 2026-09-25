-- | An input that keeps only the digits typed into it, and raises them.
--
-- The component renders its own value, and whatever else the user types is
-- taken back out of the input: a render compares an input's value with what
-- the element holds, not with what was rendered last, so an edit the
-- component turns down does not stay on the screen.
module Example.Digits (component) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)

-- A record rather than the bare text, so that every edit is a new model and
-- renders, even one that leaves the digits as they were.
newtype State = State {digits :: Text}

newtype Action = Edit Text

component :: forall q i m. (Monad m) => H.Component q i Text m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure (State ""))
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render model = HH.input [HP.class_ (HH.ClassName "digits"), HP.value model.digits, HE.onValueInput Edit]

    handleAction :: Action -> H.HalogenM State Action Empty Text m ()
    handleAction (Edit typed) = do
      let digits = T.filter isDigit typed
      put (State digits)
      H.raise digits
