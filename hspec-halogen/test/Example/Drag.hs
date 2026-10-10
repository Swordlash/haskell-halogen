-- | Pieces to sort into two bins, by dragging: with HTML5 drag and drop,
-- or with pointer events, as a page that follows the pointer itself does
-- (the only kind of drag a touch screen has).
module Example.Drag (component, Query (..), Kind (..)) where

import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent qualified as Drag

-- | Where each piece is: in a bin, or in the tray when it is not here.
newtype Query a = GetBins (Map Text Text -> a)

-- | Which events the pieces and bins listen to.
data Kind = Html5 | Pointer

data State = State {kind :: Kind, bins :: Map Text Text, held :: Maybe Text}

data Action = Pick Text | Over Drag.DragEvent | Drop Text | Grab Text | Release Text

pieces :: [Text]
pieces = ["a", "b", "c"]

component :: forall o m. (MonadIO m) => H.Component Query Kind o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \kind -> pure (State kind Map.empty Nothing)
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.handleQuery = handleQuery}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      -- A drag would otherwise select the text it passes over, and the next
      -- press on the selection drag the text instead, cancelling the pointer.
      HH.div [HP.styleText "user-select: none"]
        [ HH.div [HP.class_ (HH.ClassName "tray")] [piece st.kind p | p <- pieces, not (Map.member p st.bins)]
        , HH.div_ [bin b st | b <- ["left", "right"]]
        ]

    piece kind p =
      HH.span
        ( HP.class_ (HH.ClassName ("piece piece-" <> p)) : case kind of
            Html5 -> [HP.draggable True, HE.onDragStart (const (Pick p))]
            Pointer -> [HE.onPointerDown (const (Grab p))]
        )
        [HH.text p]

    bin b st =
      HH.div
        ( HP.class_ (HH.ClassName ("bin bin-" <> b)) : case st.kind of
            Html5 -> [HE.onDragOver Over, HE.onDrop (const (Drop b))]
            Pointer -> [HE.onPointerUp (const (Release b))]
        )
        [HH.text (b <> ": " <> mconcat [p | (p, b') <- Map.toList st.bins, b' == b])]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Pick p -> modify $ \s -> s {held = Just p} :: State
      -- Without it the bin refuses the drop.
      Over event -> H.liftEffect (preventDefault (Drag.toEvent event))
      Drop b -> place b
      Grab p -> modify $ \s -> s {held = Just p} :: State
      Release b -> place b

    place b = modify $ \s -> case s.held of
      Just p -> s {bins = Map.insert p b s.bins, held = Nothing} :: State
      Nothing -> s

    handleQuery :: Query a -> H.HalogenM State Action Empty o m (Maybe a)
    handleQuery (GetBins reply) = Just . reply <$> gets (.bins)
