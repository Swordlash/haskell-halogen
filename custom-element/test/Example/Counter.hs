-- | A counter with a label, as a custom element: the label is its input,
-- from the element's @label@ property or attribute; it raises every new
-- count, which the element dispatches as a @count@ event; and its queries
-- are the element's methods. It bumps the reference it is given when it is
-- finalized, at once ('H.liftEffect'), so that a test can tell.
module Example.Counter
  ( counter
  , Query (..)
  , definition
  , other
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (gets, modify, put)
import Data.Aeson (toJSON)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Maybe (fromMaybe)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen qualified as H
import Halogen.CustomElement (Definition (..))
import Halogen.CustomElement qualified as CE
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP

data Query a
  = GetCount (Int -> a)
  | Reset a
  | Add Int a
  | -- | A request the counter does not answer.
    Refuse (Int -> a)

data State = State {label :: Text, count :: Int}

data Action = Increment

counter :: forall m. (MonadIO m) => IORef Int -> H.Component Query Text Int m
counter finalized =
  H.component
    (\label -> pure State {label, count = 0})
    render
    [ H.onAction handleAction
    , H.onQuery handleQuery
    , H.onReceive (\label -> modify (\st -> st {label}))
    , H.onFinalize (H.liftEffect (liftIO (atomicModifyIORef' finalized (\n -> (n + 1, ())))))
    ]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "label")] [HH.text st.label]
        , HH.button [HP.class_ (HH.ClassName "increment"), HE.onClick (const Increment)] [HH.text "+"]
        , HH.span [HP.class_ (HH.ClassName "count")] [HH.text (Text.pack (show st.count))]
        ]

    handleAction :: Action -> H.HalogenM State Action Empty Int m ()
    handleAction Increment = do
      modify (\st -> st {count = st.count + 1})
      gets (.count) >>= H.raise

    handleQuery :: Query a -> H.HalogenM State Action Empty Int m (Maybe a)
    handleQuery = \case
      GetCount reply -> Just . reply <$> gets (.count)
      Reset next -> Just next <$ modify (\st -> st {count = 0})
      Add n next -> Just next <$ modify (\st -> st {count = st.count + n})
      Refuse _ -> pure Nothing

-- | The counter's element, rendered into the element itself, so that the
-- runner's selectors and clicks reach it.
definition :: Definition Query Text Int
definition =
  CE.defaultDefinition
    { input = \props -> fromMaybe "none" (CE.prop "label" props <|> CE.attr "label" props)
    , observed = ["label"]
    , output = \n -> Just ("count", toJSON n)
    , methods =
        [ CE.method "count" (\() -> GetCount)
        , CE.tellMethod "reset" (\() -> Reset)
        , CE.tellMethod "add" Add
        , CE.method "refuse" (\() -> Refuse)
        ]
    , shadow = CE.NoShadow
    }

-- | Another component for the same element: what a program run again
-- defines in the counter's place. It finalizes into the same reference.
other :: forall q m. (MonadIO m) => IORef Int -> H.Component q Text Int m
other finalized =
  H.component
    pure
    (\label -> HH.p [HP.class_ (HH.ClassName "other")] [HH.text ("other: " <> label)])
    [ H.onReceive put
    , H.onFinalize (H.liftEffect (liftIO (atomicModifyIORef' finalized (\n -> (n + 1, ())))))
    ]
