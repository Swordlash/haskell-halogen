module Halogen.Material.RadioButton
  ( radio
  , RadioButtonCfg (..)
  , emptyRadioButtonCfg
  , RadioChange (..)
  )
where

import Clay (Css)
import Control.Monad.Extra
import Control.Monad.UUID
import DOM.HTML.Indexed
import Data.UUID.Types as UUID
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Monad
import Protolude

data RadioButtonCfg = RadioButtonCfg
  { label :: Text
  , enabled :: Bool
  , extraStyle :: Css
  , groupName :: Text
  , checked :: Bool
  }

emptyRadioButtonCfg :: RadioButtonCfg
emptyRadioButtonCfg =
  RadioButtonCfg
    { label = ""
    , enabled = True
    , extraStyle = mempty
    , groupName = ""
    , checked = False
    }

data RadioButtonState = RadioButtonState
  { mdcFormField :: Maybe MDCFormField
  , label :: Text
  , enabled :: Bool
  , extraStyle :: Css
  , groupName :: Text
  , checked :: Bool
  , id :: UUID
  }

data RadioButtonAction
  = Initialize
  | Finalize
  | Change

data RadioChange = RadioChange Text Bool

radio :: (MonadMaterial m, MonadUUID m) => H.Component q RadioButtonCfg RadioChange m
radio =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \RadioButtonCfg {..} -> do
          id <- generateV4
          pure $ RadioButtonState {mdcFormField = Nothing, ..}
      , render
      , eval = H.mkEval $ H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize, H.finalize = Just Finalize}
      }
  where
    ref = H.RefLabel "radio"

    render RadioButtonState {..} =
      HH.div
        [HP.class_ (HH.ClassName "mdc-form-field")]
        [ HH.div
            [HP.classes $ HH.ClassName "mdc-radio" : pureIf (not enabled) (HH.ClassName "mdc-radio--disabled"), HP.ref ref, HP.style extraStyle]
            [ HH.input
                [ HP.type_ InputRadio
                , HP.name groupName
                , HP.id (UUID.toText id)
                , HP.class_ (HH.ClassName "mdc-radio__native-control")
                , HP.checked checked
                , HP.disabled (not enabled)
                , HE.onChange (const Change)
                ]
            , HH.div
                [HP.class_ $ HH.ClassName "mdc-radio__background"]
                [ HH.div [HP.class_ $ HH.ClassName "mdc-radio__outer-circle"] []
                , HH.div [HP.class_ $ HH.ClassName "mdc-radio__inner-circle"] []
                ]
            , HH.div [HP.class_ $ HH.ClassName "mdc-radio__ripple"] []
            ]
        , HH.label [HP.for (UUID.toText id)] [HH.text label]
        ]

    handleAction = \case
      Initialize -> do
        H.getHTMLElementRef ref >>= \case
          Just el -> do
            mdcFormField <- lift $ initRadioButton el
            modify $ \s -> s {mdcFormField = Just mdcFormField}
          Nothing -> panic "Cannot initialize radion button!"
      Finalize -> traverse_ (lift . destroyRadioButton) =<< gets (.mdcFormField)
      Change -> do
        lab <- gets (.label)
        state (\s -> (not s.checked, s {checked = not s.checked} :: RadioButtonState)) >>= H.raise . RadioChange lab
