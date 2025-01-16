module Halogen.Material.Checkbox
  ( CheckboxSpec (..)
  , emptyCheckboxSpec
  , CheckboxChange (..)
  , CheckboxQuery (..)
  , checkbox
  )
where

import Clay (Css, transparent)
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
import Halogen.Svg.Attributes qualified as HPS
import Halogen.Svg.Elements qualified as HHS
import Protolude

data CheckboxSpec = CheckboxSpec
  { label :: Text
  , enabled :: Bool
  , extraStyle :: Css
  , checked :: Bool
  }

emptyCheckboxSpec :: CheckboxSpec
emptyCheckboxSpec =
  CheckboxSpec
    { label = ""
    , enabled = True
    , extraStyle = mempty
    , checked = False
    }

data CheckboxState = CheckboxState
  { mdcFormField :: Maybe MDCFormField
  , label :: Text
  , enabled :: Bool
  , extraStyle :: Css
  , checked :: Bool
  , id :: UUID
  }

data CheckboxAction
  = Initialize
  | Finalize
  | Checked Bool

data CheckboxChange = CheckboxChange Bool

data CheckboxQuery a
  = GetChecked (Bool -> a)

checkbox :: (MonadMaterial m, MonadUUID m) => H.Component CheckboxQuery CheckboxSpec CheckboxChange m
checkbox =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \CheckboxSpec {..} -> do
          id <- generateV4
          pure CheckboxState {mdcFormField = Nothing, ..}
      , render
      , eval =
          H.mkEval $
            H.defaultEval
              { H.handleAction = handleAction
              , H.handleQuery = handleQuery
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              }
      }
  where
    ref = H.RefLabel "checkbox"

    render CheckboxState {..} =
      HH.div [HP.class_ $ HH.ClassName "mdc-touch-targer-wrapper"] $
        pure $
          HH.div
            [HP.class_ $ HH.ClassName "mdc-form-field"]
            [ HH.div
                [ HP.classes $ HH.ClassName "mdc-checkbox" : HH.ClassName "mdc-checkbox--touch" : pureIf (not enabled) (HH.ClassName "mdc-checkbox--disabled")
                , HP.ref ref
                , HP.style extraStyle
                ]
                [ HH.input
                    [ HP.type_ InputCheckbox
                    , HP.id $ UUID.toText id
                    , HP.class_ $ HH.ClassName "mdc-checkbox__native-control"
                    , HP.checked checked
                    , HP.enabled enabled
                    , HE.onChecked Checked
                    ]
                , HH.div
                    [HP.class_ $ HH.ClassName "mdc-checkbox__background"]
                    [ HHS.svg
                        [HPS.class_ $ HH.ClassName "mdc-checkbox__checkmark", HPS.viewBox 0 0 24 24]
                        [ HHS.path
                            -- TODO implement path commands
                            [HPS.class_ $ HH.ClassName "mdc-checkbox__checkmark-path", HPS.fill transparent, HPS.d [HPS.PathCommand "M1.73,12.91 8.1,19.28 22.79,4.59"]]
                        ]
                    , HH.div [HP.class_ $ HH.ClassName "mdc-checkbox__mixedmark"] []
                    ]
                , HH.div [HP.class_ $ HH.ClassName "mdc-checkbox__ripple"] []
                ]
            , HH.label [HP.for $ UUID.toText id] [HH.text label]
            ]

    handleQuery = \case
      GetChecked k -> Just . k <$> gets (.checked)

    handleAction = \case
      Initialize -> do
        H.getHTMLElementRef ref >>= \case
          Just el -> do
            mdcFormField <- lift $ initCheckbox el
            modify $ \s -> s {mdcFormField = Just mdcFormField}
          Nothing -> panic "Cannot initialize checkbox!"
      Finalize -> gets (.mdcFormField) >>= traverse_ (lift . destroyCheckbox)
      Checked checked -> do
        modify $ \s -> s {checked} :: CheckboxState
        H.raise $ CheckboxChange checked
