module Halogen.Material.TextField
  ( TextFieldCfg (..)
  , TextFieldStyle (..)
  , TextFieldAffix (..)
  , TextFieldHelperLine (..)
  , TextFieldOutput (..)
  , TextFieldQuery (..)
  , textField
  , emptyTextFieldCfg
  )
where

import Control.Monad.Extra (pureIf)
import Control.Monad.UUID
import DOM.HTML.Indexed
import Data.Text qualified as T
import Data.UUID.Types as UUID
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.HTML.Properties.ARIA qualified as HPA
import Halogen.Material.Icons
import Halogen.Material.Monad
import Protolude

data TextFieldStyle
  = Filled

-- | Outlined
data TextFieldAffix
  = NoAffix
  | TextAffix Text
  | IconAffix Icon

isIconAffix :: TextFieldAffix -> Bool
isIconAffix = \case
  IconAffix _ -> True
  _ -> False

data TextFieldHelperLine
  = NoHelperLine
  | HelperLine Text
  | CharacterCounter

data TextFieldCfg = TextFieldCfg
  { text :: Text
  , label :: Maybe Text
  , enabled :: Bool
  , type_ :: InputType
  , helperLine :: TextFieldHelperLine
  , style :: TextFieldStyle
  , prefix :: TextFieldAffix
  , suffix :: TextFieldAffix
  , minMaxLength :: (Maybe Int, Maybe Int)
  }

emptyTextFieldCfg :: TextFieldCfg
emptyTextFieldCfg =
  TextFieldCfg
    { text = ""
    , label = Nothing
    , enabled = True
    , type_ = InputText
    , helperLine = NoHelperLine
    , style = Filled
    , prefix = NoAffix
    , suffix = NoAffix
    , minMaxLength = (Nothing, Nothing)
    }

data TextFieldState = TextFieldState
  { mdcTextField :: Maybe MDCTextField
  , text :: Text
  , label :: Maybe Text
  , enabled :: Bool
  , type_ :: InputType
  , helperLine :: TextFieldHelperLine
  , style :: TextFieldStyle
  , prefix :: TextFieldAffix
  , suffix :: TextFieldAffix
  , labelId :: UUID
  , helperId :: UUID
  , minMaxLength :: (Maybe Int, Maybe Int)
  }

newtype TextFieldOutput = InputChanged Text

newtype TextFieldQuery a = GetText (Text -> a)

data TextFieldAction
  = Initialize
  | Finalize
  | InputChange Text

textField :: (MonadMaterial m, MonadUUID m) => H.Component TextFieldQuery TextFieldCfg TextFieldOutput m
textField =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \TextFieldCfg {..} -> do
          labelId <- generateV4
          helperId <- generateV4
          pure $
            TextFieldState
              { mdcTextField = Nothing
              , ..
              }
      , render
      , eval = H.mkEval $ H.defaultEval {H.initialize = Just Initialize, H.finalize = Just Finalize, H.handleAction = handleAction, H.handleQuery = handleQuery}
      }
  where
    ref = H.RefLabel "text-field"

    render TextFieldState {..} = case helperLine of
      NoHelperLine -> tf
      HelperLine t ->
        HH.div_
          [ tf
          , HH.div
              [HP.class_ $ HH.ClassName "mdc-text-field-helper-line"]
              [HH.span [HP.class_ $ HH.ClassName "mdc-text-field-helper-text", HPA.hidden "true", HP.id (UUID.toText helperId)] [HH.text t]]
          ]
      CharacterCounter ->
        HH.div_
          [ tf
          , HH.div [HP.class_ $ HH.ClassName "mdc-text-field-character-counter"] [HH.text showCounter] -- TODO length limit
          ]
      where
        showCounter = case minMaxLength of
          (_, Just l) -> show (T.length text) <> " / " <> show l
          _ -> show $ T.length text
        tf = renderTF TextFieldState {..}

    renderTF TextFieldState {..} = case style of
      Filled ->
        HH.label
          [ HP.ref ref
          , HP.classes $
              HH.ClassName "mdc-text-field"
                : HH.ClassName "mdc-text-field--filled"
                : catMaybes
                  [ pureIf (isJust label && not (T.null text)) (HH.ClassName "mdc-text-field--label-floating")
                  , pureIf (isNothing label) (HH.ClassName "mdc-text-field--no-label")
                  , pureIf (not enabled) (HH.ClassName "mdc-text-field--disabled")
                  ]
          ]
          $ HH.span [HP.class_ $ HH.ClassName "mdc-text-field__ripple"] []
            : catMaybes
              [ fmap (HH.span [HP.classes $ HH.ClassName "mdc-floating-label" : pureIf (not $ T.null text) (HH.ClassName "mdc-floating-label--float-above")] . pure . HH.text) label
              , case prefix of
                  NoAffix -> Nothing
                  TextAffix t -> Just $ HH.span [HP.classes [HH.ClassName "mdc-text-field__affix", HH.ClassName "mdc-text-field__affix--prefix"]] [HH.text t]
                  IconAffix i ->
                    Just $
                      HH.i
                        ( HP.classes [HH.ClassName "material-icons", HH.ClassName "mdc-text-field__icon--leading"]
                            : if isIconAffix suffix then [] else [HP.tabIndex 0, HPA.role "button"]
                        )
                        [HH.text $ iconText i]
              , pure $
                  HH.input $
                    [ HP.type_ type_
                    , HP.class_ $ HH.ClassName "mdc-text-field__input"
                    , HP.disabled (not enabled)
                    , HP.value text
                    , HE.onInputValueChange $ Just . InputChange
                    , HPA.labelledBy $ UUID.toText labelId
                    , HPA.describedBy $ UUID.toText helperId
                    ]
                      <> catMaybes
                        [ fmap HP.minLength $ fst minMaxLength
                        , fmap HP.maxLength $ snd minMaxLength
                        ]
              , case suffix of
                  NoAffix -> Nothing
                  TextAffix t -> Just $ HH.span [HP.classes [HH.ClassName "mdc-text-field__affix", HH.ClassName "mdc-text-field__affix--suffix"]] [HH.text t]
                  IconAffix i ->
                    Just $
                      HH.i
                        [ HP.classes [HH.ClassName "material-icons", HH.ClassName "mdc-text-field__icon--trailing"]
                        , HP.tabIndex 0
                        , HPA.role "button"
                        ]
                        [HH.text $ iconText i]
              , pure $ HH.span [HP.class_ $ HH.ClassName "mdc-line-ripple"] []
              ]
    -- Outlined -> TODO

    handleQuery = \case
      GetText f -> (Just . f) <$> gets (.text)

    handleAction = \case
      Initialize ->
        H.getHTMLElementRef ref >>= \case
          Just el -> do
            mdcTextField <- lift $ initTextField el
            modify $ \s -> s {mdcTextField = Just mdcTextField}
          Nothing -> panic "Cannot initialize text field, no HTMLElement found"
      Finalize ->
        gets (.mdcTextField) >>= traverse_ (lift . destroyTextField)
      InputChange t -> do
        modify $ \s -> s {text = t} :: TextFieldState
        H.raise $ InputChanged t
