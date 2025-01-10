{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Halogen.Material.TextField where

import DOM.HTML.Indexed
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Icons
import Halogen.Material.Monad
import Protolude

data TextFieldStyle
  = Filled
  | Outlined

data TextFieldAffix
  = NoAffix
  | TextAffix Text
  | IconAffix Icon

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
  }

newtype TextFieldOutput = InputChanged Text

newtype TextFieldQuery a = GetText (Text -> a)

data TextFieldAction
  = Initialize
  | Finalize
  | InputChange Text

textField :: (MonadMaterial m) => H.Component TextFieldQuery TextFieldCfg TextFieldOutput m
textField =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \TextFieldCfg {..} -> TextFieldState {mdcTextField = Nothing, ..}
      , render
      , eval = H.mkEval $ H.defaultEval {H.initialize = Just Initialize, H.finalize = Just Finalize, H.handleAction = handleAction, H.handleQuery = handleQuery}
      }
  where
    ref = H.RefLabel "text-field"

    render :: TextFieldState -> HH.HTML w TextFieldAction
    render = undefined

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
