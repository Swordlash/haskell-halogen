module Halogen.Material.Button
  ( button
  , emptyButtonSpec
  , ButtonSpec (..)
  , ButtonStyle (..)
  , IconPosition (..)
  , ButtonClicked (..)
  )
where

import Clay (Css)
import Data.Text qualified as T
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Icons
import Halogen.Material.Monad
import Protolude

data IconPosition = Leading | Trailing

data ButtonStyle = Raised | Unelevated | Outlined

data ButtonClicked = ButtonClicked

data ButtonSpec = ButtonSpec
  { label :: Text
  , style :: Maybe ButtonStyle
  , icon :: Maybe (Icon, IconPosition)
  , enabled :: Bool
  , extraStyle :: Css
  }

emptyButtonSpec :: ButtonSpec
emptyButtonSpec =
  ButtonSpec
    { label = ""
    , style = Nothing
    , icon = Nothing
    , enabled = True
    , extraStyle = mempty
    }

data ButtonState = ButtonState
  { mdcRipple :: Maybe MDCRipple
  , label :: Text
  , style :: Maybe ButtonStyle
  , icon :: Maybe (Icon, IconPosition)
  , enabled :: Bool
  , extraStyle :: Css
  }

data ButtonAction
  = Initialize
  | Finalize
  | Clicked

button
  :: (MonadMaterial m)
  => H.Component q ButtonSpec ButtonClicked m
button =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \ButtonSpec {..} -> pure ButtonState {mdcRipple = Nothing, ..}
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, initialize = Just Initialize, finalize = Just Finalize}
      }
  where
    ref = H.RefLabel "elem"

    render ButtonState {..} =
      HH.div [HP.class_ (HH.ClassName "mdc-touch-target-wrapper")]
        $ pure
        $ HH.button
          [HP.classes classes, HP.style extraStyle, HP.disabled (not enabled), HE.onClick (const Clicked), HP.ref ref]
        $ case icon of
          Just (_, Trailing) ->
            HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] []
              : HH.span [HP.class_ (HH.ClassName "mdc-button__touch")] []
              -- when the icon is trailing, label is not optional
              : HH.span [HP.class_ (HH.ClassName "mdc-button__label")] [HH.text label]
              : maybeToList renderedIc
          Just (_, Leading) ->
            HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] []
              : HH.span [HP.class_ (HH.ClassName "mdc-button__touch")] []
              : catMaybes
                [ renderedIc
                , lab
                ]
          Nothing ->
            HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] [] : maybeToList lab
      where
        lab = if T.null label then Nothing else Just $ HH.span [HP.class_ (HH.ClassName "mdc-button__label")] [HH.text label]

        (renderedIc, renderIconClass) = case icon of
          Just (ic, dir) -> (Just $ renderIcon [HH.ClassName "mdc-button__icon"] ic,) $ case dir of
            Leading -> Just $ HH.ClassName "mdc-button--icon-leading"
            Trailing -> Just $ HH.ClassName "mdc-button--icon-trailing"
          Nothing -> (empty, empty)

        classes =
          [HH.ClassName "mdc-button", HH.ClassName "mdc-button--touch"]
            <> case style of
              Nothing -> []
              Just Raised -> [HH.ClassName "mdc-button--raised"]
              Just Unelevated -> [HH.ClassName "mdc-button--unelevated"]
              Just Outlined -> [HH.ClassName "mdc-button--outlined"]
            <> maybeToList renderIconClass

    handleAction = \case
      Initialize ->
        H.getHTMLElementRef ref >>= \case
          Nothing -> panic "Cannot initialize button Ripple, no HTML element found"
          Just e -> do
            ripple <- lift $ initRipple e
            modify $ \s -> s {mdcRipple = Just ripple}
      Finalize -> traverse_ (lift . destroyRipple) =<< gets (.mdcRipple)
      Clicked ->
        H.raise ButtonClicked
