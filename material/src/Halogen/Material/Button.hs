{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Halogen.Material.Button
  ( button
  , emptyButtonCfg
  , ButtonCfg (..)
  , ButtonStyle (..)
  , IconPosition (..)
  , ButtonClicked (..)
  )
where

import Data.Foreign
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Icons
import Halogen.VDom.DOM.Monad
import Protolude hiding (log)
import Web.DOM.Internal.Types (HTMLElement)

newtype MDCRipple = MDCRipple (Foreign MDCRipple)

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "window.Halogen.init_material_ripple" initRipple :: HTMLElement -> IO MDCRipple
#else
initRipple :: HTMLElement -> IO MDCRipple
initRipple _ = panic "can only be run in JS"
#endif

data IconPosition = Leading | Trailing

data ButtonStyle = Raised | Unelevated | Outlined

data ButtonClicked = ButtonClicked

data ButtonCfg = ButtonCfg
  { label :: Text
  , style :: Maybe ButtonStyle
  , icon :: Maybe (Icon, IconPosition)
  , enabled :: Bool
  }

emptyButtonCfg :: ButtonCfg
emptyButtonCfg =
  ButtonCfg
    { label = ""
    , style = Nothing
    , icon = Nothing
    , enabled = True
    }

data ButtonState = ButtonState
  { buttonElement :: Maybe HTMLElement
  , mdcRipple :: Maybe MDCRipple
  , label :: Text
  , style :: Maybe ButtonStyle
  , icon :: Maybe (Icon, IconPosition)
  , enabled :: Bool
  }

data ButtonAction
  = Initialize
  | Clicked

button :: (MonadIO m, MonadDOM m) => H.Component H.VoidF ButtonCfg ButtonClicked m
button =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \ButtonCfg {..} -> ButtonState {buttonElement = Nothing, mdcRipple = Nothing, ..}
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, H.initialize = Just Initialize}
      }
  where
    ref = H.RefLabel "elem"

    render ButtonState {..} =
      HH.div [HP.class_ (HH.ClassName "mdc-touch-target-wrapper")]
        $ pure
        $ HH.button
          [HP.classes classes, HP.disabled (not enabled), HE.onClick (const Clicked), HP.ref ref]
        $ fold
        $ case icon of
          Just (_, Trailing) ->
            [ [HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] []]
            , [HH.span [HP.class_ (HH.ClassName "mdc-button__label")] [HH.text label]]
            , renderedIc
            ]
          Just (_, Leading) ->
            [ [HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] []]
            , renderedIc
            , [HH.span [HP.class_ (HH.ClassName "mdc-button__label")] [HH.text label]]
            ]
          Nothing ->
            [ [HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] []]
            , [HH.span [HP.class_ (HH.ClassName "mdc-button__label")] [HH.text label]]
            ]
      where
        (renderedIc, renderIconClass) = case icon of
          Just (ic, Leading) -> ([renderIcon [HH.ClassName "mdc-button__icon"] ic], [HH.ClassName "mdc-button--icon-leading"])
          Just (ic, Trailing) -> ([renderIcon [HH.ClassName "mdc-button__icon"] ic], [HH.ClassName "mdc-button--icon-trailing"])
          Nothing -> ([], [])

        classes =
          fold
            [ [HH.ClassName "mdc-button", HH.ClassName "mdc-button--touch"]
            , case style of
                Nothing -> []
                Just Raised -> [HH.ClassName "mdc-button--raised"]
                Just Unelevated -> [HH.ClassName "mdc-button--unelevated"]
                Just Outlined -> [HH.ClassName "mdc-button--outlined"]
            , renderIconClass
            ]

    handleAction = \case
      Initialize ->
        H.getHTMLElementRef ref >>= traverse_ (liftIO . initRipple)
      Clicked ->
        H.raise ButtonClicked
