{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Halogen.Material.Monad
  ( MonadMaterial (..)
  , MDCRipple
  , MDCList
  , MDCTabBar
  , MDCTextField
  , MDCFormField
  )
where

import Data.Foreign
import Protolude
import Web.DOM.Internal.Types (HTMLElement (..))

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim
import Data.Coerce
#elif defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSVal)
#endif

newtype MDCRipple = MDCRipple (Foreign MDCRipple)

newtype MDCList = MDCList (Foreign MDCList)

newtype MDCTabBar = MDCTabBar (Foreign MDCTabBar)

newtype MDCTextField = MDCTextField (Foreign MDCTextField)

newtype MDCFormField = MDCFormField (Foreign MDCFormField)

class (Monad m) => MonadMaterial m where
  initRipple :: HTMLElement -> m MDCRipple
  destroyRipple :: MDCRipple -> m ()
  initList :: HTMLElement -> m MDCList
  destroyList :: MDCList -> m ()
  initListItems :: MDCList -> m [MDCRipple]
  initTabBar :: HTMLElement -> m MDCTabBar
  destroyTabBar :: MDCTabBar -> m ()
  initTextField :: HTMLElement -> m MDCTextField
  destroyTextField :: MDCTextField -> m ()
  initRadioButton :: HTMLElement -> m MDCFormField
  destroyRadioButton :: MDCFormField -> m ()
  initCheckbox :: HTMLElement -> m MDCFormField
  destroyCheckbox :: MDCFormField -> m ()

#if defined(javascript_HOST_ARCH)

foreign import javascript unsafe "halogen_init_material_ripple" initRipple' :: HTMLElement -> IO MDCRipple
foreign import javascript unsafe "halogen_destroy_material_ripple" destroyRipple' :: MDCRipple -> IO ()
foreign import javascript unsafe "halogen_init_material_list" initList' :: HTMLElement -> IO MDCList
foreign import javascript unsafe "halogen_destroy_material_list" destroyList' :: MDCList -> IO ()
foreign import javascript unsafe "halogen_init_material_list_items" initListItems' :: MDCList -> IO JSVal
foreign import javascript unsafe "halogen_init_material_tab_bar" initTabBar' :: HTMLElement -> IO MDCTabBar
foreign import javascript unsafe "halogen_destroy_material_tab_bar" destroyTabBar' :: MDCTabBar -> IO ()
foreign import javascript unsafe "halogen_init_material_text_field" initTextField' :: HTMLElement -> IO MDCTextField
foreign import javascript unsafe "halogen_destroy_material_text_field" destroyTextField' :: MDCTextField -> IO ()
foreign import javascript unsafe "halogen_init_material_radio_button" initRadioButton' :: HTMLElement -> IO MDCFormField
foreign import javascript unsafe "halogen_destroy_material_radio_button" destroyRadioButton' :: MDCFormField -> IO ()
foreign import javascript unsafe "halogen_init_material_checkbox" initCheckbox' :: HTMLElement -> IO MDCFormField
foreign import javascript unsafe "halogen_destroy_material_checkbox" destroyCheckbox' :: MDCFormField -> IO ()

instance MonadMaterial IO where
  initRipple = initRipple'
  destroyRipple = destroyRipple'
  initList = initList'
  destroyList = destroyList'
  initListItems = fmap coerce . (fromJSArray <=< initListItems')
  initTabBar = initTabBar'
  destroyTabBar = destroyTabBar'
  initTextField = initTextField'
  destroyTextField = destroyTextField'
  initRadioButton = initRadioButton'
  destroyRadioButton = destroyRadioButton'
  initCheckbox = initCheckbox'
  destroyCheckbox = destroyCheckbox'

#elif defined(wasm32_HOST_ARCH)

foreign import javascript unsafe "globalThis.halogen_init_material_ripple($1)" initRipple' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_destroy_material_ripple($1)" destroyRipple' :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.halogen_init_material_list($1)" initList' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_destroy_material_list($1)" destroyList' :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.halogen_init_material_list_items($1)" initListItems' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_init_material_tab_bar($1)" initTabBar' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_destroy_material_tab_bar($1)" destroyTabBar' :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.halogen_init_material_text_field($1)" initTextField' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_destroy_material_text_field($1)" destroyTextField' :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.halogen_init_material_radio_button($1)" initRadioButton' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_destroy_material_radio_button($1)" destroyRadioButton' :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.halogen_init_material_checkbox($1)" initCheckbox' :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.halogen_destroy_material_checkbox($1)" destroyCheckbox' :: JSVal -> IO ()
foreign import javascript unsafe "$1.length" jsArrayLength :: JSVal -> Int
foreign import javascript unsafe "$1[$2]" jsArrayIndex :: JSVal -> Int -> JSVal

instance MonadMaterial IO where
  initRipple = fmap MDCRipple . initRipple' . unHTMLElement
  destroyRipple (MDCRipple ripple) = destroyRipple' ripple
  initList = fmap MDCList . initList' . unHTMLElement
  destroyList (MDCList mdcList) = destroyList' mdcList
  initListItems (MDCList mdcList) = do
    items <- initListItems' mdcList
    pure $ map (MDCRipple . jsArrayIndex items) [0 .. jsArrayLength items - 1]
  initTabBar = fmap MDCTabBar . initTabBar' . unHTMLElement
  destroyTabBar (MDCTabBar tabBar) = destroyTabBar' tabBar
  initTextField = fmap MDCTextField . initTextField' . unHTMLElement
  destroyTextField (MDCTextField textField) = destroyTextField' textField
  initRadioButton = fmap MDCFormField . initRadioButton' . unHTMLElement
  destroyRadioButton (MDCFormField formField) = destroyRadioButton' formField
  initCheckbox = fmap MDCFormField . initCheckbox' . unHTMLElement
  destroyCheckbox (MDCFormField formField) = destroyCheckbox' formField

unHTMLElement :: HTMLElement -> JSVal
unHTMLElement (HTMLElement element) = element

#endif
