module Halogen.Material.Monad where

import Data.Foreign
import Protolude
import Web.DOM.Internal.Types (HTMLElement)

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim
import Data.Coerce
#endif

newtype MDCRipple = MDCRipple (Foreign MDCRipple)

newtype MDCList = MDCList (Foreign MDCList)

newtype MDCTabBar = MDCTabBar (Foreign MDCTabBar)

class (Monad m) => MonadMaterial m where
  initRipple :: HTMLElement -> m MDCRipple
  destroyRipple :: MDCRipple -> m ()
  initList :: HTMLElement -> m MDCList
  destroyList :: MDCList -> m ()
  initListItems :: MDCList -> m [MDCRipple]
  initTabBar :: HTMLElement -> m MDCTabBar
  destroyTabBar :: MDCTabBar -> m ()

#if defined(javascript_HOST_ARCH)

foreign import javascript unsafe "halogen_init_material_ripple" initRipple' :: HTMLElement -> IO MDCRipple
foreign import javascript unsafe "halogen_destroy_material_ripple" destroyRipple' :: MDCRipple -> IO ()
foreign import javascript unsafe "halogen_init_material_list" initList' :: HTMLElement -> IO MDCList
foreign import javascript unsafe "halogen_destroy_material_list" destroyList' :: MDCList -> IO ()
foreign import javascript unsafe "halogen_init_material_list_items" initListItems' :: MDCList -> IO JSVal
foreign import javascript unsafe "halogen_init_material_tab_bar" initTabBar' :: HTMLElement -> IO MDCTabBar
foreign import javascript unsafe "halogen_destroy_material_tab_bar" destroyTabBar' :: MDCTabBar -> IO ()

instance MonadMaterial IO where
  initRipple = initRipple'
  destroyRipple = destroyRipple'
  initList = initList'
  destroyList = destroyList'
  initListItems = fmap coerce . (fromJSArray <=< initListItems')
  initTabBar = initTabBar'
  destroyTabBar = destroyTabBar'

#endif
