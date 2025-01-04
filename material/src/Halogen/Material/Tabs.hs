module Halogen.Material.Tabs where

import Clay (Css)
import Data.Foreign (Foreign)
import Data.Text qualified as T
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Icons
import Halogen.Material.Ripple
import Halogen.VDom.DOM.Monad
import Protolude hiding (log)
import Web.DOM.Internal.Types (HTMLElement)

newtype MDCTabBar = MDCTabBar (Foreign MDCTabBar)

#if defined (javascript_HOST_ARCH)
foreign import javascript unsafe "window.Halogen.init_material_tab_bar" initTabBar :: HTMLElement -> IO MDCTabBar
#else
initTabBar :: HTMLElement -> IO MDCTabBar
initTabBar _ = panic "can only be run in JS"
#endif

data IconPosition = Leading | Stacked

data TabCfg = TabCfg
  { label :: Maybe Text
  , icon :: Maybe (Icon, IconPosition)
  }

data TabsCfg slots i m = TabsCfg
  { tabs :: NonEmpty (TabCfg, HH.ComponentHTML i slots m)
  , selectedTab :: Int
  }

data TabsState slots i m = TabsState
  { tabBar :: Maybe MDCTabBar
  , tabs :: NonEmpty (TabCfg, HH.ComponentHTML i slots m)
  , selectedTab :: Int
  }

data TabsAction
  = Initialize
  | TabSelected Int
