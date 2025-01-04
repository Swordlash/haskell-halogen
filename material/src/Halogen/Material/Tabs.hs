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
import Halogen.Material.Monad
import Halogen.VDom.DOM.Monad
import Protolude hiding (log)
import Web.DOM.Internal.Types (HTMLElement)

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
