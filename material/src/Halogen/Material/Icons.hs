module Halogen.Material.Icons (Icon (..), iconText, renderIcon) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Protolude

-- TODO all icons
data Icon
  = Search
  | Home
  | Settings
  | Info
  | Delete
  | Favorite
  | Apps
  | Download
  | Add
  | Menu
  | Close
  | Cancel
  | Block
  | Logout
  deriving (Enum, Bounded)

iconText :: Icon -> Text
iconText = \case
  Search -> "search"
  Home -> "home"
  Settings -> "settings"
  Info -> "info"
  Delete -> "delete"
  Favorite -> "favorite"
  Apps -> "apps"
  Download -> "download"
  Add -> "add"
  Menu -> "menu"
  Close -> "close"
  Cancel -> "cancel"
  Block -> "block"
  Logout -> "logout"

renderIcon :: [ClassName] -> Icon -> HH.HTML w i
renderIcon clss icon =
  HH.i
    [HP.classes (ClassName "material-icons" : clss), HPA.hidden "true"]
    [HH.text $ iconText icon]
