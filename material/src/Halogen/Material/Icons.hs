module Halogen.Material.Icons where

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

iconText :: Icon -> Text
iconText = \case
    Search -> "search"
    Home -> "home"
    Settings -> "settings"
    Info -> "info"
    Delete -> "delete"
    Favorite -> "favorite"

renderIcon :: [ClassName] -> Icon -> HH.HTML w i
renderIcon clss icon =
    HH.i
        [HP.classes (ClassName "material-icons" : clss), HPA.hidden "true"]
        [HH.text $ iconText icon]
