module Main where

import Data.Row
import Halogen as H
import Halogen.HTML as HH hiding (map)
import Halogen.Material.Button as HM
import Halogen.Material.Icons qualified as Icon
import Halogen.VDom.DOM.Monad
import Protolude

#if defined(javascript_HOST_ARCH)
import Halogen.Aff.Util as HA
import Halogen.VDom.Driver (runUI)
#endif

attachComponent :: IO (HalogenSocket VoidF () IO)
#if defined(javascript_HOST_ARCH)
attachComponent =
  HA.awaitBody >>= runUI component ()
#else
attachComponent = panic "This module can only be run on JavaScript"
#endif

main :: IO ()
main = void attachComponent

type Slots = ("debounced" .== H.Slot VoidF ButtonClicked Text)

component :: forall m. (MonadDOM m, MonadIO m) => H.Component H.VoidF () () m
component =
    H.mkComponent $
        H.ComponentSpec
            { initialState = const ()
            , render
            , eval = H.mkEval H.defaultEval
            }
  where
    p = Proxy @"debounced"

    render :: () -> H.ComponentHTML Void Slots m
    render _ =
        HH.ul
            []
            $ map (HH.li [] . pure)
            $ [ HH.slot_ p "Text" HM.button emptyButtonCfg{label = "Text button"}
              , HH.slot_ p "Text-Icon" HM.button emptyButtonCfg{label = "Text button with icon", icon = Just (Icon.Search, HM.Leading)}
              , HH.slot_ p "Outlined" HM.button emptyButtonCfg{label = "Outlined button", style = Just HM.Outlined}
              , HH.slot_ p "Outlined-Icon" HM.button emptyButtonCfg{label = "Outlined button with icon", style = Just HM.Outlined, icon = Just (Icon.Info, HM.Leading)}
              , HH.slot_ p "Contained" HM.button emptyButtonCfg{label = "Contained button", style = Just HM.Raised}
              , HH.slot_ p "Contained-Icon" HM.button emptyButtonCfg{label = "Contained button with icon", style = Just HM.Raised, icon = Just (Icon.Favorite, HM.Leading)}
              , HH.slot_ p "Unelevated" HM.button emptyButtonCfg{label = "Unelevated button", style = Just HM.Unelevated}
              , HH.slot_ p "Unelevated-Icon" HM.button emptyButtonCfg{label = "Unelevated button with icon", style = Just HM.Unelevated, icon = Just (Icon.Settings, HM.Leading)}
              , HH.slot_ p "Disabled" HM.button emptyButtonCfg{label = "Disabled button", enabled = False}
              , HH.slot_ p "Disabled-Icon" HM.button emptyButtonCfg{label = "Disabled button with icon", icon = Just (Icon.Delete, HM.Leading), enabled = False}
              , HH.slot_ p "Contained-Icon-Trailing" HM.button emptyButtonCfg{label = "Contained button with trailing icon", style = Just HM.Raised, icon = Just (Icon.Favorite, HM.Trailing)}
              ]
