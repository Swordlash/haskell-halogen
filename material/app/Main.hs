module Main where

import Clay qualified as C
import Data.Row
import Halogen as H
import Halogen.HTML as HH hiding (map)
import Halogen.Material.Button as HM
import Halogen.Material.Icons qualified as Icon
import Halogen.Material.List as HL
import Halogen.Material.Monad
import Halogen.VDom.DOM.Monad
import Protolude
import Protolude.Partial ((!!))

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

type Slots =
  ("list" .== H.Slot (ListQuery Buttons VoidF) (ListOutput Void) ())
    .+ Buttons

type Buttons = ("buttons" .== H.Slot VoidF ButtonClicked Text)

component :: forall m. (MonadDOM m, MonadMaterial m, MonadIO m) => H.Component H.VoidF () () m
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState = const ()
      , render
      , eval = H.mkEval H.defaultEval
      }
  where
    p = Proxy @"buttons"
    pl = Proxy @"list"

    render :: () -> H.ComponentHTML Void Slots m
    render _ =
      HH.slot_ pl () HL.list ListCfg {items, elemRenderer, extraStyle}
      where
        extraStyle :: C.Css
        extraStyle = do
          C.backgroundColor C.white
          C.border (C.px 1) C.solid (C.rgb 229 229 229)
          C.width (C.pct 50)

        items =
          map ListElem $
            zip
              [0 ..]
              [ ("Text", emptyButtonCfg {label = "Text button"})
              , ("Text-Icon", emptyButtonCfg {label = "Text button with icon", icon = Just (Icon.Search, HM.Leading)})
              , ("Outlined", emptyButtonCfg {label = "Outlined button", style = Just HM.Outlined})
              , ("Outlined-Icon", emptyButtonCfg {label = "Outlined button with icon", style = Just HM.Outlined, icon = Just (Icon.Info, HM.Leading)})
              , ("Contained", emptyButtonCfg {label = "Contained button", style = Just HM.Raised})
              , ("Contained-Icon", emptyButtonCfg {label = "Contained button with icon", style = Just HM.Raised, icon = Just (Icon.Favorite, HM.Leading)})
              , ("Unelevated", emptyButtonCfg {label = "Unelevated button", style = Just HM.Unelevated})
              , ("Unelevated-Icon", emptyButtonCfg {label = "Unelevated button with icon", style = Just HM.Unelevated, icon = Just (Icon.Settings, HM.Leading)})
              , ("Disabled", emptyButtonCfg {label = "Disabled button", enabled = False})
              , ("Disabled-Icon", emptyButtonCfg {label = "Disabled button with icon", icon = Just (Icon.Delete, HM.Leading), enabled = False})
              , ("Contained-Icon-Trailing", emptyButtonCfg {label = "Contained button with trailing icon", style = Just HM.Raised, icon = Just (Icon.Favorite, HM.Trailing)})
              ]

        elemRenderer :: ElemRenderer (Int, (Text, ButtonCfg)) Buttons Void m
        elemRenderer =
          ElemRenderer
            { metaRenderer = Just $ \(_, (name, cfg)) -> HH.slot_ p name HM.button cfg
            , textRenderer = Oneline (fst . snd)
            , iconRenderer = Just $ \(idx, _) -> [minBound .. maxBound] !! idx
            }
