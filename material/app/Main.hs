module Main (main, component) where

import Clay qualified as C
import Control.Monad.UUID
import DOM.HTML.Indexed (InputType (..))
import Data.Row
import Halogen as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Button qualified as HMB
import Halogen.Material.Icons qualified as HMI
import Halogen.Material.List qualified as HML
import Halogen.Material.Monad
import Halogen.Material.RadioButton qualified as HMR
import Halogen.Material.Tabs qualified as HMT
import Halogen.Material.TextField qualified as HMTF
import Protolude hiding (All)
import Protolude.Partial (fromJust, (!!))

#if defined(javascript_HOST_ARCH)
import Halogen.IO.Util as HA
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

type All m = List m .+ Radios .+ TextFields

type Slots m = ("tab" .== H.Slot (HMT.TabsQuery (All m) VoidF) (HMT.TabsCfg (All m) Void m) (HMT.TabsOutput Void) ())

type List m = ("list" .== H.Slot (HML.ListQuery Buttons VoidF) (HML.ListCfg (Int, (Text, HMB.ButtonCfg)) Buttons Void m) (HML.ListOutput Void) ())

type Buttons = ("button" .== H.Slot VoidF HMB.ButtonCfg HMB.ButtonClicked Text)

type TextFields = ("textField" .== H.Slot HMTF.TextFieldQuery HMTF.TextFieldCfg HMTF.TextFieldOutput Int)

type Radios = ("radio" .== H.Slot VoidF HMR.RadioButtonCfg HMR.RadioChange Int)

component :: forall q i o m. (MonadMaterial m, MonadUUID m) => H.Component q i o m
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState = const $ pure ()
      , render
      , eval = H.mkEval H.defaultEval
      }
  where
    render :: () -> H.ComponentHTML Void (Slots m) m
    render _ =
      HH.slot_ "tab" () HMT.tabsComponent $ HMT.emptyTabsCfg {HMT.tabs = tabs, HMT.extraStyle = C.width (C.pct 50)}
      where
        tabs =
          fromJust $
            nonEmpty
              [
                ( HMT.TabCfg {label = Just "List", icon = Just (HMI.Menu, HMT.Stacked)}
                , HH.slot_ "list" () HML.list HML.ListCfg {items, elemRenderer, extraStyle}
                )
              ,
                ( HMT.TabCfg {label = Just "Logout", icon = Just (HMI.Logout, HMT.Stacked)}
                , HH.div
                    [ HP.style $ do
                        C.display C.flex
                        C.flexDirection C.column
                        C.width C.auto
                        C.padding pad pad pad pad
                        extraStyle
                    ]
                    [ HH.slot_ "textField" 0 HMTF.textField $
                        HMTF.emptyTextFieldCfg
                          { HMTF.label = Just "Username"
                          , HMTF.helperLine = HMTF.CharacterCounter
                          , HMTF.minMaxLength = (Nothing, Just 20)
                          }
                    , HH.slot_ "textField" 1 HMTF.textField $
                        HMTF.emptyTextFieldCfg
                          { HMTF.label = Just "Password"
                          , HMTF.type_ = InputPassword
                          , HMTF.helperLine = HMTF.CharacterCounter
                          }
                    , HH.slot_ "textField" 2 HMTF.textField $
                        HMTF.emptyTextFieldCfg
                          { HMTF.label = Just "Donation"
                          , HMTF.prefix = HMTF.TextAffix "$"
                          , HMTF.suffix = HMTF.IconAffix HMI.CreditCard
                          , HMTF.type_ = InputNumber
                          , HMTF.helperLine = HMTF.HelperLine "Any donation helps our cause!"
                          }
                    ]
                )
              ,
                ( HMT.TabCfg {label = Just "Radio Buttons", icon = Just (HMI.Radio, HMT.Stacked)}
                , HH.div
                    [ HP.style $ do
                        C.display C.flex
                        C.flexDirection C.column
                        C.width C.auto
                    ]
                    [ HH.slot_ "radio" 0 HMR.radio $
                        HMR.emptyRadioButtonCfg
                          { HMR.label = "White"
                          , HMR.groupName = "group1"
                          , HMR.checked = True
                          }
                    , HH.slot_ "radio" 1 HMR.radio $
                        HMR.emptyRadioButtonCfg
                          { HMR.label = "Black"
                          , HMR.groupName = "group1"
                          }
                    , HH.slot_ "radio" 2 HMR.radio $
                        HMR.emptyRadioButtonCfg
                          { HMR.label = "Red"
                          , HMR.groupName = "group1"
                          }
                    , HH.slot_ "radio" 3 HMR.radio $
                        HMR.emptyRadioButtonCfg
                          { HMR.label = "Green"
                          , HMR.groupName = "group1"
                          }
                    ]
                )
              ]

        pad :: C.Size C.LengthUnit
        pad = C.em 1

        extraStyle :: C.Css
        extraStyle = do
          C.backgroundColor C.white
          C.border (C.px 1) C.solid (C.rgb 229 229 229)

        items =
          map HML.ListElem $
            zip
              [0 ..]
              [ ("Text", HMB.emptyButtonCfg {HMB.label = "Text button"})
              , ("Text-Icon", HMB.emptyButtonCfg {HMB.label = "Text button with icon", HMB.icon = Just (HMI.Search, HMB.Leading)})
              , ("Outlined", HMB.emptyButtonCfg {HMB.label = "Outlined button", HMB.style = Just HMB.Outlined})
              , ("Outlined-Icon", HMB.emptyButtonCfg {HMB.label = "Outlined button with icon", HMB.style = Just HMB.Outlined, HMB.icon = Just (HMI.Info, HMB.Leading)})
              , ("Contained", HMB.emptyButtonCfg {HMB.label = "Contained button", HMB.style = Just HMB.Raised})
              , ("Contained-Icon", HMB.emptyButtonCfg {HMB.label = "Contained button with icon", HMB.style = Just HMB.Raised, HMB.icon = Just (HMI.Favorite, HMB.Leading)})
              , ("Unelevated", HMB.emptyButtonCfg {HMB.label = "Unelevated button", HMB.style = Just HMB.Unelevated})
              , ("Unelevated-Icon", HMB.emptyButtonCfg {HMB.label = "Unelevated button with icon", HMB.style = Just HMB.Unelevated, HMB.icon = Just (HMI.Settings, HMB.Leading)})
              , ("Disabled", HMB.emptyButtonCfg {HMB.label = "Disabled button", HMB.enabled = False})
              , ("Disabled-Icon", HMB.emptyButtonCfg {HMB.label = "Disabled button with icon", HMB.icon = Just (HMI.Delete, HMB.Leading), HMB.enabled = False})
              , ("Contained-Icon-Trailing", HMB.emptyButtonCfg {HMB.label = "Contained button with trailing icon", HMB.style = Just HMB.Raised, HMB.icon = Just (HMI.Favorite, HMB.Trailing)})
              ]

        elemRenderer :: HML.ElemRenderer (Int, (Text, HMB.ButtonCfg)) Buttons Void m
        elemRenderer =
          HML.ElemRenderer
            { metaRenderer = Just $ \(_, (name, cfg)) -> HH.slot_ "button" name HMB.button cfg
            , textRenderer = HML.Oneline (fst . snd)
            , iconRenderer = Just $ \(idx, _) -> [minBound .. maxBound] !! idx
            }
