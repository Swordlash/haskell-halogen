module Example.Material (component) where

import Clay qualified as C
import Control.Monad.UUID
import DOM.HTML.Indexed (InputType (..))
import Data.Row
import Halogen as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Button qualified as HMB
import Halogen.Material.Checkbox qualified as HMC
import Halogen.Material.Icons qualified as HMI
import Halogen.Material.List qualified as HML
import Halogen.Material.Monad
import Halogen.Material.RadioButton qualified as HMR
import Halogen.Material.Tabs qualified as HMT
import Halogen.Material.TextField qualified as HMTF
import Protolude hiding (All)
import Protolude.Partial (fromJust, (!!))

type All m = List m .+ Radios .+ TextFields .+ Checkboxes

type Slots m = ("tab" .== H.Slot (HMT.TabsQuery (All m) VoidF) (HMT.TabsOutput Action) ())

type List m = ("list" .== H.Slot (HML.ListQuery Buttons VoidF) (HML.ListOutput Void) ())

type Buttons = ("button" .== H.Slot VoidF HMB.ButtonClicked Text)

type TextFields = ("textField" .== H.Slot HMTF.TextFieldQuery HMTF.TextFieldOutput Int)

type Radios = ("radio" .== H.Slot VoidF HMR.RadioClicked Int)

type Checkboxes = ("checkbox" .== H.Slot HMC.CheckboxQuery HMC.CheckboxChange Int)

-- | The controls keep their own state, since the tab bar only hides the tabs
-- that are not selected. The radio buttons are the one place this component
-- reacts to what happens inside a tab, to show the chosen colour.
data Action
  = TabSelected
  | ColourPicked Text

component :: forall q i o m. (MonadMaterial m, MonadUUID m) => H.Component q i o m
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState = const $ pure "White"
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    handleAction :: Action -> H.HalogenM Text Action (Slots m) o m ()
    handleAction = \case
      TabSelected -> pure ()
      ColourPicked colour -> put colour

    render :: Text -> H.ComponentHTML Action (Slots m) m
    render colour =
      HH.slot "tab" () HMT.tabsComponent HMT.emptyTabsSpec {HMT.tabs = tabs} $ \case
        HMT.SelectedTab _ -> TabSelected
        HMT.ChildOutput a -> a
      where
        radio n spec = HH.slot "radio" n HMR.radio spec $ \(HMR.RadioClicked label) -> ColourPicked label

        tabs =
          fromJust $
            nonEmpty
              [
                ( HMT.TabSpec {label = Just "List", icon = Just (HMI.Menu, HMT.Stacked)}
                , HH.slot_ "list" () HML.list HML.ListSpec {items, elemRenderer, extraStyle}
                )
              ,
                ( HMT.TabSpec {label = Just "Logout", icon = Just (HMI.Logout, HMT.Stacked)}
                , HH.div
                    [ HP.style $ do
                        C.display C.flex
                        C.flexDirection C.column
                        C.width C.auto
                        C.padding pad pad pad pad
                        extraStyle
                    ]
                    [ HH.slot_ "textField" 0 HMTF.textField $
                        HMTF.emptyTextFieldSpec
                          { HMTF.label = Just "Username"
                          , HMTF.helperLine = HMTF.CharacterCounter
                          , HMTF.minMaxLength = (Nothing, Just 20)
                          }
                    , HH.slot_ "textField" 1 HMTF.textField $
                        HMTF.emptyTextFieldSpec
                          { HMTF.label = Just "Password"
                          , HMTF.type_ = InputPassword
                          , HMTF.helperLine = HMTF.CharacterCounter
                          }
                    , HH.slot_ "textField" 2 HMTF.textField $
                        HMTF.emptyTextFieldSpec
                          { HMTF.label = Just "Donation"
                          , HMTF.prefix = HMTF.TextAffix "$"
                          , HMTF.suffix = HMTF.IconAffix HMI.CreditCard
                          , HMTF.type_ = InputNumber
                          , HMTF.helperLine = HMTF.HelperLine "Any donation helps our cause!"
                          }
                    ]
                )
              ,
                ( HMT.TabSpec {label = Just "Radio Buttons", icon = Just (HMI.Radio, HMT.Stacked)}
                , HH.div
                    [ HP.style $ do
                        C.display C.flex
                        C.flexDirection C.column
                        C.width C.auto
                    ]
                    [ radio 0 $
                        HMR.emptyRadioButtonSpec
                          { HMR.label = "White"
                          , HMR.groupName = "group1"
                          , HMR.checked = True
                          }
                    , radio 1 $
                        HMR.emptyRadioButtonSpec
                          { HMR.label = "Black"
                          , HMR.groupName = "group1"
                          }
                    , radio 2 $
                        HMR.emptyRadioButtonSpec
                          { HMR.label = "Red"
                          , HMR.groupName = "group1"
                          }
                    , radio 3 $
                        HMR.emptyRadioButtonSpec
                          { HMR.label = "Green"
                          , HMR.groupName = "group1"
                          }
                    , HH.p_ [HH.text $ "Chosen colour: " <> colour]
                    ]
                )
              ,
                ( HMT.TabSpec {label = Just "Checkboxes", icon = Just (HMI.CheckBox, HMT.Stacked)}
                , HH.div
                    [ HP.style $ do
                        C.display C.flex
                        C.flexDirection C.column
                        C.width C.auto
                    ]
                    [ HH.slot_ "checkbox" 0 HMC.checkbox $
                        HMC.emptyCheckboxSpec
                          { HMC.label = "English"
                          , HMC.checked = True
                          }
                    , HH.slot_ "checkbox" 1 HMC.checkbox $
                        HMC.emptyCheckboxSpec
                          { HMC.label = "German"
                          }
                    , HH.slot_ "checkbox" 2 HMC.checkbox $
                        HMC.emptyCheckboxSpec
                          { HMC.label = "French"
                          }
                    , HH.slot_ "checkbox" 3 HMC.checkbox $
                        HMC.emptyCheckboxSpec
                          { HMC.label = "Polish"
                          , HMC.checked = True
                          , HMC.enabled = False
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
              [ ("Text", HMB.emptyButtonSpec {HMB.label = "Text button"})
              , ("Text-Icon", HMB.emptyButtonSpec {HMB.label = "Text button with icon", HMB.icon = Just (HMI.Search, HMB.Leading)})
              , ("Outlined", HMB.emptyButtonSpec {HMB.label = "Outlined button", HMB.style = Just HMB.Outlined})
              , ("Outlined-Icon", HMB.emptyButtonSpec {HMB.label = "Outlined button with icon", HMB.style = Just HMB.Outlined, HMB.icon = Just (HMI.Info, HMB.Leading)})
              , ("Contained", HMB.emptyButtonSpec {HMB.label = "Contained button", HMB.style = Just HMB.Raised})
              , ("Contained-Icon", HMB.emptyButtonSpec {HMB.label = "Contained button with icon", HMB.style = Just HMB.Raised, HMB.icon = Just (HMI.Favorite, HMB.Leading)})
              , ("Unelevated", HMB.emptyButtonSpec {HMB.label = "Unelevated button", HMB.style = Just HMB.Unelevated})
              , ("Unelevated-Icon", HMB.emptyButtonSpec {HMB.label = "Unelevated button with icon", HMB.style = Just HMB.Unelevated, HMB.icon = Just (HMI.Settings, HMB.Leading)})
              , ("Disabled", HMB.emptyButtonSpec {HMB.label = "Disabled button", HMB.enabled = False})
              , ("Disabled-Icon", HMB.emptyButtonSpec {HMB.label = "Disabled button with icon", HMB.icon = Just (HMI.Delete, HMB.Leading), HMB.enabled = False})
              , ("Contained-Icon-Trailing", HMB.emptyButtonSpec {HMB.label = "Contained button with trailing icon", HMB.style = Just HMB.Raised, HMB.icon = Just (HMI.Favorite, HMB.Trailing)})
              ]

        elemRenderer :: HML.ElemRenderer (Int, (Text, HMB.ButtonSpec)) Buttons Void m
        elemRenderer =
          HML.ElemRenderer
            { metaRenderer = Just $ \(_, (name, cfg)) -> HH.slot_ "button" name HMB.button cfg
            , textRenderer = HML.Oneline (fst . snd)
            , iconRenderer = Just $ \(idx, _) -> [minBound .. maxBound] !! idx
            }
