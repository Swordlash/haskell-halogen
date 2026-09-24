module Test.Tabs (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Material.Monad (MonadMaterial)
import Halogen.Material.Tabs qualified as HMT
import Halogen.Material.TextField qualified as HMTF
import Protolude hiding (find)
import Test.Hspec.Halogen

type Slots = ("textField" .== H.Slot HMTF.TextFieldQuery HMTF.TextFieldOutput Int)

-- | Two tabs: some text, then a text field whose content should survive
-- switching away and back.
twoTabs :: (MonadMaterial m, MonadUUID m) => HMT.TabsSpec Slots Void m
twoTabs =
  HMT.emptyTabsSpec
    { HMT.tabs =
        (HMT.TabSpec {label = Just "First", icon = Nothing}, HH.text "first")
          :| [
               ( HMT.TabSpec {label = Just "Second", icon = Nothing}
               , HH.slot_ "textField" 0 HMTF.textField HMTF.emptyTextFieldSpec {HMTF.label = Just "Name"}
               )
             ]
    }

tabBar :: forall m -> (MonadBrowserTest m, MonadMaterial m) => PageM s (Mounted s (HMT.TabsQuery Slots HMTF.TextFieldQuery) (HMT.TabsOutput Void) m)
tabBar m = mount m HMT.tabsComponent twoTabs

selectTab :: Mounted s q o m -> Int -> PageM s ()
selectTab ui n = find ui (".mdc-tab:nth-child(" <> show n <> ")") >>= click

spec :: forall m -> (MonadBrowserTest m, MonadMaterial m) => Spec
spec m = do
  it "shows only the selected tab" $ runPage $ do
    ui <- tabBar m
    [firstPanel, secondPanel] <- findAll ui "[role=tabpanel]"
    shouldBeVisible firstPanel
    shouldBeHidden secondPanel
    selectTab ui 2
    shouldBeHidden firstPanel
    shouldBeVisible secondPanel

  it "keeps what was typed in a tab across a switch" $ runPage $ do
    ui <- tabBar m
    selectTab ui 2
    input <- find ui "input"
    typeText input "alice"
    selectTab ui 1
    selectTab ui 2
    getProperty input "value" `shouldReturn` "alice"
    query ui (HMT.ParentQuery (Proxy @"textField") 0 (HMTF.GetText identity)) `shouldReturn` Just "alice"

  it "raises and answers with the tab the user selects" $ runPage $ do
    ui <- tabBar m
    selectTab ui 2
    outputs ui >>= (`shouldBe` [1]) . selections
    query ui (HMT.GetSelectedTab identity) `shouldReturn` Just 1
  where
    selections os = [n | HMT.SelectedTab n <- os]
