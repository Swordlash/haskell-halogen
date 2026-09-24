module Test.TextField (spec) where

import Halogen.Material.Monad (MonadMaterial)
import Halogen.Material.TextField qualified as HMTF
import Protolude hiding (find)
import Test.Hspec
import Test.Hspec.Halogen

floated :: Text
floated = "mdc-floating-label--float-above"

spec :: forall m -> (MonadBrowserTest m, MonadMaterial m) => Spec
spec m = do
  it "raises what is typed, one keystroke at a time" $ withPage $ \page -> do
    ui <- mount page m HMTF.textField HMTF.emptyTextFieldSpec {HMTF.label = Just "Name"}
    find ui "input" >>= (`typeText` "ab")
    outputs ui >>= (`shouldBe` ["a", "ab"]) . map (\(HMTF.InputChanged t) -> t)
    query ui (HMTF.GetText identity) `shouldReturn` Just "ab"

  it "keeps the label up while a cleared field has focus" $ withPage $ \page -> do
    ui <- mount page m HMTF.textField HMTF.emptyTextFieldSpec {HMTF.label = Just "Name"}
    input <- find ui "input"
    label <- find ui ".mdc-floating-label"
    label `shouldNotHaveClass` floated
    click input
    label `shouldHaveClass` floated
    typeText input "ab"
    press page "Backspace"
    press page "Backspace"
    getProperty input "value" `shouldReturn` ""
    label `shouldHaveClass` floated
    blur input
    label `shouldNotHaveClass` floated

  it "opens an outlined field's notch around a preset value" $ withPage $ \page -> do
    ui <-
      mount
        page
        m
        HMTF.textField
        HMTF.emptyTextFieldSpec
          { HMTF.label = Just "Email"
          , HMTF.text = "someone@example.com"
          , HMTF.style = HMTF.Outlined
          }
    find ui ".mdc-floating-label" >>= (`shouldHaveClass` floated)
    find ui ".mdc-notched-outline" >>= (`shouldHaveClass` "mdc-notched-outline--notched")
