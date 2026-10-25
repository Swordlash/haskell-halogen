module Test.Editing (spec) where

import Example.Editing qualified as Editing
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "does not press a button by a key whose keydown was prevented" $ runPage $ do
    ui <- mount m Editing.component ()
    button <- find ui "button.guarded"
    focus button
    press " "
    press "Enter"
    find ui ".clicks" >>= (`shouldHaveText` "0")
    click button
    find ui ".clicks" >>= (`shouldHaveText` "1")

  it "types into an element edited in place, and Backspace takes a character out" $ runPage $ do
    ui <- mount m Editing.component ()
    editable <- find ui ".note"
    typeText editable "abc"
    editable `shouldHaveText` "abc"
    press "Backspace"
    editable `shouldHaveText` "ab"
    find ui ".edits" >>= (`shouldHaveText` "4")

  it "types a negative and a fractional number into a number field" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.number"
    typeText field "-42"
    getProperty field "value" `shouldReturn` "-42"
    find ui ".typed" >>= (`shouldHaveText` "-42")
    clear field
    typeText field "1.5"
    getProperty field "value" `shouldReturn` "1.5"
    find ui ".typed" >>= (`shouldHaveText` "1.5")

  it "takes a character back out of a number field with Backspace" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.number"
    typeText field "-"
    press "Backspace"
    typeText field "4"
    getProperty field "value" `shouldReturn` "4"

  it "leaves a read-only field's value as it was" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.readonly"
    focus field
    press "Backspace"
    press "x"
    getProperty field "value" `shouldReturn` "fixed"

  it "leaves out a character whose keypress was prevented" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.no-x"
    typeText field "axb"
    getProperty field "value" `shouldReturn` "ab"

  it "leaves a field as it was when its beforeinput is prevented" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.refuses"
    typeText field "abc"
    getProperty field "value" `shouldReturn` ""

  it "types no more than a field's maxlength" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.short"
    typeText field "abcdef"
    getProperty field "value" `shouldReturn` "abc"
