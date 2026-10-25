module Test.Editing (spec) where

import Data.Text qualified as T
import Example.Editing qualified as Editing
import Halogen.JSBits.Promise (JSError (..))
import Protolude hiding (find)
import Test.Hspec (shouldThrow)
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

  it "takes an emoji back out whole with Backspace" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.emoji"
    typeText field "a😀"
    find ui ".smiled" >>= (`shouldHaveText` "a😀")
    press "Backspace"
    getProperty field "value" `shouldReturn` "a"
    find ui ".smiled" >>= (`shouldHaveText` "a")

  it "does not submit a form by Enter when its submit button is disabled" $ runPage $ do
    ui <- mount m Editing.component ()
    find ui "input.locked-field" >>= (`typeText` "x")
    press "Enter"
    settle
    find ui ".locked-submits" >>= (`shouldHaveText` "0")

  it "submits a form by Enter through its submit button" $ runPage $ do
    ui <- mount m Editing.component ()
    find ui "input.order-field" >>= (`typeText` "tea")
    press "Enter"
    find ui ".order-clicks" >>= (`shouldHaveText` "1")
    find ui ".order-submits" >>= (`shouldHaveText` "1")

  it "does not put half of an emoji into a field it does not fit" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.single"
    typeText field "😀"
    getProperty field "value" `shouldReturn` ""

  -- Five seconds, the runner's timeout for an action, before it fails.
  it "does not click a button hidden by visibility, and fails as the runner does" $
    runPage (mount m Editing.component () >>= (`find` "button.invisible") >>= click)
      `shouldThrow` \(JSError _ message) -> "Timeout 5000ms exceeded" `T.isInfixOf` message

  -- What the actions wait for is what isVisible reports: visibility: hidden
  -- hides an element from both.
  it "counts an element hidden by visibility as hidden" $ runPage $ do
    ui <- mount m Editing.component ()
    invisible <- find ui "button.invisible"
    isVisible invisible `shouldReturn` False
    shouldBeHidden invisible
    find ui "button.tab-first" >>= shouldBeVisible

  -- Five seconds too: a read-only field never becomes editable.
  it "does not clear a read-only field, and fails as the runner does" $
    runPage (mount m Editing.component () >>= (`find` "input.readonly") >>= clear)
      `shouldThrow` \(JSError _ message) -> "Timeout 5000ms exceeded" `T.isInfixOf` message

  it "tabs in tabindex order, past tabindex -1" $ runPage $ do
    ui <- mount m Editing.component ()
    find ui "button.tab-first" >>= focus
    find ui ".focused" >>= (`shouldHaveText` "first")
    press "Tab"
    find ui ".focused" >>= (`shouldHaveText` "second")
    press "Shift+Tab"
    find ui ".focused" >>= (`shouldHaveText` "first")

  it "takes the focus away by a click on what cannot have it" $ runPage $ do
    ui <- mount m Editing.component ()
    find ui "button.tab-first" >>= focus
    find ui ".focused" >>= (`shouldHaveText` "first")
    find ui ".focused" >>= click
    find ui ".focused" >>= (`shouldHaveText` "none")

  it "presses a button with the key Playwright calls Space" $ runPage $ do
    ui <- mount m Editing.component ()
    find ui "button.tab-first" >>= focus
    press "Space"
    find ui ".focused" >>= (`shouldHaveText` "pressed first")

  it "types no more than a field's maxlength" $ runPage $ do
    ui <- mount m Editing.component ()
    field <- find ui "input.short"
    typeText field "abcdef"
    getProperty field "value" `shouldReturn` "abc"
