-- | How a render reconciles the page with the HTML it renders: an element is
-- kept and patched where the new HTML still has it, and replaced only where
-- the element itself changed; a property is written only when its value did.
module Test.Reconcile (spec) where

import Example.Reconcile qualified as Reconcile
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  let press' ui name = find ui ("button." <> name) >>= click

  it "keeps every element a render leaves as it was" $ runPage $ do
    ui <- mount m Reconcile.component ()
    before <- traverse (find ui) [".items li", ".label", ".free", ".target", ".styled", ".titled"]
    press' ui "rerender"
    find ui ".renders" >>= (`shouldHaveText` "1")
    after <- traverse (find ui) [".items li", ".label", ".free", ".target", ".styled", ".titled"]
    zipWithM_ shouldBeSameElement after before

  it "adds a list item without touching the ones before it" $ runPage $ do
    ui <- mount m Reconcile.component ()
    [firstItem] <- findAll ui ".items li"
    press' ui "grow"
    press' ui "grow"
    items@(stillFirst : _) <- findAll ui ".items li"
    stillFirst `shouldBeSameElement` firstItem
    traverse textContent items `shouldReturn` ["item 1", "item 2", "item 3"]

  it "rewrites a text where it is, keeping its element" $ runPage $ do
    ui <- mount m Reconcile.component ()
    label <- find ui ".label"
    press' ui "relabel"
    label `shouldHaveText` "second"
    find ui ".label" >>= (`shouldBeSameElement` label)

  it "leaves alone what the page put in an element the component does not control" $ runPage $ do
    ui <- mount m Reconcile.component ()
    free <- find ui ".free"
    typeText free "typed by hand"
    press' ui "rerender"
    getProperty free "value" `shouldReturn` "typed by hand"

  it "writes a property when its value changes" $ runPage $ do
    ui <- mount m Reconcile.component ()
    target <- find ui ".target"
    getProperty target "disabled" `shouldReturn` "false"
    press' ui "toggle-disabled"
    getProperty target "disabled" `shouldReturn` "true"
    press' ui "rerender"
    getProperty target "disabled" `shouldReturn` "true"
    press' ui "toggle-disabled"
    getProperty target "disabled" `shouldReturn` "false"

  it "clears a property it no longer renders" $ runPage $ do
    ui <- mount m Reconcile.component ()
    titled <- find ui ".titled"
    press' ui "toggle-title"
    getProperty titled "title" `shouldReturn` "a title"
    press' ui "toggle-title"
    getProperty titled "title" `shouldReturn` ""

  it "replaces an element whose tag changes" $ runPage $ do
    ui <- mount m Reconcile.component ()
    italic <- find ui ".styled"
    getProperty italic "tagName" `shouldReturn` "I"
    press' ui "toggle-tag"
    isAttached italic `shouldReturn` False
    find ui ".styled" >>= \bold -> getProperty bold "tagName" `shouldReturn` "B"

  it "keeps a child component, and what it holds, across its parent's renders" $ runPage $ do
    ui <- mount m Reconcile.component ()
    increment <- find ui ".increment"
    click increment
    click increment
    press' ui "rerender"
    press' ui "grow"
    find ui ".increment" >>= (`shouldBeSameElement` increment)
    find ui ".count" >>= (`shouldHaveText` "2")
