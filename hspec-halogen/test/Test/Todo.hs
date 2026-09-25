module Test.Todo (spec) where

import Example.Todo qualified as Todo
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen

type Todo s m = Mounted s Todo.Query Void m

todoList :: forall m -> (MonadBrowserTest m) => PageM s (Todo s m)
todoList m = mount m Todo.component ()

-- | Type an item and press Enter.
add :: Todo s m -> Text -> PageM s ()
add ui title = do
  draft <- find ui ".draft"
  typeText draft title
  press "Enter"

titles :: (MonadBrowserTest m) => Todo s m -> PageM s (Maybe [Text])
titles ui = query ui (H.mkRequest Todo.GetTitles)

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "adds what was typed when Enter is pressed, and clears the field" $ runPage $ do
    ui <- todoList m
    add ui "buy milk"
    find ui ".items .title" >>= (`shouldHaveText` "buy milk")
    find ui ".draft" >>= \draft -> getProperty draft "value" `shouldReturn` ""

  it "adds with the button too, but nothing blank" $ runPage $ do
    ui <- todoList m
    draft <- find ui ".draft"
    typeText draft "   "
    find ui ".add" >>= click
    typeText draft "walk the dog"
    find ui ".add" >>= click
    titles ui `shouldReturn` Just ["walk the dog"]

  it "counts what is left as items are ticked off" $ runPage $ do
    ui <- todoList m
    add ui "one"
    add ui "two"
    find ui ".left" >>= (`shouldHaveText` "2 left")
    [firstRow, _] <- findAll ui ".items li"
    findIn firstRow ".toggle" >>= click
    firstRow `shouldHaveClass` "done"
    find ui ".left" >>= (`shouldHaveText` "1 left")

  it "deletes an item" $ runPage $ do
    ui <- todoList m
    add ui "one"
    add ui "two"
    [firstRow, _] <- findAll ui ".items li"
    findIn firstRow ".remove" >>= click
    titles ui `shouldReturn` Just ["two"]

  it "moves an item's own element when it moves up" $ runPage $ do
    ui <- todoList m
    add ui "one"
    add ui "two"
    [_, secondRow] <- findAll ui ".items li"
    noteField <- findIn secondRow ".note"
    typeText noteField "from the corner shop"
    findIn secondRow ".up" >>= click
    titles ui `shouldReturn` Just ["two", "one"]
    -- The note is in the first row now, still holding what was typed: the
    -- row's element moved, rather than the rows' contents being rewritten.
    [firstRow, _] <- findAll ui ".items li"
    findIn firstRow ".title" >>= (`shouldHaveText` "two")
    findIn firstRow ".note" >>= \moved -> getProperty moved "value" `shouldReturn` "from the corner shop"
