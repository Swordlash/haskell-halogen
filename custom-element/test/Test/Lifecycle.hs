-- | What an element's component lives through: its outputs leave as events,
-- it goes when the element is removed but not when it is moved, and a
-- definition taken over or taken back takes it too.
module Test.Lifecycle (spec) where

import Data.Aeson (toJSON)
import Data.IORef (newIORef, readIORef)
import Example.Counter qualified as Counter
import Halogen.CustomElement qualified as CE
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Test.Hspec.Halogen
import Test.Page

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  -- The counter raises 0 from its initializer: an output raised while the
  -- element mounts is an event too, for a page listening before it inserts
  -- the element.
  it "dispatches what the component raises as a CustomEvent, which bubbles to the document, from its initializer on" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-raises" Counter.definition (Counter.counter finalized)
    unsafeIOToPageM (listen "count")
    page <- htmlContainer "<x-ce-raises id=\"raises\"></x-ce-raises>"
    button <- findIn page "button.increment"
    click button
    click button
    eventually $
      unsafeIOToPageM (heard "count")
        `shouldReturn` [Heard {detail = toJSON n, bubbles = True, composed = True, target = "raises"} | n <- [0, 1, 2 :: Int]]
    undefine registration

  it "runs the component's finalizer when the element is removed" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-removed" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-removed id=\"removed\"></x-ce-removed>"
    _ <- findIn page ".count"
    unsafeIOToPageM (remove "removed")
    eventually (unsafeIOToPageM (readIORef finalized) `shouldReturn` 1)
    undefine registration

  it "keeps the component and its state when the element is moved" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-moved" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<div id=\"from\"><x-ce-moved id=\"moved\"></x-ce-moved></div><div id=\"to\"></div>"
    findIn page "button.increment" >>= click
    unsafeIOToPageM (move "moved" "to")
    settle
    findIn page "#to .count" >>= (`shouldHaveText` "1")
    unsafeIOToPageM (readIORef finalized) `shouldReturn` 0
    findIn page "#to button.increment" >>= click
    findIn page "#to .count" >>= (`shouldHaveText` "2")
    undefine registration
    unsafeIOToPageM (readIORef finalized) `shouldReturn` 1

  -- The define here is made without waiting, so that the three steps are
  -- one task: the element is out of the page as its name is defined again
  -- (so the definition does not mount it), and back before its removal is
  -- acted on.
  it "mounts a moved element again when its name was defined again while it was out of the page" $ runPage $ do
    first <- unsafeIOToPageM (newIORef 0)
    second <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-redefined-moved" Counter.definition (Counter.counter first)
    page <- htmlContainer "<div id=\"redefined-from\"><x-ce-redefined-moved id=\"redefined-moved\" label=\"moved\"></x-ce-redefined-moved></div><div id=\"redefined-to\"></div>"
    _ <- findIn page "#redefined-moved .count"
    again <- unsafeIOToPageM $ do
      detach "redefined-moved"
      again <- runTest (CE.define "x-ce-redefined-moved" Counter.definition (Counter.other second) :: m (CE.Registration m))
      reattach "redefined-moved" "redefined-to"
      pure again
    settle
    findIn page "#redefined-to .other" >>= (`shouldHaveText` "other: moved")
    unsafeIOToPageM (readIORef first) `shouldReturn` 1
    undefine registration
    undefine again
    unsafeIOToPageM (readIORef second) `shouldReturn` 1

  it "swaps the component of every element when the name is defined again" $ runPage $ do
    first <- unsafeIOToPageM (newIORef 0)
    second <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-again" Counter.definition (Counter.counter first)
    page <- htmlContainer "<x-ce-again id=\"before\" label=\"before\"></x-ce-again>"
    _ <- findIn page "#before .count"
    again <- defineElement m "x-ce-again" Counter.definition (Counter.other second)
    findIn page "#before .other" >>= (`shouldHaveText` "other: before")
    length <$> findAllIn page "#before .count" `shouldReturn` 0
    unsafeIOToPageM (readIORef first) `shouldReturn` 1
    -- The first registration was taken over: disposing of it does nothing.
    undefine registration
    findIn page "#before .other" >>= (`shouldHaveText` "other: before")
    later <- htmlContainer "<x-ce-again id=\"after\" label=\"after\"></x-ce-again>"
    findIn later ".other" >>= (`shouldHaveText` "other: after")
    undefine again
    unsafeIOToPageM (readIORef second) `shouldReturn` 2

  it "renders nothing once its registration is disposed of, until it is defined again" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-disposed" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-disposed id=\"disposed\"></x-ce-disposed>"
    _ <- findIn page ".count"
    undefine registration
    unsafeIOToPageM (readIORef finalized) `shouldReturn` 1
    unsafeIOToPageM (lightHTML "disposed") `shouldReturn` ""
    later <- htmlContainer "<x-ce-disposed id=\"inert\"></x-ce-disposed>"
    settle
    unsafeIOToPageM (lightHTML "inert") `shouldReturn` ""
    unsafeIOToPageM (call "inert" "count" []) `shouldReturn` Right "null"
    -- Defining it again mounts the elements that waited.
    again <- defineElement m "x-ce-disposed" Counter.definition (Counter.counter finalized)
    findIn later ".count" >>= (`shouldHaveText` "0")
    findIn page "#disposed .count" >>= (`shouldHaveText` "0")
    undefine again
