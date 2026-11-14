{-# LANGUAGE TemplateHaskell #-}

-- | A handler declared for the links inside its element, and handlers
-- declared for what a paste or a drag carries ("Example.Delegated"), in a
-- real browser: which events are theirs, whose default the listener
-- prevents, and what the handler reads of the link.
module Test.Delegated (spec) where

import Example.Delegated qualified as Delegated
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, toJSText)
import Protolude hiding (find)
import Test.Hspec.Halogen

-- Natively the stubs answer False and empty, which is never read: the
-- suite only reports a skip there.
$( browserJS
     ["test/jsbits/delegated.js"]
     [ ("js_click", "halogen_delegated_click", Safe, [t|JSText -> Bool -> Bool -> IO Bool|])
     , ("js_paste", "halogen_delegated_paste", Safe, [t|JSText -> Bool -> IO Bool|])
     , ("js_drag", "halogen_delegated_drag", Safe, [t|JSText -> JSText -> Bool -> IO Bool|])
     , ("js_hash", "halogen_delegated_hash", Safe, [t|IO JSText|])
     ]
 )

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "follows a plain click on a link inside its element, and keeps the browser from it" $ runPage $ do
    ui <- mount m Delegated.component ()
    find ui ".delegated-two" >>= click
    -- A click on an element inside the link is the link's.
    find ui ".delegated-inner" >>= click
    find ui ".delegated-followed" >>= (`shouldHaveText` "#delegated-two #delegated-one")
    -- Neither link was followed by the browser.
    fromJSText <$> unsafeIOToPageM js_hash `shouldReturn` ""

  it "prevents the default of the clicks it is declared for, and of no other" $ runPage $ do
    ui <- mount m Delegated.component ()
    clickOn ".delegated-one" False False `shouldReturn` True
    -- A click whose target is the text inside the link, not an element:
    -- the match starts from the text's parent, and judges the link. (The
    -- handler's own read, 'Element.fromEventTarget', finds no element in a
    -- text node and makes no action: a browser's own clicks target
    -- elements.)
    clickOn ".delegated-inner" True False `shouldReturn` True
    -- A ctrl-click opens a new tab: the browser's.
    clickOn ".delegated-two" False True `shouldReturn` False
    -- A link to another tab is judged on the link, not on its container.
    clickOn ".delegated-blank" False False `shouldReturn` False
    -- Text in the container that is no link.
    clickOn ".delegated-text" False False `shouldReturn` False
    -- The link around the listening element is not inside it.
    clickOn ".delegated-inside" False False `shouldReturn` False
    find ui ".delegated-followed" >>= (`shouldHaveText` "#delegated-one")

  it "matches nothing for a selector that does not parse, and the program goes on" $ runPage $ do
    ui <- mount m Delegated.component ()
    clickOn ".delegated-broken-link" False False `shouldReturn` False
    clickOn ".delegated-two" False False `shouldReturn` True
    find ui ".delegated-followed" >>= (`shouldHaveText` "#delegated-two")

  it "takes a paste of files and leaves one of text to the browser" $ runPage $ do
    ui <- mount m Delegated.component ()
    paste False `shouldReturn` False
    find ui ".delegated-pasted" >>= (`shouldHaveText` "")
    paste True `shouldReturn` True
    find ui ".delegated-pasted" >>= (`shouldHaveText` "a.txt,text/plain,3.0")

  it "takes a drag and a drop of files and leaves one of text to the browser" $ runPage $ do
    ui <- mount m Delegated.component ()
    drag "dragover" False `shouldReturn` False
    drag "drop" False `shouldReturn` False
    find ui ".delegated-dragged" >>= (`shouldHaveText` "0")
    drag "dragover" True `shouldReturn` True
    drag "drop" True `shouldReturn` True
    find ui ".delegated-dragged" >>= (`shouldHaveText` "1")
    find ui ".delegated-dropped" >>= (`shouldHaveText` "a.txt,text/plain,3.0")
  where
    clickOn selector text ctrl = do
      prevented <- unsafeIOToPageM (js_click (toJSText selector) text ctrl)
      settle
      pure prevented
    paste file = do
      prevented <- unsafeIOToPageM (js_paste (toJSText ".delegated-area") file)
      settle
      pure prevented
    drag ty file = do
      prevented <- unsafeIOToPageM (js_drag (toJSText ".delegated-area") (toJSText ty) file)
      settle
      pure prevented
