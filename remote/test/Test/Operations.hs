-- | What a component does to the page through the @Web.*@ types, recorded
-- as operations for the client: a focus, a dialog opened and closed, an
-- element scrolled into view, the title; and what is not recorded.
--
-- Natively only: in a browser (under Node, on both browser backends) a
-- handle is the engine's value and answers nothing, as in the loopback.
module Test.Operations (spec) where

import Data.Aeson (Value (..), object, (.=))
import Halogen.JSBits (inBrowser)
import Halogen.Remote.Internal (runRemoteDOM)
import Halogen.Remote.Internal qualified as Internal
import Halogen.Remote.Protocol (NodeId, Op (..))
import Halogen.Remote.Session qualified as Session
import Halogen.VDom.DOM.Monad (MonadAttributes (..), MonadBrowserDOM (..), MonadDOM (..))
import Halogen.VDom.Types (ElemName (..))
import Protolude
import Test.Components
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy, xdescribe)
import Test.Session (clickOn, elementWithId, listenerOn, mounted, nextOps, required, silent)
import Web.DOM.Internal.Types (HTMLElement (..), Node (..))
import Web.HTML.Common (AttrName (..))
import Web.HTML.HTMLElement qualified as HTMLElement

-- | Click a button the first render made, by its id.
clicking :: Session.Session -> [Op] -> Text -> IO ()
clicking session ops ident = do
  button <- required ident (elementWithId ops ident)
  listener <- required (ident <> "'s listener") (listenerOn ops button "click")
  Session.receive session (clickOn listener button)

-- | Where the node was made, in the operations.
madeAt :: [Op] -> NodeId -> Maybe Int
madeAt ops node = indexWhere (\case Create n _ _ -> n == node; _ -> False) ops

-- | Where the first operation that satisfies the test is.
indexWhere :: (Op -> Bool) -> [Op] -> Maybe Int
indexWhere test ops = head [i | (i, op) <- zip [0 ..] ops, test op]

spec :: Spec
spec = (if inBrowser then xdescribe else describe) "what a component does to the page through the Web types" $ do
  it "focuses in Initialize, after the element is made, in the first batch" $ do
    (session, _, ops, _) <- mounted operating
    first' <- required "the first field" (elementWithId ops "first")
    let focused = indexWhere (== Call first' "focus" []) ops
    focused `shouldSatisfy` isJust
    (>) <$> focused <*> madeAt ops first' `shouldBe` Just True
    -- Only that one.
    length [() | Call {} <- ops] `shouldBe` 1
    Session.dispose session

  it "records a focus, a dialog opened and closed, and a scroll, each after the click that asked" $ do
    (session, wire, ops, _) <- mounted operating
    second' <- required "the second field" (elementWithId ops "second")
    dialog <- required "the dialog" (elementWithId ops "dialog")
    far <- required "the far element" (elementWithId ops "far")
    clicking session ops "focus-second"
    nextOps wire `shouldReturn` [Call second' "focus" []]
    clicking session ops "focus-quietly"
    nextOps wire `shouldReturn` [Call second' "focus" [object ["preventScroll" .= True]]]
    clicking session ops "open"
    nextOps wire `shouldReturn` [Call dialog "showModal" []]
    clicking session ops "close"
    nextOps wire `shouldReturn` [Call dialog "close" []]
    clicking session ops "scroll"
    nextOps wire `shouldReturn` [Call far "scrollIntoView" [object ["block" .= ("center" :: Text)]]]
    Session.dispose session

  it "records nothing for a node that has gone, or a method not on the list" $ do
    (session, wire, ops, _) <- mounted operating
    doomed <- required "the doomed field" (elementWithId ops "doomed")
    clicking session ops "forget"
    gone <- nextOps wire
    [c | Remove _ c <- gone] `shouldBe` [doomed]
    clicking session ops "focus-forgotten"
    silent wire
    clicking session ops "not-listed"
    silent wire
    Session.dispose session

  it "keeps a call made on a node that is taken out before the batch ends, before its removal" $ do
    -- A link clicked through its ref and hidden by the same action: the
    -- click is the page's to perform (a download starts), whether or not a
    -- batch was taken in between.
    recorder <- Internal.newRecorder
    let HTMLElement bodyValue = Internal.bodyHandle recorder
        body = Node bodyValue
    anchor <- runRemoteDOM recorder $ do
      made' <- createElement Nothing (ElemName "a") (Internal.documentHandle recorder)
      elementToNode made' >>= (`appendChild` body)
      pure made'
    opening <- Internal.takeBatch recorder
    made <- required "the link" (head [n | Create n _ "a" <- fold opening])
    clickable <- required "the link as an HTML element" (HTMLElement.fromElement anchor)
    HTMLElement.click clickable
    runRemoteDOM recorder (elementToNode anchor >>= (`removeChild` body))
    Internal.takeBatch recorder `shouldReturn` Just [Call made "click" [], Remove 1 made]
    -- Forgotten now: a call on it is nobody's.
    HTMLElement.click clickable
    Internal.takeBatch recorder `shouldReturn` Just []

  it "keeps what the batch said of such a node before the call, and leaves out what it said after" $ do
    -- A link made, given its address, clicked and taken out again by one
    -- action: the click must find the address.
    recorder <- Internal.newRecorder
    let HTMLElement bodyValue = Internal.bodyHandle recorder
        body = Node bodyValue
    runRemoteDOM recorder $ do
      anchor <- createElement Nothing (ElemName "a") (Internal.documentHandle recorder)
      node <- elementToNode anchor
      appendChild node body
      setAttribute Nothing (AttrName "href") "/report.csv" anchor
      liftIO (for_ (HTMLElement.fromElement anchor) HTMLElement.click)
      setAttribute Nothing (AttrName "class") "done" anchor
      removeChild node body
    batch <- Internal.takeBatch recorder
    made <- required "the link" (head [n | Create n _ "a" <- fold batch])
    fmap (dropWhile (/= Create made Nothing "a")) batch
      `shouldBe` Just
        [ Create made Nothing "a"
        , Insert 1 0 made
        , SetAttribute made Nothing "href" "/report.csv"
        , Call made "click" []
        , Remove 1 made
        ]

  it "sets the document's title, from Initialize and from a click" $ do
    (session, wire, ops, _) <- mounted titling
    [o | o@Assign {} <- ops] `shouldBe` [Assign 0 "title" (String "First title")]
    clicking session ops "retitle"
    nextOps wire `shouldReturn` [Assign 0 "title" (String "Second title")]
    Session.dispose session
