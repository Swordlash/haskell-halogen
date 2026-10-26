-- | The recorder without a session: when a node taken out is forgotten, and
-- when a batch may be taken.
module Test.Recorder (spec) where

import Halogen.Remote.Internal (Recorder, RemoteDOM, runRemoteDOM)
import Halogen.Remote.Internal qualified as Internal
import Halogen.Remote.Protocol (NodeId, Op (..))
import Halogen.VDom.DOM.Monad (MonadBrowserDOM (..), MonadDOM (..))
import Halogen.VDom.Types (ElemName (..))
import Protolude
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import Web.DOM.Internal.Types (Element, HTMLElement (..), Node (..))
import Web.Event.Event (EventType (..))

-- | The body, as a node to put things in.
bodyOf :: Recorder -> Node
bodyOf recorder = let HTMLElement value = Internal.bodyHandle recorder in Node value

element :: Recorder -> Text -> RemoteDOM Element
element recorder name = createElement Nothing (ElemName name) (Internal.documentHandle recorder)

-- | The number the batch made an element with this name under.
created :: [Op] -> Text -> IO NodeId
created ops name = case [n | Create n _ tag <- ops, tag == name] of
  [n] -> pure n
  _ -> expectationFailure ("the batch did not create one " <> toS name) >> panic "unreachable"

taken :: Recorder -> IO [Op]
taken recorder = Internal.takeBatch recorder >>= maybe (expectationFailure "the recorder is closed" >> panic "unreachable") pure

spec :: Spec
spec = describe "the recorder" $ do
  it "forgets a node taken out when the batch is taken, not before, and not one put back" $ do
    recorder <- Internal.newRecorder
    let body = bodyOf recorder
    (outer, inner, moved) <- runRemoteDOM recorder $ do
      outer <- elementToNode =<< element recorder "div"
      inner <- element recorder "span"
      moved <- elementToNode =<< element recorder "p"
      innerNode <- elementToNode inner
      appendChild outer body
      appendChild innerNode outer
      appendChild moved outer
      listener <- mkEventListener (const pass)
      target <- elementToEventTarget inner
      addEventListener (EventType "click") listener target
      pure (outer, inner, moved)
    opening <- taken recorder
    [outerId, _, movedId] <- traverse (created opening) ["div", "span", "p"]
    Internal.liveNodes recorder `shouldReturn` 5
    Internal.liveListeners recorder `shouldReturn` 1
    runRemoteDOM recorder $ do
      innerNode <- elementToNode inner
      removeChild outer body
      -- What halting the removed div does: its children taken out of it,
      -- their listeners stopped.
      removeChild innerNode outer
      listener <- mkEventListener (const pass)
      target <- elementToEventTarget inner
      removeEventListener (EventType "click") listener target
      -- And a node moved out of it before the batch ends.
      appendChild moved body
    -- Taken out, but the batch is not over: nothing is forgotten yet.
    Internal.liveNodes recorder `shouldReturn` 5
    patch <- taken recorder
    -- The div's removal and the paragraph's move; nothing about what the
    -- client forgets with the div.
    patch `shouldBe` [Remove 1 outerId, Insert 1 0 movedId]
    -- The document, the body and the paragraph.
    Internal.liveNodes recorder `shouldReturn` 3
    Internal.liveListeners recorder `shouldReturn` 0
    -- What is forgotten is named by nothing more.
    runRemoteDOM recorder (elementToNode inner >>= (`appendChild` body))
    taken recorder `shouldReturn` []

  it "takes no batch while a render is under way" $ do
    recorder <- Internal.newRecorder
    inside <- newEmptyMVar
    release <- newEmptyMVar
    _ <- forkIO $ runRemoteDOM recorder $ Internal.whileRendering $ do
      _ <- element recorder "div"
      liftIO (putMVar inside () >> takeMVar release)
      void (element recorder "span")
    takeMVar inside
    batch <- newEmptyMVar
    _ <- forkIO (taken recorder >>= putMVar batch)
    threadDelay 100000
    tryReadMVar batch >>= (`shouldSatisfy` isNothing)
    putMVar release ()
    ops <- takeMVar batch
    [name | Create _ _ name <- ops] `shouldBe` ["div", "span"]

  it "takes the render that failed, once it has" $ do
    recorder <- Internal.newRecorder
    failed <- try (runRemoteDOM recorder (Internal.whileRendering (element recorder "div" >> liftIO (throwIO (ErrorCall "the render failed")))))
    either (\(ErrorCall _) -> True) (const False) failed `shouldBe` True
    ops <- taken recorder
    [name | Create _ _ name <- ops] `shouldBe` ["div"]
