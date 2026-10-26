-- | A session whose wire is a list: what mounting, events and disposal send,
-- read as operations.
module Test.Session
  ( spec
  , Wire
  , newWire
  , nextOps
  , elementWithId
  , listenerOn
  , clickOn
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as Lazy
import Halogen qualified as H
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Protocol (Batch (..), EventMessage (..), ListenerId, NodeId, Op (..))
import Halogen.Remote.Session qualified as Session
import Halogen.VDom.DOM.Monad (PropScalar (..))
import Protolude
import Test.Components
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)

-- | What the session has sent and the test not yet read.
newtype Wire = Wire (MVar [Batch])

newWire :: IO (Wire, Text -> IO ())
newWire = do
  sent <- newMVar []
  pure (Wire sent, \text -> for_ (Aeson.decodeStrictText text) (\batch -> modifyMVar_ sent (pure . (<> [batch]))))

-- | The operations of what the session sends next: the first message, which
-- is waited for, and whatever follows it at once (a render may be cut in
-- two), in order.
nextOps :: Wire -> IO [Op]
nextOps (Wire sent) = tries (500 :: Int)
  where
    tries 0 = expectationFailure "the session sent nothing" $> []
    tries n =
      readMVar sent >>= \case
        [] -> threadDelay 10000 >> tries (n - 1)
        _ -> do
          threadDelay 50000
          concatMap (.ops) <$> modifyMVar sent (\batches -> pure ([], batches))

-- | That the session sends nothing for a while.
silent :: Wire -> IO ()
silent (Wire sent) = do
  threadDelay 200000
  readMVar sent >>= (`shouldBe` [])

-- | The element the session made with an id, in what was sent.
elementWithId :: [Op] -> Text -> Maybe NodeId
elementWithId ops ident = head [n | SetProperty n "id" (ScalarText t) <- ops, t == ident]

-- | The listener a node listens with for events of a type, in what was sent.
listenerOn :: [Op] -> NodeId -> Text -> Maybe ListenerId
listenerOn ops node ty = head [l | Listen n t l <- ops, n == node, t == ty]

-- | An event message, as the client sends one.
eventFor :: ListenerId -> NodeId -> Text -> Value -> Text
eventFor listener node ty event = Lazy.toStrict (encodeToLazyText EventMessage {listener, node, eventType = ty, event})

clickOn :: ListenerId -> NodeId -> Text
clickOn listener node = eventFor listener node "click" (object ["type" .= ("click" :: Text), "button" .= (0 :: Int)])

-- | What the client says of an input event: the input holds the text.
inputOn :: ListenerId -> NodeId -> Text -> Text
inputOn listener node typed =
  eventFor listener node "input" $
    object
      [ "type" .= ("input" :: Text)
      , "target" .= object ["value" .= typed, "tagName" .= ("INPUT" :: Text)]
      , "currentTarget" .= object ["value" .= typed, "tagName" .= ("INPUT" :: Text)]
      ]

-- | A component mounted in a session of its own, and the first message.
mounted :: forall q. H.Component q () Void RemoteDOM -> IO (Session.Session, Wire, [Op], H.HalogenSocket q Void RemoteDOM)
mounted component = do
  (wire, send) <- newWire
  session <- Session.newSession send
  socket <- Session.mount session component ()
  ops <- nextOps wire
  pure (session, wire, ops, socket)

-- | A query of the mounted component.
queryOf :: H.HalogenSocket q o RemoteDOM -> q a -> RemoteDOM (Maybe a)
queryOf H.HalogenSocket {H.query = send} = send

required :: Text -> Maybe a -> IO a
required what = maybe (expectationFailure ("no " <> toS what <> " was sent") >> panic "unreachable") pure

spec :: Spec
spec = describe "a session" $ do
  it "sends the first render, after a clear, as one batch" $ do
    (session, _, ops, _) <- mounted counter
    head ops `shouldBe` Just Clear
    [name | Create _ Nothing name <- ops] `shouldBe` ["div", "button", "span"]
    [t | CreateText _ t <- ops] `shouldBe` ["+", "0"]
    button <- required "button" (elementWithId ops "increment")
    count <- required "count" (elementWithId ops "count")
    root <- required "div" (head [n | Create n _ "div" <- ops])
    -- The children are put in order, and the root in the body last.
    [(p, ix, c) | Insert p ix c <- ops, p == root] `shouldBe` [(root, 0, button), (root, 1, count)]
    lastMay [(p, ix, c) | Insert p ix c <- ops] `shouldBe` Just (1, 0, root)
    listenerOn ops button "click" `shouldSatisfy` isJust
    Session.dispose session

  it "sends only what a click changes" $ do
    (session, wire, ops, socket) <- mounted counter
    button <- required "button" (elementWithId ops "increment")
    listener <- required "listener" (listenerOn ops button "click")
    countText <- required "count's text" (head [n | CreateText n "0" <- ops])
    Session.receive session (clickOn listener button)
    nextOps wire `shouldReturn` [SetText countText "1"]
    Session.run session (queryOf socket (GetCount identity)) `shouldReturn` Just 1
    Session.dispose session

  it "delivers the value an input event reports" $ do
    (session, wire, ops, socket) <- mounted echo
    field <- required "input" (elementWithId ops "field")
    listener <- required "listener" (listenerOn ops field "input")
    Session.receive session (inputOn listener field "hi")
    patch <- nextOps wire
    [t | SetText _ t <- patch] `shouldBe` ["hi"]
    Session.run session (queryOf socket (GetText identity)) `shouldReturn` Just "hi"
    Session.dispose session

  it "removes a child that goes, and forgets its nodes and listeners" $ do
    (session, wire, ops, _) <- mounted toggling
    toggle <- required "toggle" (elementWithId ops "toggle")
    poke <- required "the child's button" (elementWithId ops "poke")
    listener <- required "listener" (listenerOn ops toggle "click")
    pokeListener <- required "the child's listener" (listenerOn ops poke "click")
    nodes <- Session.liveNodes session
    Session.liveListeners session `shouldReturn` 2
    Session.receive session (clickOn listener toggle)
    patch <- nextOps wire
    [c | Remove _ c <- patch] `shouldBe` [poke]
    [t | CreateText _ t <- patch] `shouldBe` ["hidden"]
    Session.liveListeners session `shouldReturn` 1
    -- The button and its text went, the text that says so came.
    Session.liveNodes session `shouldReturn` (nodes - 1)
    -- An event for the child's listener, which was in flight, is dropped.
    Session.receive session (clickOn pokeListener poke)
    Session.receive session (clickOn listener toggle)
    again <- nextOps wire
    [t | CreateText _ t <- again] `shouldBe` ["poked 0"]
    Session.dispose session

  it "keeps a child whose node moves into the element that replaces its parent's" $ do
    (session, wire, ops, _) <- mounted swapping
    swapper <- required "swap" (elementWithId ops "swap")
    swapListener <- required "swap's listener" (listenerOn ops swapper "click")
    poke <- required "the child's button" (elementWithId ops "poke")
    pokeListener <- required "the child's listener" (listenerOn ops poke "click")
    pokeText <- required "the child's text" (head [n | CreateText n "poked 0" <- ops])
    oldWrapper <- required "the div around the child" (elementWithId ops "wrapper")
    nodes <- Session.liveNodes session
    Session.receive session (clickOn swapListener swapper)
    patch <- nextOps wire
    wrapper <- required "the section around the child" (elementWithId patch "wrapper")
    [name | Create n _ name <- patch, n == wrapper] `shouldBe` ["section"]
    -- The child's node, the one already on the page, goes into the section,
    -- and no second child is made.
    [(p, c) | Insert p _ c <- patch, c == poke] `shouldBe` [(wrapper, poke)]
    [name | Create _ _ name <- patch] `shouldBe` ["section"]
    [c | Remove _ c <- patch] `shouldBe` [oldWrapper]
    Session.liveNodes session `shouldReturn` nodes
    Session.liveListeners session `shouldReturn` 2
    -- The child is still there, and still heard.
    Session.receive session (clickOn pokeListener poke)
    nextOps wire `shouldReturn` [SetText pokeText "poked 1"]
    -- And back into a div.
    Session.receive session (clickOn swapListener swapper)
    back <- nextOps wire
    div' <- required "the new div around the child" (elementWithId back "wrapper")
    [(p, c) | Insert p _ c <- back, c == poke] `shouldBe` [(div', poke)]
    Session.receive session (clickOn pokeListener poke)
    nextOps wire `shouldReturn` [SetText pokeText "poked 2"]
    Session.liveNodes session `shouldReturn` nodes
    Session.dispose session

  it "runs a handler that prevents the default and stops propagation" $ do
    (session, wire, ops, _) <- mounted guardedForm
    formNode <- required "form" (elementWithId ops "form")
    listener <- required "listener" (listenerOn ops formNode "submit")
    Session.receive session (eventFor listener formNode "submit" (object ["type" .= ("submit" :: Text)]))
    patch <- nextOps wire
    [t | SetText _ t <- patch] `shouldBe` ["1"]
    Session.dispose session

  it "drops an event for a listener it does not have, and a message that is none" $ do
    (session, wire, _, socket) <- mounted counter
    Session.receive session (clickOn 999 999)
    Session.receive session "not json"
    Session.receive session "{\"listener\": 1}"
    silent wire
    Session.run session (queryOf socket (GetCount identity)) `shouldReturn` Just 0
    Session.dispose session

  it "sends nothing once disposed of, and forgets its listeners" $ do
    (session, wire, ops, _) <- mounted counter
    button <- required "button" (elementWithId ops "increment")
    listener <- required "listener" (listenerOn ops button "click")
    Session.dispose session
    Session.receive session (clickOn listener button)
    silent wire
    Session.liveListeners session `shouldReturn` 0
    Session.dispose session

  it "mounts one component" $ do
    (session, _, _, _) <- mounted counter
    (Session.mount session counter () $> False) `catch` (\Session.AlreadyMounted -> pure True) `shouldReturn` True
    Session.dispose session
