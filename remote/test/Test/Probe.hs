-- | The probe's questions and answers through a session driven by hand, and
-- the session's identity: what the page is told, in what order, and what a
-- session counts as unsent.
module Test.Probe (spec) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Remote.Protocol (Batch (..), FromProgram (..), Op (..), decodeFromProgram, encodeProbeQuestion)
import Halogen.Remote.Session qualified as Session
import Protolude
import Test.Components
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe, shouldReturn, shouldSatisfy)
import Test.Session (clickOn, elementWithId, listenerOn)

-- | Every message the session sends, decoded, in order; and a gate the
-- sender waits at before each batch (not before an answer) while it is
-- closed.
data Wire = Wire
  { sent :: MVar [FromProgram]
  , gate :: MVar ()
  }

-- | A wire whose gate is open or closed to begin with.
newWire :: Bool -> IO (Wire, Text -> IO ())
newWire open = do
  sent <- newMVar []
  gate <- if open then newMVar () else newEmptyMVar
  let send text = for_ (decodeFromProgram text) $ \message -> do
        case message of
          ProgramBatch _ -> readMVar gate
          ProbeAnswer _ _ -> pass
        modifyMVar_ sent (pure . (<> [message]))
  pure (Wire {sent, gate}, send)

-- | Wait, up to five seconds, for the wire to have carried n messages, and
-- give them.
carried :: Wire -> Int -> IO [FromProgram]
carried wire n = tries (500 :: Int)
  where
    tries 0 = readMVar wire.sent >>= \s -> expectationFailure ("the session sent " <> show (length s) <> " messages, not " <> show n) $> []
    tries k =
      readMVar wire.sent >>= \s ->
        if length s >= n then pure s else threadDelay 10_000 >> tries (k - 1)

-- | That the wire carries nothing more than it has for a while.
stays :: Wire -> Int -> IO ()
stays wire n = do
  threadDelay 200_000
  (length <$> readMVar wire.sent) `shouldReturn` n

question :: Session.Session -> Int -> IO ()
question session = Session.receive session . encodeProbeQuestion

nothing, idleTree :: Activity
nothing = Activity {trees = 0, running = 0, waiting = 0}
idleTree = Activity {trees = 1, running = 0, waiting = 0}

data Wait = Wait

-- | A button whose handler waits in 'liftIO' until the test lets it go.
waiting :: forall m. (MonadIO m) => MVar () -> H.Component H.VoidF () Void m
waiting release =
  H.component
    (\() -> pure False)
    render
    [H.onAction (\Wait -> liftIO (readMVar release) >> put True)]
  where
    render :: Bool -> H.ComponentHTML Wait Empty m
    render done = HH.button [HP.id "wait", HE.onClick (const Wait)] [HH.text (if done then "done" else "waiting")]

spec :: Spec
spec = do
  describe "the probe, through a session" $ do
    it "answers before the mount that there is no tree, and sends nothing else" $ do
      (wire, send) <- newWire True
      session <- Session.newSession send
      Session.activity session `shouldReturn` nothing
      question session 7
      carried wire 1 `shouldReturn` [ProbeAnswer 7 nothing]
      stays wire 1
      Session.dispose session

    it "answers after the batches that send what was recorded when it asked" $ do
      (wire, send) <- newWire False
      session <- Session.newSession send
      _ <- Session.mount session counter ()
      -- The first batch is held at the gate: being sent, so running.
      Session.activity session >>= \a -> a.running `shouldBe` 1
      question session 1
      stays wire 0
      putMVar wire.gate ()
      carried wire 2 >>= \case
        [ProgramBatch opening, ProbeAnswer 1 a] -> do
          opening.sequence `shouldBe` 1
          a `shouldBe` idleTree
          -- A click with the gate closed again: its patch is held, and two
          -- questions asked meanwhile are answered after it, at once.
          _ <- takeMVar wire.gate
          button <- maybe (panic "no button") pure (elementWithId opening.ops "increment")
          listener <- maybe (panic "no listener") pure (listenerOn opening.ops button "click")
          Session.receive session (clickOn listener button)
          Session.activity session >>= \b -> b.running `shouldBe` 1
          question session 2
          question session 3
          stays wire 2
          putMVar wire.gate ()
          drop 2 <$> carried wire 5 >>= \case
            [ProgramBatch patch, ProbeAnswer 2 b, ProbeAnswer 3 c] -> do
              patch.sequence `shouldBe` 2
              [t | SetText _ t <- patch.ops] `shouldBe` ["1"]
              b `shouldBe` idleTree
              c `shouldBe` idleTree
            other -> expectationFailure ("not a batch then two answers: " <> show other)
        other -> expectationFailure ("not a batch then an answer: " <> show other)
      Session.activity session `shouldReturn` idleTree
      Session.dispose session

    it "says what is still unsent when it answers, and answers without waiting for a batch it was not behind" $ do
      (wire, send) <- newWire True
      session <- Session.newSession send
      _ <- Session.mount session counter ()
      [ProgramBatch opening] <- carried wire 1
      button <- maybe (panic "no button") pure (elementWithId opening.ops "increment")
      listener <- maybe (panic "no listener") pure (listenerOn opening.ops button "click")
      -- Close the gate: the click's batch is held while being sent. A question
      -- asked then waits for it; one asked after a second click, whose patch
      -- is recorded while the first is held, is behind that one too.
      _ <- takeMVar wire.gate
      Session.receive session (clickOn listener button)
      threadDelay 100_000
      question session 1
      Session.receive session (clickOn listener button)
      question session 2
      putMVar wire.gate ()
      messages <- carried wire 5
      case messages of
        [_, ProgramBatch b2, ProbeAnswer 1 a1, ProgramBatch b3, ProbeAnswer 2 a2] -> do
          [t | SetText _ t <- b2.ops] `shouldBe` ["1"]
          -- When the first question is answered, the second click's patch
          -- is recorded and not sent: running.
          a1.running `shouldBe` 1
          [t | SetText _ t <- b3.ops] `shouldBe` ["2"]
          a2 `shouldBe` idleTree
        other -> expectationFailure ("not the order of the contract: " <> show other)
      Session.dispose session

    it "answers every question, however they fall among the batches, after what was recorded before each" $ do
      (wire, send) <- newWire True
      session <- Session.newSession send
      _ <- Session.mount session counter ()
      [ProgramBatch opening] <- carried wire 1
      button <- maybe (panic "no button") pure (elementWithId opening.ops "increment")
      listener <- maybe (panic "no listener") pure (listenerOn opening.ops button "click")
      -- A click and a question at once, again and again, each question
      -- waited for: they arrive in every state of the sender (idle,
      -- deciding to send, taking a batch, sending one), and a question
      -- that waited for a batch nothing will send would never be answered.
      for_ [1 .. 50 :: Int] $ \i -> do
        Session.receive session (clickOn listener button)
        question session i
        messages <- carried wire (1 + 2 * i)
        -- Its answer is the last message, after the click's patch.
        let shown = lastMay [t | ProgramBatch b <- messages, SetText _ t <- b.ops]
        lastMay messages `shouldBe` Just (ProbeAnswer i idleTree)
        shown `shouldBe` Just (show i)
      Session.dispose session

    it "counts a fiber waiting in liftIO as waiting, and the tree as idle" $ do
      (wire, send) <- newWire True
      release <- newEmptyMVar
      session <- Session.newSession send
      _ <- Session.mount session (waiting release) ()
      [ProgramBatch opening] <- carried wire 1
      button <- maybe (panic "no button") pure (elementWithId opening.ops "wait")
      listener <- maybe (panic "no listener") pure (listenerOn opening.ops button "click")
      Session.receive session (clickOn listener button)
      question session 1
      carried wire 2 >>= (`shouldSatisfy` (== Just (ProbeAnswer 1 Activity {trees = 1, running = 0, waiting = 1})) . lastMay)
      putMVar release ()
      messages <- carried wire 3
      [t | ProgramBatch b <- messages, SetText _ t <- b.ops] `shouldBe` ["done"]
      question session 2
      carried wire 4 >>= (`shouldSatisfy` (== Just (ProbeAnswer 2 idleTree)) . lastMay)
      Session.dispose session

    it "counts no tree once disposed of, and answers nothing more" $ do
      (wire, send) <- newWire True
      session <- Session.newSession send
      _ <- Session.mount session counter ()
      _ <- carried wire 1
      Session.dispose session
      Session.activity session `shouldReturn` nothing
      question session 1
      stays wire 1

  describe "a session's identity" $ do
    it "is a number unique in the process, which equality, order and show go by" $ do
      alpha <- Session.newSession (const pass)
      beta <- Session.newSession (const pass)
      alpha `shouldBe` alpha
      alpha `shouldNotBe` beta
      Session.sessionId alpha `shouldNotBe` Session.sessionId beta
      compare alpha beta `shouldBe` compare (Session.sessionId alpha) (Session.sessionId beta)
      show alpha `shouldBe` (show (Session.sessionId alpha) :: Text)
      Session.dispose alpha
      Session.dispose beta
