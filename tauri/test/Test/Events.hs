-- | Events, on the double's bus (test/web/tauri-double.js): the Rust side's,
-- the page's own, and the current window's.
module Test.Events (spec) where

import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, toJSON, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Halogen.Subscription qualified as HS
import Halogen.Tauri
import Halogen.Tauri.Window qualified as Window
import System.Timeout (timeout)
import Test.Double
import Test.Hspec

data Progress = Progress {file :: Text, percent :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

spec :: Spec
spec = do
  describe "listen" $ do
    it "delivers the Rust side's event, decoded" $ do
      (chan, _) <- subscribed (listen "progress")
      fire "progress" Progress {file = "a.txt", percent = 40}
      heard chan `shouldReturn` Progress {file = "a.txt", percent = 40}

    it "drops a payload that does not decode, and delivers the next" $ do
      (chan, _) <- subscribed (listen "progress-dropping")
      fire "progress-dropping" ("not a record" :: Text)
      fire "progress-dropping" Progress {file = "b.txt", percent = 100}
      heard chan `shouldReturn` Progress {file = "b.txt", percent = 100}

    it "hears an event emitted to a window, as listening anywhere does" $ do
      (chan, _) <- subscribed (listen "progress-to")
      fireTo "other" "progress-to" Progress {file = "c.txt", percent = 1}
      heard chan `shouldReturn` Progress {file = "c.txt", percent = 1}

  describe "listenEither" $
    it "delivers aeson's message for a payload that does not decode" $ do
      (chan, _) <- subscribed (listenEither "progress-either" :: HS.Emitter IO (Either Text Progress))
      fire "progress-either" (42 :: Int)
      heard chan >>= (`shouldSatisfy` either (const True) (const False))

  describe "a subscription's unsubscribe" $ do
    it "unlistens, and nothing is delivered after it" $ do
      earlier <- unlistens
      (chan, subscription) <- subscribed (listen "progress-stopped" :: HS.Emitter IO Progress)
      HS.unsubscribe subscription
      listening "progress-stopped" `shouldReturn` 0
      unlistens `shouldReturn` earlier + 1
      fire "progress-stopped" Progress {file = "d.txt", percent = 2}
      nothingHeard chan

    it "made before Tauri has given the unlisten function unlistens once it has" $ do
      earlier <- unlistens
      subscription <- HS.subscribe (listen "progress-early" :: HS.Emitter IO Progress) (const (pure ()))
      HS.unsubscribe subscription
      HS.unsubscribe subscription
      waitUntil "the listener is taken off" ((== earlier + 1) <$> unlistens)
      listening "progress-early" `shouldReturn` 0

  describe "emit" $ do
    it "reaches the page's own listeners, as Tauri sends it back" $ do
      (chan, _) <- subscribed (listen "saved")
      emit "saved" Progress {file = "e.txt", percent = 3}
      heard chan `shouldReturn` Progress {file = "e.txt", percent = 3}
      emitted >>= (`shouldContain` [Emitted {target = Nothing, event = "saved", payload = toJSON Progress {file = "e.txt", percent = 3}}])

    it "emitTo sends to the target's label" $ do
      emitTo "settings" "theme" ("dark" :: Text)
      emitted >>= (`shouldContain` [Emitted {target = Just "settings", event = "theme", payload = String "dark"}])

  describe "the current window's" $ do
    it "resize is the window's own, and not another window's" $ do
      (chan, _) <- subscribed Window.resized
      fireTo "other" "tauri://resize" (object ["width" .= (1 :: Int), "height" .= (1 :: Int)])
      fireTo "main" "tauri://resize" (object ["width" .= (800 :: Int), "height" .= (600 :: Int)])
      heard chan `shouldReturn` Window.PhysicalSize {width = 800, height = 600}

    it "move, focus, blur and theme are decoded" $ do
      (moves, _) <- subscribed Window.moved
      (focuses, _) <- subscribed Window.focused
      (blurs, _) <- subscribed Window.blurred
      (themes, _) <- subscribed Window.themeChanged
      fireTo "main" "tauri://move" (object ["x" .= (-10 :: Int), "y" .= (20 :: Int)])
      fireTo "main" "tauri://focus" Null
      fireTo "main" "tauri://blur" Null
      fireTo "main" "tauri://theme-changed" ("dark" :: Text)
      heard moves `shouldReturn` Window.PhysicalPosition {x = -10, y = 20}
      heard focuses `shouldReturn` ()
      heard blurs `shouldReturn` ()
      heard themes `shouldReturn` Window.Dark

    it "close request is heard, and destroy destroys the window" $ do
      (chan, _) <- subscribed Window.closeRequested
      fireTo "main" "tauri://close-requested" Null
      heard chan `shouldReturn` ()
      earlier <- destroyed
      Window.destroy `shouldReturn` Right ()
      destroyed `shouldReturn` earlier + 1

  describe "in a plain browser" $
    it "listen never listens, and emit sends nothing" $ do
      earlier <- emitted
      outsideTauri $ do
        _ <- HS.subscribe (listen "outside" :: HS.Emitter IO Value) (const (pure ()))
        emit "outside" Null
      threadDelay 50_000
      listening "outside" `shouldReturn` 0
      emitted `shouldReturn` earlier

-- | Subscribe, and return once the double has the listener on its bus, so
-- that what the test fires next reaches it.
subscribed :: HS.Emitter IO a -> IO (Chan a, HS.Subscription IO)
subscribed emitter = do
  chan <- newChan
  earlier <- totalListening
  subscription <- HS.subscribe emitter (writeChan chan)
  waitUntil "the listener is on the bus" ((> earlier) <$> totalListening)
  pure (chan, subscription)
  where
    totalListening = listening ""

-- | The next value delivered, within two seconds.
heard :: Chan a -> IO a
heard chan = timeout 2_000_000 (readChan chan) >>= maybe (fail "nothing was delivered within two seconds") pure

-- | Nothing is delivered within a tenth of a second.
nothingHeard :: Chan a -> Expectation
nothingHeard chan = timeout 100_000 (readChan chan) >>= maybe (pure ()) (const (expectationFailure "something was delivered"))

waitUntil :: String -> IO Bool -> Expectation
waitUntil what check = go (200 :: Int)
  where
    go 0 = expectationFailure ("timed out waiting until " <> what)
    go n = check >>= \ok -> unless ok (threadDelay 10_000 >> go (n - 1))
