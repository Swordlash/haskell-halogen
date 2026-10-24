-- | Commands, against the double's table of them (test/web/tauri-double.js).
module Test.Invoke (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeStrict', object, (.=))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Halogen.Tauri
import System.Timeout (timeout)
import Test.Double (outsideTauri, release)
import Test.Hspec

data Note = Note {noteId :: Int, title :: Text, tags :: [Text]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Held = Held {name :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

note :: Note
note = Note {noteId = 7, title = "Zażółć gęślą jaźń", tags = ["a", "b"]}

spec :: Spec
spec = do
  it "is in Tauri" $
    isTauri `shouldReturn` True

  describe "a command" $ do
    it "takes a record and gives one back" $
      invoke "echo" note `shouldReturn` Right note

    it "that returns nothing gives null, which invoke_ takes" $ do
      invoke_ "nothing" noArgs `shouldReturn` Right ()
      invoke "nothing" noArgs `shouldReturn` Right Null

    it "rejected with a string is Rejected, with the string as its message" $
      invoke "fail" (object ["message" .= ("disk full" :: Text)])
        `shouldReturn` (Left (Rejected JSError {name = "Error", message = "disk full"}) :: Either TauriError Value)

    it "rejected with an object is Rejected, with the object's JSON as its message" $ do
      outcome <- invoke "fail_object" (object ["path" .= ("/notes/7" :: Text)]) :: IO (Either TauriError Value)
      case outcome of
        Left (Rejected e) -> do
          e.name `shouldBe` "Error"
          eitherDecodeStrict' (encodeUtf8 e.message)
            `shouldBe` Right (object ["kind" .= ("NotFound" :: Text), "path" .= ("/notes/7" :: Text)])
        other -> expectationFailure ("expected a rejection, got " <> show other)

    it "that does not exist is Rejected as Tauri rejects it" $
      invoke "no_such_command" noArgs
        `shouldReturn` (Left (Rejected JSError {name = "Error", message = "Command no_such_command not found"}) :: Either TauriError Value)

    it "whose result does not decode is BadResult" $
      (invoke "number" noArgs :: IO (Either TauriError Note)) >>= \case
        Left (BadResult message) -> message `shouldSatisfy` ("Note" `Text.isInfixOf`)
        other -> expectationFailure ("expected BadResult, got " <> show other)

    it "with arguments that are no object is refused before it is sent" $
      (invoke "echo" [1 :: Int, 2] :: IO (Either TauriError Value)) >>= \case
        Left (Rejected e) -> e.name `shouldBe` "TypeError"
        other -> expectationFailure ("expected a TypeError, got " <> show other)

  describe "start and result" $ do
    it "send the command at once and wait for it later, as often as asked" $ do
      started <- start "held" Held {name = "later"}
      release "later"
      result started `shouldReturn` Right Held {name = "later"}
      result started `shouldReturn` Right (object ["name" .= ("later" :: Text)])

    it "keep the command going when a wait for it is killed" $ do
      started <- start "held" Held {name = "killed"}
      t <- forkIO (void (result started :: IO (Either TauriError Value)))
      threadDelay 20_000
      timeout 1_000_000 (killThread t) `shouldReturn` Just ()
      release "killed"
      result started `shouldReturn` Right Held {name = "killed"}

  describe "in a plain browser" $
    it "is not in Tauri, and a command gives NotInTauri" $
      outsideTauri $ do
        isTauri `shouldReturn` False
        (invoke "echo" note :: IO (Either TauriError Note)) `shouldReturn` Left NotInTauri
