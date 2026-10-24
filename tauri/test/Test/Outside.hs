-- | What the package does outside Tauri, which is everywhere this suite
-- runs: natively, and under Node on the browser backends, where there is no
-- @window.__TAURI__@ either. And how a command's arguments are encoded,
-- which is the same everywhere. Tauri itself (or its double) is the browser
-- suite's (Browser.hs).
module Test.Outside (spec) where

import Data.Aeson (ToJSON (..), Value, camelTo2, defaultOptions, eitherDecodeStrict', fieldLabelModifier, genericToJSON, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Halogen.Subscription qualified as HS
import Halogen.Tauri
import Halogen.Tauri.Window qualified as Window
import Test.Hspec

-- | A command's arguments as a record, with the default options: Haskell's
-- camelCase fields are the keys Tauri expects of a Rust command's
-- snake_case parameters.
data SaveNote = SaveNote {noteId :: Int, title :: Text, tags :: [Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | The same, for a command declared with @rename_all = "snake_case"@.
newtype SnakeArgs = SnakeArgs SaveNote

instance ToJSON SnakeArgs where
  toJSON (SnakeArgs args) = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} args

-- | The JSON text read back, so that the test does not depend on the order
-- in which aeson writes an object's keys.
decoded :: Text -> Either String Value
decoded = eitherDecodeStrict' . encodeUtf8

spec :: Spec
spec = do
  describe "encodeArgs" $ do
    it "keys a record's fields by their camelCase names, as Tauri expects by default" $
      decoded (encodeArgs SaveNote {noteId = 7, title = "Zażółć", tags = ["a", "b"]})
        `shouldBe` Right (object ["noteId" .= (7 :: Int), "title" .= ("Zażółć" :: Text), "tags" .= (["a", "b"] :: [Text])])
    it "keys them in snake_case with the options for a snake_case command" $
      decoded (encodeArgs (SnakeArgs SaveNote {noteId = 7, title = "t", tags = []}))
        `shouldBe` Right (object ["note_id" .= (7 :: Int), "title" .= ("t" :: Text), "tags" .= ([] :: [Text])])
    it "gives an empty object for noArgs" $
      encodeArgs noArgs `shouldBe` "{}"

  describe "outside Tauri" $ do
    it "is not in Tauri" $
      isTauri `shouldReturn` False
    it "gives NotInTauri for a command, sending nothing" $
      (invoke "echo" SaveNote {noteId = 1, title = "", tags = []} :: IO (Either TauriError Value))
        `shouldReturn` Left NotInTauri
    it "gives NotInTauri for a command started and waited for later" $ do
      started <- start "echo" noArgs
      (result started :: IO (Either TauriError Value)) `shouldReturn` Left NotInTauri
      invoke_ "save" noArgs `shouldReturn` Left NotInTauri
    it "has an emitter that never fires, and an unsubscribe that does nothing" $ do
      heard <- newIORef (0 :: Int)
      subscription <- HS.subscribe (listen "greeting" :: HS.Emitter IO Text) (const (modifyIORef' heard (+ 1)))
      window <- HS.subscribe Window.resized (const (modifyIORef' heard (+ 1)))
      emit "greeting" ("hello" :: Text)
      emitTo "main" "greeting" ("hello" :: Text)
      HS.unsubscribe subscription
      HS.unsubscribe subscription
      HS.unsubscribe window
      readIORef heard `shouldReturn` 0
    it "has no window to destroy" $
      Window.destroy `shouldReturn` Left NotInTauri
