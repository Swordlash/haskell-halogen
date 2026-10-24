-- | A socket a component opens: in its @Initialize@, inside an effect, with
-- its events subscribed to at once; sent on from a click; and closed in its
-- @Finalize@ when it goes away.
module Test.SocketComponent (spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (gets, modify)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Net.WebSocket qualified as WS
import Test.Hspec.Halogen
import Test.Sockets (accept, closedBy, sent, serverSends, socketListeners, sockets)

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "opens its socket as it starts, and renders what arrives, in order" $ runPage $ do
    ui <- mount m (chat :: H.Component H.VoidF Text Void m) "rendered"
    unsafeIOToPageM (sockets "rendered") `shouldReturn` 1
    unsafeIOToPageM $ do
      accept "rendered" ""
      serverSends "rendered" (WS.TextMessage "hello")
      serverSends "rendered" (WS.BinaryMessage "\0\1")
    log' <- find ui ".log"
    eventually (log' `shouldHaveText` "open;hello;2 bytes")

  it "sends from a click, inside its dispatch" $ runPage $ do
    ui <- mount m (chat :: H.Component H.VoidF Text Void m) "clicked"
    unsafeIOToPageM (accept "clicked" "")
    find ui ".log" >>= eventually . (`shouldHaveText` "open")
    find ui ".say" >>= click
    unsafeIOToPageM (sent "clicked") `shouldReturn` [WS.TextMessage "hi"]

  it "closes its socket in its Finalize when it goes away" $ runPage $ do
    ui <- mount m (chat :: H.Component H.VoidF Text Void m) "finalized"
    unsafeIOToPageM (accept "finalized" "")
    find ui ".log" >>= eventually . (`shouldHaveText` "open")
    unmount ui
    unsafeIOToPageM (closedBy "finalized") `shouldReturn` Just (Nothing, "")
    -- The close event comes afterwards, to nobody, and the socket's listeners
    -- go with it.
    eventually (unsafeIOToPageM (socketListeners "finalized") `shouldReturn` 0)

data State = State
  { name :: Text
  , socket :: Maybe WS.Socket
  , heard :: [Text]
  }

data Action = Initialize | Opened | Received WS.Message | Ended WS.Close | Say | Finalize

-- | A component that opens the double's socket under its name.
chat :: forall q o m. (MonadIO m) => H.Component q Text o m
chat =
  H.mkComponent
    H.ComponentSpec
      { initialState = \name -> pure State {name, socket = Nothing, heard = []}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              }
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "say"), HE.onClick (const Say)] [HH.text "say"]
        , HH.p [HP.class_ (HH.ClassName "log")] [HH.text (Text.intercalate ";" st.heard)]
        ]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> do
        name <- gets (.name)
        H.liftEffect (WS.open ("ws://double/" <> name) []) >>= \case
          Left refused -> heard refused.name
          Right socket -> do
            modify $ \st -> st {socket = Just socket}
            void $ H.subscribe (Opened <$ WS.opened socket)
            void $ H.subscribe (Received <$> WS.messages socket)
            void $ H.subscribe (Ended <$> WS.closed socket)
      Opened -> heard "open"
      Received (WS.TextMessage line) -> heard line
      Received (WS.BinaryMessage bytes) -> heard (Text.pack (show (BS.length bytes)) <> " bytes")
      Ended c -> heard ("closed " <> Text.pack (show c.code))
      Say -> gets (.socket) >>= traverse_ (\s -> void (H.liftEffect (WS.send s (WS.TextMessage "hi"))))
      Finalize -> gets (.socket) >>= traverse_ (H.liftEffect . WS.close')

    heard line = modify $ \st -> st {heard = st.heard <> [line]}
