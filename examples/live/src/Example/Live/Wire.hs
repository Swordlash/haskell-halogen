-- | The board, shown in this page through the wire rather than by it: the
-- board component runs in 'RemoteDOM', its operations go to a client in an
-- element of this page through "Halogen.Remote.Loopback", the client's
-- events come back, and the messages both ways are shown beside it as they
-- cross. It is what the native server does, without the network.
module Example.Live.Wire
  ( page
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Data.Text qualified as Text
import Example.Live (Board, board)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Remote.Loopback (Loopback (..), Traffic (..))
import Halogen.Remote.Loopback qualified as Loopback
import Halogen.Subscription qualified as HS
import Protolude hiding (State)

data State = State
  { container :: H.RefLabel
  , running :: Maybe Loopback
  , seen :: [Text]
  }

data Action = Crossed Traffic

page :: forall q m. (MonadIO m, MonadUUID m) => Board -> H.Component q () Void m
page shared =
  H.component
    (\() -> (\container -> State {container, running = Nothing, seen = []}) <$> H.newRefLabel "wire")
    render
    [ H.onInitialize start
    , H.onFinalize (gets (.running) >>= traverse_ (liftIO . (.dispose)))
    , H.onAction $ \(Crossed message) -> modify (\s -> s {seen = lastN 40 (s.seen <> [line message])})
    ]
  where
    start = do
      label <- gets (.container)
      element <- H.getHTMLElementRef label
      for_ element $ \container -> do
        -- Subscribed to before mounting, so that the first render's
        -- message, sent as soon as the board is mounted, is seen too.
        HS.Subscribe {emitter, listener} <- liftIO HS.create
        void (H.subscribe (Crossed <$> emitter))
        loopback <- liftIO (Loopback.mountWatched (HS.notify listener) container (board shared) ())
        modify (\s -> s {running = Just loopback})

    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div
        [HP.class_ (H.ClassName "wire")]
        [ HH.section_ [HH.h2_ [HH.text "The page"], HH.div [HP.ref st.container, HP.class_ (H.ClassName "remote")] []]
        , HH.section_ [HH.h2_ [HH.text "The wire"], HH.pre [HP.class_ (H.ClassName "traffic")] [HH.text (Text.unlines st.seen)]]
        ]

    line = \case
      ToPage text -> "to the page:    " <> text
      ToProgram text -> "to the program: " <> text

    lastN n xs = drop (length xs - n) xs
