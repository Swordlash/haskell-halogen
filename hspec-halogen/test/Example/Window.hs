-- | A component that hears the page rather than its own elements, through
-- the emitters of "Halogen.Query.Event.Window": keys pressed anywhere, the
-- window resized, the tab hidden and shown, the network lost and found, the
-- screen's frames and a timer. It subscribes to all of them when it is
-- initialized and shows what each has said.
--
-- Every delivery from any of them is also counted in the counter the
-- component is given, by the emitter itself rather than by the component's
-- actions, so a test can see whether an emitter still delivers once its
-- subscription has ended: after 'Stop', or once the component is gone,
-- when the driver no longer runs its actions at all.
module Example.Window
  ( Query (..)
  , component
  )
where

import Data.IORef (IORef, atomicModifyIORef')
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Event.Window qualified as Window
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Protolude hiding (State)
import UnliftIO (MonadUnliftIO)
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState)
import Web.HTML.HTMLDocument.VisibilityState qualified as VisibilityState
import Web.UIEvent.KeyboardEvent qualified as KeyboardEvent

data Query a
  = -- | Unsubscribe from every emitter, as a component that no longer
    -- wants to hear the page would.
    Stop a
  | -- | The times of the last frames heard, oldest first.
    GetFrames ([Double] -> a)

data State = State
  { delivered :: IORef Int
  , keys :: [Text]
  , sizes :: [(Int, Int)]
  , visibilities :: [VisibilityState]
  , connections :: [Bool]
  , frames :: [Double]
  , ticks :: Int
  , subscriptions :: [H.SubscriptionId]
  }

data Action
  = Initialize
  | Key Text
  | Resized (Int, Int)
  | Seen VisibilityState
  | Connected Bool
  | Frame Double
  | Tick

component :: forall o m. (MonadBrowserDOM m, MonadUnliftIO m) => H.Component Query (IORef Int) o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \delivered ->
          pure State {delivered, keys = [], sizes = [], visibilities = [], connections = [], frames = [], ticks = 0, subscriptions = []}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.handleQuery = handleQuery, H.initialize = Just Initialize}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ line "keys" (T.unwords st.keys)
        , line "sizes" (T.unwords [show w <> "x" <> show h | (w, h) <- st.sizes])
        , line "visibility" (T.unwords (map VisibilityState.print st.visibilities))
        , line "online" (T.unwords (map (bool "offline" "online") st.connections))
        , line "ticks" (show st.ticks)
        ]

    line :: Text -> Text -> H.ComponentHTML Action Empty m
    line cls txt = HH.p [HP.class_ (HH.ClassName cls)] [HH.text txt]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> do
        counter <- gets (.delivered)
        -- Made at once, in 'H.liftEffect', so that the subscriptions are
        -- there once the component has been initialized, which is when
        -- 'mount' returns and a test starts acting on the page. After a
        -- 'lift' they would come later, and what the test does first
        -- could go unheard.
        keyDowns <- H.liftEffect Window.keyDown
        resizes <- H.liftEffect Window.resize
        visibility <- H.liftEffect Window.visibility
        online <- H.liftEffect Window.online
        subscriptions <-
          traverse
            (H.subscribe . counted counter)
            [ Key . KeyboardEvent.key <$> keyDowns
            , Resized <$> resizes
            , Seen <$> visibility
            , Connected <$> online
            , Frame <$> Window.animationFrame
            , Tick <$ Window.interval 20
            ]
        modify $ \st -> st {subscriptions}
      Key k -> modify $ \st -> st {keys = st.keys <> [k]}
      Resized size -> modify $ \st -> st {sizes = st.sizes <> [size]}
      Seen v -> modify $ \st -> st {visibilities = st.visibilities <> [v]}
      Connected c -> modify $ \st -> st {connections = st.connections <> [c]}
      -- Only the last few: there are sixty a second.
      Frame t -> modify $ \st -> st {frames = lastFew (st.frames <> [t])}
      Tick -> modify $ \st -> st {ticks = st.ticks + 1}

    handleQuery :: forall a. Query a -> H.HalogenM State Action Empty o m (Maybe a)
    handleQuery = \case
      Stop a -> do
        gets (.subscriptions) >>= traverse_ H.unsubscribe
        modify $ \st -> st {subscriptions = []}
        pure (Just a)
      GetFrames reply -> Just . reply <$> gets (.frames)

    lastFew xs = drop (length xs - 10) xs

-- | The emitter, counting what it delivers before the component hears it.
counted :: IORef Int -> HS.Emitter IO a -> HS.Emitter IO a
counted counter (HS.Emitter register) =
  HS.Emitter $ \k -> register $ \a -> atomicModifyIORef' counter (\n -> (n + 1, ())) >> k a
