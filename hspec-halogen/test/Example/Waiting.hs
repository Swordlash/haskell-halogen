{-# LANGUAGE TemplateHaskell #-}

-- | Components that keep the program busy in the ways a test waits for, or
-- must not wait for.
--
-- 'component' loads in its actions: one waits in a @liftIO@ for a
-- 'threadDelay', one for a promise of the page's (a request, say), one for
-- two seconds. Beside them a ticker pauses with 'H.delay' for ever, which a
-- test waiting for the program to be quiet must not wait for.
--
-- 'spinner' never stops working once started: each action starts the next
-- on its tree's loop, so the tree is running from then on, until the
-- spinner's own deadline.
module Example.Waiting
  ( component
  , spinner
  )
where

import Data.Row (Empty)
import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSVal, Safety (..), browserJS)
import Halogen.JSBits.Promise (Promise, wait)
import Halogen.Subscription qualified as HS
import Protolude hiding (State, wait)

$( browserJS
     ["test/jsbits/waiting.js"]
     [("js_after", "halogen_waiting_after", Safe, [t|Int -> IO (Promise JSVal)|])]
 )

data State = State
  { status :: Text
  , ticks :: Int
  }

data Action
  = Load
  | LoadPromise
  | LoadSlowly
  | Tick

-- | The ticker's pause.
tickEvery :: NominalDiffTime
tickEvery = 0.02

component :: forall q i o m. (MonadIO m) => H.Component q i o m
component =
  H.component
    (\_ -> pure State {status = "idle", ticks = 0})
    render
    [H.onAction handleAction, H.onInitialize (void (H.fork (forever (H.delay tickEvery >> handleAction Tick))))]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "status")] [HH.text st.status]
        , HH.p [HP.class_ (HH.ClassName "ticks")] [HH.text (show st.ticks)]
        , button "load" Load
        , button "load-promise" LoadPromise
        , button "load-slowly" LoadSlowly
        ]

    button :: Text -> Action -> H.ComponentHTML Action Empty m
    button cls action = HH.button [HP.class_ (HH.ClassName cls), HE.onClick (const action)] [HH.text cls]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Load -> loading "loaded" (threadDelay 300000)
      LoadPromise -> loading "loaded from a promise" (void (wait =<< js_after 300))
      LoadSlowly -> loading "loaded slowly" (threadDelay 2000000)
      Tick -> modify $ \st -> st {ticks = st.ticks + 1}

    loading :: Text -> IO () -> H.HalogenM State Action Empty o m ()
    loading done work = do
      modify $ \st -> st {status = "loading"}
      liftIO work
      modify $ \st -> st {status = done}

data Spinning = Spinning
  { seconds :: Double
  , deadline :: Double
  , spins :: Int
  , again :: Maybe (HS.Listener IO SpinAction)
  }

data SpinAction = Start | Spin

-- | Spins for this many seconds, from a moment after it is mounted: each
-- spin renders, then notifies an emitter of its own for the next, which the
-- thread running the loop runs there and then, inside the one before. The
-- loop is busy from then on, until the time is up, and nothing else gets on
-- it meanwhile: unmounting the spinner waits for the end. It starts from a
-- timer of its own, so that the thread running its loop is the timer's and
-- not the test's, which would otherwise spin in
-- 'Test.Hspec.Halogen.mount' until the time was up.
spinner :: forall q o m. (MonadIO m) => H.Component q Double o m
spinner =
  H.component
    (\seconds -> pure Spinning {seconds, deadline = 0, spins = 0, again = Nothing})
    (\st -> HH.p [HP.class_ (HH.ClassName "spins")] [HH.text (show st.spins)])
    [H.onAction handleAction, H.onInitialize (void (H.subscribe soon))]
  where
    -- The timer's thread ends once it has emitted, and is not killed when
    -- the component goes: it runs the loop (and the spinning) from then on,
    -- and so the finalizer that would kill it.
    soon = HS.makeEmitter $ \emit -> do
      void (forkIO (threadDelay 10000 >> emit Start))
      pure pass

    handleAction :: SpinAction -> H.HalogenM Spinning SpinAction Empty o m ()
    handleAction = \case
      Start -> do
        HS.Subscribe {emitter, listener} <- H.liftEffect (liftIO HS.create)
        void (H.subscribe emitter)
        now <- H.liftEffect (liftIO getMonotonicTime)
        modify $ \st -> st {deadline = now + st.seconds, again = Just listener}
        handleAction Spin
      Spin -> do
        modify $ \st -> st {spins = st.spins + 1}
        st <- get
        now <- H.liftEffect (liftIO getMonotonicTime)
        when (now < st.deadline) $ for_ st.again $ \listener -> H.liftEffect (liftIO (HS.notify listener Spin))
