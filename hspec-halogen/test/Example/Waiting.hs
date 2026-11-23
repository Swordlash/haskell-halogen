{-# LANGUAGE TemplateHaskell #-}

-- | Components that keep the program busy in the ways a test waits for, or
-- must not wait for.
--
-- 'component' loads in its actions: one waits in an @H.async@ for a
-- 'threadDelay', one for a promise of the page's (a request, say), one for
-- two seconds. Beside them a ticker pauses with 'H.delay' for ever, which a
-- test waiting for the program to be quiet must not wait for.
--
-- 'spinner' never stops working once started: each action starts the next
-- on its tree's loop, so the tree is running from then on, until the test
-- tells it to stop.
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
  | LoadForever
  | LoadLater
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
        , button "load-forever" LoadForever
        , button "load-later" LoadLater
        ]

    button :: Text -> Action -> H.ComponentHTML Action Empty m
    button cls action = HH.button [HP.class_ (HH.ClassName cls), HE.onClick (const action)] [HH.text cls]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Load -> loading "loaded" (threadDelay 300000)
      LoadPromise -> loading "loaded from a promise" (void (wait =<< js_after 300))
      LoadSlowly -> loading "loaded slowly" (threadDelay 2000000)
      -- A request that never comes back: a test of a deadline needs one,
      -- which no machine is slow enough to see answered. Going away ends it.
      LoadForever -> loading "never loaded" (forever (threadDelay 1000000))
      -- A counted pause, which a test's clock holds until it is advanced.
      LoadLater -> do
        modify $ \st -> st {status = "loading"}
        H.delayCounted 2
        modify $ \st -> st {status = "loaded later"}
      Tick -> modify $ \st -> st {ticks = st.ticks + 1}

    loading :: Text -> IO () -> H.HalogenM State Action Empty o m ()
    loading done work = do
      modify $ \st -> st {status = "loading"}
      H.async (liftIO work)
      modify $ \st -> st {status = done}

data Spinning = Spinning
  { going :: IO Bool
  , stopAt :: Double
  , spins :: Int
  , again :: Maybe (HS.Listener IO SpinAction)
  }

data SpinAction = Start | Spin

-- | Spins for as long as the action it is given says, from a moment after
-- it is mounted: each spin renders, then notifies an emitter of its own for
-- the next, which the thread running the loop runs there and then, inside
-- the one before. The loop is busy from then on, and nothing else gets on
-- it meanwhile: unmounting the spinner waits for it to stop. It starts from
-- a timer of its own, so that the thread running its loop is the timer's
-- and not the test's, which would otherwise spin in
-- 'Test.Hspec.Halogen.mount' until it stopped.
--
-- A test of a deadline gives it an action that says to go on until the
-- deadline has fired, so that the program is busy for all of the deadline
-- whatever the machine's speed. It stops after a minute in any case, so
-- that a test that fails before it can tell the spinner to stop does not
-- hang the suite.
spinner :: forall q o m. (MonadIO m) => H.Component q (IO Bool) o m
spinner =
  H.component
    (\going -> pure Spinning {going, stopAt = 0, spins = 0, again = Nothing})
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
        HS.Subscribe {emitter, listener} <- H.sync (liftIO HS.create)
        void (H.subscribe emitter)
        now <- H.sync (liftIO getMonotonicTime)
        modify $ \st -> st {stopAt = now + 60, again = Just listener}
        handleAction Spin
      Spin -> do
        modify $ \st -> st {spins = st.spins + 1}
        st <- get
        now <- H.sync (liftIO getMonotonicTime)
        more <- H.sync (liftIO st.going)
        when (more && now < st.stopAt) $ for_ st.again $ \listener -> H.sync (liftIO (HS.notify listener Spin))
