{-# LANGUAGE TemplateHaskell #-}

-- | Links clicked while the tree's loop is busy rendering.
--
-- A component's action runs on its tree's loop, one piece of work at a
-- time. A click that arrives while the loop is busy has its action queued,
-- and when nothing can finish that work inside the click's dispatch, the
-- action runs only after the browser has finished dispatching the click and
-- taken its default action. Whatever has to happen during the dispatch in
-- every case, such as keeping the browser from following a link, therefore
-- has to happen in the event's listener ('HE.preventingDefault',
-- 'HE.stoppingPropagation'), which "Halogen.Router"'s links do.
--
-- The test makes that case itself: pressing a button renders a child
-- component, whose 'initialState' runs during that render, on the loop and
-- outside any 'H.liftEffect', and dispatches a click to the link named. The
-- loop's own thread is then under the click's listener on the JavaScript
-- stack and cannot go on until the listener returns.
--
-- On WebAssembly a user's click can also find the loop busy with work that
-- is runnable but not running: the scheduler hands the page back after a
-- slice of time with a long render still unfinished, and runs the rest in a
-- later task. Nothing of the loop is under such a click, so its listener
-- lets that work finish, inside the dispatch, and then runs the action
-- there, where preventing the default from the action still counts. The
-- last button puts a click there deterministically: its action waits until
-- the page lets it go on, from a call the page makes in a task of its own,
-- and that call returns only once the long render (a few thousand rows of
-- filler) has begun and been preempted; in the same task the page then
-- clicks the link whose action prevents the default. (On the JavaScript
-- backend a listener cannot run other threads inside a dispatch, so there
-- the click's action is queued as before, and the test is skipped.)
--
-- What the browser decided once the dispatch was over is shown by the child,
-- or for the last button beside it: @followed@ (nothing prevented the
-- default), @prevented@, or @stopped@ (the click never got past the link).
-- The actions the clicks make are listed once they have run, with
-- @Filled@ for the long render.
module Example.Busy
  ( Route
  , Link (..)
  , linkClass
  , component
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (Callback, Safety (..), browserJS, freeCallback, jsValText, mkSyncCallback)
import Halogen.JSBits.Promise (Promise (..), await)
import Halogen.Router qualified as Router
import Protolude hiding (State)
import Web.Event.Event (EventType (..), preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

-- The click is dispatched by test/jsbits/busy.js, declared once for both
-- browser backends; natively the stub answers 0, which is never shown, since
-- the suite only reports a skip there.
$( browserJS
     ["test/jsbits/busy.js"]
     [ ("js_click_link", "halogen_busy_click_link", Unsafe, [t|Int -> IO Int|])
     , ("js_release_then_click", "halogen_busy_release_then_click", Unsafe, [t|Callback -> Int -> IO Promise|])
     ]
 )

-- | The routes of the router the component is given: a hash router's, so
-- that a link followed by the browser only changes the fragment.
type Route = Maybe Int

-- | The links, each handling its click another way.
data Link
  = -- | A "Halogen.Router" link to @Just 1@, followed with 'Router.follow'.
    RouterLink
  | -- | A click handler whose action prevents the default itself, in
    -- 'H.liftEffect'.
    ActionPrevents
  | -- | A click handler that prevents the default in the listener.
    ListenerPrevents
  | -- | The same, for a handler that makes no action of the click.
    ListenerIgnores
  | -- | A click handler that stops the click's propagation in the listener.
    ListenerStops
  deriving stock (Eq, Show, Enum, Bounded)

-- | The class of a link's element.
linkClass :: Link -> Text
linkClass l = "busy-link-" <> show (fromEnum l)

data State = State
  { router :: Router.Router Route
  , clicking :: Maybe Link
  , handled :: [Text]
  , filler :: Int
  -- ^ How many rows of filler are rendered: many, to make a render long.
  , preempted :: Text
  -- ^ What the browser decided about the click made while the loop was
  -- preempted, once it is known.
  }

data Action
  = ClickDuringRender Link
  | ClickWhilePreempted
  | Followed (Router.Follow Route)
  | Clicked Link MouseEvent

type Slots = ("clicker" .== H.Slot (Const Void) Void Int)

component :: forall q o m. (MonadIO m) => H.Component q (Router.Router Route) o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \router -> pure State {router, clicking = Nothing, handled = [], filler = 0, preempted = "pending"}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      HH.div_
        $ [ HH.ul_
              [ HH.li_
                  [ linkTo st l
                  , HH.button [HP.class_ (HH.ClassName ("busy-button-" <> show (fromEnum l))), HE.onClick (const (ClickDuringRender l))] [HH.text "click it during a render"]
                  ]
              | l <- [minBound .. maxBound]
              ]
          , HH.button [HP.class_ (HH.ClassName "busy-preempted"), HE.onClick (const ClickWhilePreempted)] [HH.text "click the action link while a long render is preempted"]
          , HH.p [HP.class_ (HH.ClassName "busy-preempted-outcome")] [HH.text st.preempted]
          , HH.p [HP.class_ (HH.ClassName "busy-handled")] [HH.text (T.unwords st.handled)]
          , HH.ul [HP.class_ (HH.ClassName "busy-filler")] [HH.li_ [HH.text (show i)] | i <- [1 .. st.filler]]
          ]
        <> [HH.slot_ "clicker" (fromEnum l) clicker (fromEnum l) | Just l <- [st.clicking]]

    linkTo :: State -> Link -> H.ComponentHTML Action Slots m
    linkTo st l =
      let cls = HP.class_ (HH.ClassName (linkClass l))
          fragment = HP.href ("#busy-" <> show (fromEnum l))
       in case l of
            RouterLink -> Router.link st.router Followed (Just 1) [cls] [HH.text "router"]
            ActionPrevents -> HH.a [cls, fragment, HE.onClick (Clicked l)] [HH.text "action"]
            ListenerPrevents -> HH.a [cls, fragment, HE.preventingDefault (HE.onClick (Clicked l))] [HH.text "listener"]
            ListenerIgnores -> HH.a [cls, fragment, HE.preventingDefault (HE.handler' (EventType "click") (const Nothing))] [HH.text "ignored"]
            ListenerStops -> HH.a [cls, fragment, HE.stoppingPropagation (HE.onClick (Clicked l))] [HH.text "stopped"]

    handleAction :: Action -> H.HalogenM State Action Slots o m ()
    handleAction = \case
      ClickDuringRender l -> modify $ \st -> st {clicking = Just l}
      Followed f -> do
        router <- gets (.router)
        Router.follow router f
        record RouterLink
      Clicked l ev -> do
        when (l == ActionPrevents) $ H.liftEffect (preventDefault (toEvent ev))
        record l
      ClickWhilePreempted -> do
        gate <- H.liftEffect (liftIO newEmptyMVar)
        started <- H.liftEffect (liftIO (newIORef False))
        -- The page's call that lets this action go on. A synchronous call
        -- returns with the threads it woke still runnable, so it waits
        -- until the long render below has begun: the worker woken here has
        -- then handed the rest of this action to a new runner of the loop,
        -- inside the call, and that runner has been preempted, its render
        -- unfinished, for this thread to see the flag. The click the page
        -- makes next, in the same task, finds the loop held that way.
        release <- H.liftEffect $ liftIO $ mkSyncCallback $ \_ -> do
          putMVar gate ()
          let untilStarted = readIORef started >>= \s -> unless s (yield >> untilStarted)
          untilStarted
        clicked <- H.liftEffect (liftIO (js_release_then_click release (fromEnum ActionPrevents)))
        liftIO (takeMVar gate)
        H.liftEffect (liftIO (writeIORef started True))
        modify $ \st -> st {filler = 3000}
        modify $ \st -> st {handled = st.handled <> ["Filled"]}
        outcome <- await clicked
        liftIO (freeCallback release)
        modify $ \st -> st {preempted = either (const "failed") jsValText outcome}

    record :: Link -> H.HalogenM State Action Slots o m ()
    record l = modify $ \st -> st {handled = st.handled <> [show l]}

-- | Clicks the link of the number it is given, as it is created.
clicker :: forall q o m. (MonadIO m) => H.Component q Int o m
clicker =
  H.mkComponent
    H.ComponentSpec
      { initialState = \n -> liftIO (js_click_link n)
      , render = \outcome ->
          HH.span [HP.class_ (HH.ClassName "busy-outcome")] [HH.text (fromMaybe "missing" (atMay ["followed", "prevented", "stopped"] outcome))]
      , eval = H.mkEval H.defaultEval
      }
