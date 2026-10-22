{-# LANGUAGE TemplateHaskell #-}

-- | Links clicked while the tree's loop is busy rendering.
--
-- A component's action runs on its tree's loop, one piece of work at a
-- time: a click that arrives while the loop is busy (a render under way) has
-- its action queued, and runs only after the browser has finished
-- dispatching the click and taken its default action. Whatever has to happen
-- during the dispatch, such as keeping the browser from following a link,
-- therefore has to happen in the event's listener ('HE.preventingDefault',
-- 'HE.stoppingPropagation'), which "Halogen.Router"'s links do.
--
-- A user's click finds the loop busy when Haskell work is left to run as it
-- arrives (the rest of what an earlier click started, a render among it):
-- on WebAssembly the click's listener runs that work too, before and
-- between its own steps, and when the work takes the loop first the
-- listener finds it taken. That depends on the scheduler's timing, so the
-- test puts the click there itself instead: pressing a button renders a
-- child component, whose 'initialState' runs during that render, on the
-- loop and outside any 'H.liftEffect', and dispatches a click to the link
-- named, which finds the loop as busy as a user's click can.
-- What the browser decided once the dispatch was over is shown by the child:
-- @followed@ (nothing prevented the default), @prevented@, or @stopped@ (the
-- click never got past the link). The actions the clicks make are listed
-- once they have run.
module Example.Busy
  ( Route
  , Link (..)
  , linkClass
  , component
  )
where

import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.Router qualified as Router
import Protolude hiding (State)
import Web.Event.Event (EventType (..), preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

-- The click is dispatched by test/jsbits/busy.js, declared once for both
-- browser backends; natively the stub answers 0, which is never shown, since
-- the suite only reports a skip there.
$( browserJS
     ["test/jsbits/busy.js"]
     [("js_click_link", "halogen_busy_click_link", Unsafe, [t|Int -> IO Int|])]
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
  }

data Action
  = ClickDuringRender Link
  | Followed (Router.Follow Route)
  | Clicked Link MouseEvent

type Slots = ("clicker" .== H.Slot (Const Void) Void Int)

component :: forall q o m. (MonadIO m) => H.Component q (Router.Router Route) o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \router -> pure State {router, clicking = Nothing, handled = []}
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
          , HH.p [HP.class_ (HH.ClassName "busy-handled")] [HH.text (T.unwords st.handled)]
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
