{-# LANGUAGE TemplateHaskell #-}

-- | A component that dispatches an event from a synchronous effect, to an
-- element of its own that cancels the event and stops it. A browser runs
-- an event's handlers while it dispatches it, so the handler has done both
-- by the time the dispatch returns: the effect sees the event cancelled,
-- and the element around never hears of it.
--
-- 'subscribed' does the same with listeners it subscribes to
-- ("Halogen.Query.Event") rather than renders: a handler heard through a
-- subscription is nested in the effect as an element's own is.
module Example.Dispatch (component, subscribed) where

import Control.Monad.UUID (MonadUUID)
import DOM.HTML.Indexed (InputType (..))
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, elementToEventTarget)
import Protolude hiding (State)
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (HTMLElement (..))
import Web.Event.Event (Event, EventType (..), preventDefault, stopPropagation)

-- The events are dispatched by test/jsbits/dispatch.js, declared once for
-- both browser backends; natively the stubs answer False, which is never
-- shown, since the suite only reports a skip there.
$( browserJS
     ["test/jsbits/dispatch.js"]
     [ ("js_poke", "halogen_dispatch_poke", Unsafe, [t|HTMLElement -> IO Bool|])
     , ("js_click_box", "halogen_dispatch_click_box", Unsafe, [t|HTMLElement -> IO Bool|])
     ]
 )

data State = State
  { target :: H.RefLabel
  -- ^ The element the event is dispatched to.
  , handled :: Bool
  -- ^ The inner element's handler ran.
  , outer :: Bool
  -- ^ The event reached the element around it.
  , trail :: [Text]
  -- ^ What happened, in order.
  , result :: Maybe (Bool, Bool)
  -- ^ Whether the dispatch said the event was cancelled, and whether the
  -- handler had run by then.
  }

data Action = Fire | Inner Event | Outer

component :: forall q i o m. (MonadIO m, MonadUUID m) => H.Component q i o m
component =
  H.component
    ( \_ -> do
        target <- H.newRefLabel "target"
        pure State {target, handled = False, outer = False, trail = [], result = Nothing}
    )
    render
    [H.onAction handleAction]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div
        [HE.handler (EventType "poke") (const Outer)]
        [ HH.div [HP.ref st.target, HE.handler (EventType "poke") Inner] []
        , HH.button [HP.class_ (HH.ClassName "fire"), HE.onClick (const Fire)] [HH.text "Fire"]
        , HH.p [HP.class_ (HH.ClassName "result")] [HH.text (describe st)]
        ]

    describe :: State -> Text
    describe st = case st.result of
      Nothing -> "not fired"
      Just (cancelled, handledFirst) ->
        T.intercalate
          ", "
          [ if cancelled then "cancelled" else "not cancelled"
          , if handledFirst then "handled during dispatch" else "handled after dispatch"
          , if st.outer then "propagated" else "stopped"
          ]
          <> ": "
          <> T.unwords st.trail

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Fire -> do
        modify $ \st -> st {handled = False, outer = False, result = Nothing}
        target <- gets (.target)
        H.getHTMLElementRef target >>= traverse_ (\el -> do
          modify $ \st -> st {trail = st.trail <> ["poke"]}
          cancelled <- H.liftEffect (liftIO (js_poke el))
          modify $ \st -> st {trail = st.trail <> ["poked"]}
          handledFirst <- gets (.handled)
          modify $ \st -> st {result = Just (cancelled, handledFirst)})
      Inner event -> do
        modify $ \st -> st {trail = st.trail <> ["inner"]}
        H.liftEffect (preventDefault event >> stopPropagation event)
        modify $ \st -> st {handled = True}
      Outer -> modify $ \st -> st {outer = True, trail = st.trail <> ["outer"]}

----------------------------------------------------------------------
-- The same through subscriptions

data Subscribed = Subscribed
  { element :: H.RefLabel
  -- ^ The element the event is dispatched to.
  , box :: H.RefLabel
  -- ^ A checkbox clicked from an effect, whose click the handler cancels.
  , heard :: Bool
  -- ^ The handler of the element's subscription ran.
  , boxHeard :: Bool
  -- ^ The handler of the checkbox's subscription ran.
  , steps :: [Text]
  -- ^ What happened, in order.
  , poked :: Maybe (Bool, Bool)
  -- ^ Whether the dispatch said the event was cancelled, and whether the
  -- handler had run by then.
  , clicked :: Maybe (Bool, Bool)
  -- ^ Whether the box was checked once the click returned, and whether the
  -- handler had run by then.
  }

data SubscribedAction = Listen | Poke | ClickBox | Heard Event | BoxHeard Event

-- | A component that hears its elements through subscriptions to their
-- listeners, made when it is initialized, and dispatches events to them from
-- synchronous effects: a custom event, which the handler cancels, and a
-- checkbox's click, whose default (checking the box) the handler prevents.
subscribed :: forall q i o m. (MonadBrowserDOM m, MonadUnliftIO m, MonadUUID m) => H.Component q i o m
subscribed =
  H.component
    ( \_ -> do
        element <- H.newRefLabel "subscribed-element"
        box <- H.newRefLabel "subscribed-box"
        pure Subscribed {element, box, heard = False, boxHeard = False, steps = [], poked = Nothing, clicked = Nothing}
    )
    render
    [H.onAction handleAction, H.onInitialize (handleAction Listen)]
  where
    render :: Subscribed -> H.ComponentHTML SubscribedAction Empty m
    render st =
      HH.div_
        [ HH.div [HP.ref st.element] []
        , HH.input [HP.ref st.box, HP.type_ InputCheckbox, HP.class_ (HH.ClassName "box")]
        , HH.button [HP.class_ (HH.ClassName "poke"), HE.onClick (const Poke)] [HH.text "Poke"]
        , HH.button [HP.class_ (HH.ClassName "click-box"), HE.onClick (const ClickBox)] [HH.text "Click the box"]
        , HH.p [HP.class_ (HH.ClassName "poked")] [HH.text (outcome "cancelled" "not cancelled" st.poked)]
        , HH.p [HP.class_ (HH.ClassName "clicked")] [HH.text (outcome "checked" "unchecked" st.clicked)]
        , HH.p [HP.class_ (HH.ClassName "steps")] [HH.text (T.unwords st.steps)]
        ]

    outcome :: Text -> Text -> Maybe (Bool, Bool) -> Text
    outcome yes no = \case
      Nothing -> "not yet"
      Just (said, heardFirst) ->
        (if said then yes else no) <> ", " <> (if heardFirst then "handled during dispatch" else "handled after dispatch")

    step :: Text -> H.HalogenM Subscribed SubscribedAction Empty o m ()
    step t = modify $ \st -> st {steps = st.steps <> [t]}

    handleAction :: SubscribedAction -> H.HalogenM Subscribed SubscribedAction Empty o m ()
    handleAction = \case
      -- Subscribed in 'H.liftEffect', so that the listeners are on the
      -- elements by the time the component counts as initialized.
      Listen -> do
        st <- get
        listen st.element (EventType "poke") Heard
        listen st.box (EventType "click") BoxHeard
      Poke -> do
        modify $ \st -> st {heard = False, poked = Nothing}
        gets (.element) >>= H.getHTMLElementRef >>= traverse_ (\el -> do
          step "poke"
          cancelled <- H.liftEffect (liftIO (js_poke el))
          step "poked"
          heardFirst <- gets (.heard)
          modify $ \st -> st {poked = Just (cancelled, heardFirst)})
      ClickBox -> do
        modify $ \st -> st {boxHeard = False, clicked = Nothing}
        gets (.box) >>= H.getHTMLElementRef >>= traverse_ (\el -> do
          step "click"
          checked <- H.liftEffect (liftIO (js_click_box el))
          step "clicked"
          heardFirst <- gets (.boxHeard)
          modify $ \st -> st {clicked = Just (checked, heardFirst)})
      Heard event -> do
        step "heard"
        H.liftEffect (preventDefault event)
        modify $ \st -> st {heard = True}
      BoxHeard event -> do
        step "box"
        H.liftEffect (preventDefault event)
        modify $ \st -> st {boxHeard = True}

    listen :: H.RefLabel -> EventType -> (Event -> SubscribedAction) -> H.HalogenM Subscribed SubscribedAction Empty o m ()
    listen label eventType action =
      H.getRef label >>= traverse_ (\el -> do
        emitter <- H.liftEffect $ do
          target <- elementToEventTarget el
          HS.lowerEmitter (HQE.eventListener eventType target Just)
        void (H.subscribe (action <$> emitter)))
