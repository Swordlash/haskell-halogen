{-# LANGUAGE CPP #-}

-- | A component that dispatches an event from a synchronous effect, to an
-- element of its own that cancels the event and stops it. A browser runs
-- an event's handlers while it dispatches it, so the handler has done both
-- by the time the dispatch returns: the effect sees the event cancelled,
-- and the element around never hears of it.
module Example.Dispatch (component) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Web.DOM.Internal.Types (HTMLElement (..))
import Web.Event.Event (Event, EventType (..), preventDefault, stopPropagation)

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((el) => !el.dispatchEvent(new Event('poke', {bubbles: true, cancelable: true})))"
  js_poke :: HTMLElement -> IO Bool
#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "!$1.dispatchEvent(new Event('poke', {bubbles: true, cancelable: true}))"
  js_poke :: HTMLElement -> IO Bool
#else
js_poke :: HTMLElement -> IO Bool
js_poke _ = pure False
#endif

data State = State
  { handled :: Bool
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

target :: H.RefLabel
target = H.RefLabel "target"

component :: forall q i o m. (MonadIO m) => H.Component q i o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure State {handled = False, outer = False, trail = [], result = Nothing})
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div
        [HE.handler (EventType "poke") (const Outer)]
        [ HH.div [HP.ref target, HE.handler (EventType "poke") Inner] []
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
