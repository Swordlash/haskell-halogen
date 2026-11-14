{-# LANGUAGE TemplateHaskell #-}

-- | A component whose actions make the browser fire an event a moment
-- later, out of the program's hands, which it handles when it comes:
--
-- * a modal dialog, opened with @showModal()@ and closed by its button's
--   action with @close()@ (or by Escape): its @close@ event comes with the
--   page's next animation frame, and the component counts the closes it
--   hears;
--
-- * the history: an action pushes an entry with its depth in its state
--   (one more than the entry it was pushed from, 0 for an entry of
--   someone else's), and another goes back with @history.back()@ (or twice,
--   or forward, where there is nowhere to go); the @popstate@ comes once the
--   browser has gone through the history, and the component shows the depth
--   of the entry it arrived at.
--
-- The action that asks is over, and the program idle, before the event
-- comes; what a test reads right after the action shows what the late event
-- did only if the action waited for the event as well as for the program.
module Example.Late
  ( component
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, window, windowToEventTarget)
import Protolude hiding (State)
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (HTMLElement (..))
import Web.Event.Event (EventType (..))
import Web.HTML.History qualified as History
import Web.HTML.Window qualified as Window

-- An entry's number, kept in its state, and the dialog's methods: declared
-- once for both browser backends; natively the stubs do nothing, and the
-- suite only reports a skip there.
$( browserJS
     ["test/jsbits/late.js"]
     [ ("js_push", "halogen_late_push", Safe, [t|Int -> IO ()|])
     , ("js_entry", "halogen_late_entry", Safe, [t|IO Int|])
     , ("js_show_modal", "halogen_late_show_modal", Safe, [t|HTMLElement -> IO ()|])
     , ("js_close", "halogen_late_close", Safe, [t|HTMLElement -> IO ()|])
     ]
 )

data State = State
  { dialog :: H.RefLabel
  , closes :: Int
  , entry :: Int
  }

data Action
  = Initialize
  | Open
  | Dismiss
  | Closed
  | Push
  | Back
  | BackTwice
  | Forward
  | Arrived

component :: forall q i o m. (MonadBrowserDOM m, MonadUnliftIO m, MonadUUID m) => H.Component q i o m
component =
  H.component
    ( \_ -> do
        dialog <- H.newRefLabel "dialog"
        pure State {dialog, closes = 0, entry = 0}
    )
    render
    [H.onAction handleAction, H.onInitialize (handleAction Initialize)]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "closes")] [HH.text (show st.closes)]
        , HH.p [HP.class_ (HH.ClassName "entry")] [HH.text (show st.entry)]
        , button "open" Open
        , HH.dialog
            [HP.ref st.dialog, HE.handler (EventType "close") (const Closed)]
            [button "dismiss" Dismiss]
        , button "push" Push
        , button "back" Back
        , button "back-twice" BackTwice
        , button "forward" Forward
        ]

    button :: Text -> Action -> H.ComponentHTML Action Empty m
    button cls action = HH.button [HP.class_ (HH.ClassName cls), HE.onClick (const action)] [HH.text cls]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> do
        emitter <- H.liftEffect $ do
          target <- windowToEventTarget =<< window
          HS.lowerEmitter (HQE.eventListener (EventType "popstate") target (const (Just Arrived)))
        void (H.subscribe emitter)
        handleAction Arrived
      Open -> withDialog js_show_modal
      Dismiss -> withDialog js_close
      Closed -> modify $ \st -> st {closes = st.closes + 1}
      Push -> do
        n <- H.liftEffect (liftIO ((+ 1) <$> js_entry))
        H.liftEffect (liftIO (js_push n))
        modify $ \st -> st {entry = n}
      Back -> ours 1 (history History.back)
      BackTwice -> ours 2 (history (\h -> History.back h >> History.back h))
      Forward -> history History.forward
      Arrived -> do
        n <- H.liftEffect (liftIO js_entry)
        modify $ \st -> st {entry = n}

    withDialog :: (HTMLElement -> IO ()) -> H.HalogenM State Action Empty o m ()
    withDialog f = gets (.dialog) >>= H.getHTMLElementRef >>= traverse_ (H.liftEffect . liftIO . f)

    -- Only from an entry this many of ours deep: going back past them would
    -- take the page away from the suite, which a test that read too early,
    -- and so pushed again before its last Back had arrived, could ask for.
    ours :: Int -> H.HalogenM State Action Empty o m () -> H.HalogenM State Action Empty o m ()
    ours deep act = do
      n <- H.liftEffect (liftIO js_entry)
      when (n >= deep) act

    history :: (History.History -> IO ()) -> H.HalogenM State Action Empty o m ()
    history f = H.liftEffect (liftIO (Window.window >>= f . Window.history))
