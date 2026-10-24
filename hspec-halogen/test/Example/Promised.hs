{-# LANGUAGE TemplateHaskell #-}

-- | Work started in one action and awaited in another.
--
-- Pressing @start@ starts some JavaScript work in 'H.liftEffect', inside
-- the dispatch of the click, without waiting for it: the binding returns a
-- 'Promise' at once, which the component keeps in its state. Pressing
-- @collect@ later 'await's it, on a worker, while the rest of the tree goes
-- on: pressing @tick@ meanwhile renders its count at once. Once the work is
-- done the component shows what it resolved with, or what it was rejected
-- with, as a value rather than an exception.
module Example.Promised (component) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSText, Safety (..), browserJS, jsValText, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), await)
import Protolude hiding (State)

-- The work is test/jsbits/promised.js's. Natively the stubs return a
-- placeholder, which is never awaited, since the suite only reports a skip
-- there.
$( browserJS
     ["test/jsbits/promised.js"]
     [ ("js_later", "halogen_promised_later", Unsafe, [t|Int -> JSText -> IO Promise|])
     , ("js_failing", "halogen_promised_failing", Unsafe, [t|Int -> IO Promise|])
     ]
 )

data State = State
  { delayMs :: Int
  , pending :: Maybe Promise
  , result :: Text
  , ticks :: Int
  }

data Action = Start | StartFailing | Collect | Tick

-- | The component, given how long its work takes, in milliseconds.
component :: forall q o m. (MonadIO m) => H.Component q Int o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \delayMs -> pure State {delayMs, pending = Nothing, result = "idle", ticks = 0}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "start"), HE.onClick (const Start)] [HH.text "Start"]
        , HH.button [HP.class_ (HH.ClassName "start-failing"), HE.onClick (const StartFailing)] [HH.text "Start failing"]
        , HH.button [HP.class_ (HH.ClassName "collect"), HE.onClick (const Collect)] [HH.text "Collect"]
        , HH.button [HP.class_ (HH.ClassName "tick"), HE.onClick (const Tick)] [HH.text "Tick"]
        , HH.p [HP.class_ (HH.ClassName "result")] [HH.text st.result]
        , HH.p [HP.class_ (HH.ClassName "ticks")] [HH.text (show st.ticks)]
        ]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      -- At once, in the click's dispatch: the work starts, and nothing waits.
      Start -> do
        ms <- gets (.delayMs)
        started (H.liftEffect (liftIO (js_later ms (toJSText "ready"))))
      StartFailing -> do
        ms <- gets (.delayMs)
        started (H.liftEffect (liftIO (js_failing ms)))
      Collect ->
        gets (.pending) >>= \case
          Nothing -> pass
          Just promise -> do
            modify $ \st -> st {pending = Nothing, result = "waiting"}
            -- Waits on a worker ('await' is a 'liftIO'), while the tree
            -- goes on.
            outcome <- await promise
            modify $ \st -> st {result = either failed (("resolved: " <>) . jsValText) outcome}
      Tick -> modify $ \st -> st {ticks = st.ticks + 1}

    started start = do
      promise <- start
      modify $ \st -> st {pending = Just promise, result = "started"}

    failed :: JSError -> Text
    failed (JSError name message) = "rejected: " <> name <> ": " <> message
