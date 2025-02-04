{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Clay qualified as C
import DOM.HTML.Indexed qualified as I
import Data.Functor.Coyoneda
import Data.NT
import Data.Row
import Halogen as H
import Halogen.Component.Debounced
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Layout as L
import Halogen.HTML.Layout.BoxLayout
import Halogen.HTML.Layout.GridBagLayout
import Halogen.HTML.Properties as HP
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad
import Protolude hiding (log)
import UnliftIO (MonadUnliftIO)

#if defined(javascript_HOST_ARCH)
import Halogen.IO.Util as HA
import Halogen.VDom.Driver (runUI)
#endif

attachComponent :: IO (HalogenSocket Query Int IO)
logStr :: Text -> IO ()

#if defined(javascript_HOST_ARCH)
attachComponent =
  HA.awaitBody >>= runUI component ()
logStr = log
#else
attachComponent = panic "This module can only be run on JavaScript"
logStr = putStrLn
#endif

main :: IO ()
main = do
  HalogenSocket {messages} <- attachComponent

  void $ HS.subscribe messages $ \st ->
    logStr $ "State changed: " <> show st

{-
forever $ do
  threadDelay 5_000_000
  void $ query (IncrementQ ())
  threadDelay 5_000_000
  void $ query (DecrementQ ())
  -}

data Action = Increment Int | Decrement Int | Init

type Slots = ("debounced" .== H.Slot VoidF () ())

data Query a = IncrementQ a | DecrementQ a

component :: forall m. (MonadDOM m, MonadUnliftIO m) => H.Component Query () Int m
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, handleQuery, initialize = Just Init}
      }
  where
    initialState _ = pure 0

    render :: Int -> H.ComponentHTML Action Slots m
    render state =
      L.runLayoutM (defGridBagSettings {rows = 3, cols = 3}) $ L.do
        L.with (GridBagLayoutConstraints 1 1 2 2) $ L.runLayoutM Vertical $ L.do
          HH.button [HE.onClick $ const $ Decrement 1] [HH.text "-"]
          L.if_ (state > 5) $ HH.button [HE.onClick $ const $ Decrement 2] [HH.text "--"]
          HH.div_ [HH.text $ show state]
          HH.button [HE.onClick $ const $ Increment 1] [HH.text "+"]
          L.if_ (state > 5) $ HH.button [HE.onClick $ const $ Increment 2] [HH.text "++"]
          slot_ "debounced" () debComp ()
          HH.div_ [HH.text "Test sentinel element"]
          L.end

        L.with (GridBagLayoutConstraints 3 3 1 1) $
          HH.div [HP.style $ C.border (C.px 2) C.solid C.black] [HH.text "Banner!"]

        L.end

    handleQuery = \case
      IncrementQ cb -> do
        modify (+ 1)
        get >>= H.raise
        pure $ Just cb
      DecrementQ cb -> do
        modify (subtract 1)
        get >>= H.raise
        pure $ Just cb

    handleAction = \case
      Init ->
        lift $ log "Initialized"
      Increment n -> do
        modify (+ n)
        get >>= H.raise
      Decrement n -> do
        modify (subtract n)
        get >>= H.raise

---------------------------------------

newtype DebChanged = DebChanged Text

debComp :: (MonadDOM m, MonadUnliftIO m) => Component VoidF () () m
debComp = unsafeMkDebouncedComponent 0.5 $ ComponentSpec {initialState, render, eval}
  where
    initialState _ = pure ""

    render txt = runLayoutM Vertical $ L.do
      HH.div_ [HH.text "The text below is debounced"]
      HH.div_ [HH.text $ "Input content: " <> txt]
      HH.input
        [ HP.type_ I.InputText
        , HP.value txt
        , HP.style $ C.width C.auto
        , HE.onInputValueChange $ Just . DebChanged
        ]
      L.end

    eval = NT $ \case
      Initialize a -> pure a
      Finalize a -> pure a
      Receive _i a -> pure a
      Action (DebChanged str) a ->
        put str $> a
      Query (Coyoneda _req _fct) _f -> panic "Void2"
