{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Clay qualified as C
import DOM.HTML.Indexed qualified as I
import Data.Coerce
import Data.Foreign
import Data.Functor.Coyoneda
import Data.NT
import Data.Row
import Halogen as H
import Halogen.Component.Debounced
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription qualified as HS
import Protolude
import Web.Event.Event

#if defined(javascript_HOST_ARCH)
import Halogen.Aff.Util as HA
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.Driver (runUI)
#endif

attachComponent :: IO (HalogenSocket Query Int IO)
logStr :: Text -> IO ()

#if defined(javascript_HOST_ARCH)
attachComponent =
  HA.awaitBody >>= runUI component ()
logStr = DOM.log
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

data Action = Increment Int | Decrement Int

type Slots = ("debounced" .== H.Slot VoidF () ())

data Query a = IncrementQ a | DecrementQ a

component :: H.Component Query () Int IO
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, handleQuery}
      }
  where
    initialState _ = 0

    render :: Int -> H.ComponentHTML Action Slots IO
    render state =
      HH.div_ $
        [HH.button [HE.onClick $ const $ Decrement 1] [HH.text "-"]]
          <> [HH.button [HE.onClick $ const $ Decrement 2] [HH.text "--"] | state > 5]
          <> [ HH.div_ [HH.text $ show state]
             , HH.button [HE.onClick $ const $ Increment 1] [HH.text "+"]
             ]
          <> [HH.button [HE.onClick $ const $ Increment 2] [HH.text "++"] | state > 5]
          <> [HH.div_ [slot_ (Proxy @"debounced") () debComp ()]]

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
      Increment n -> do
        modify (+ n)
        get >>= H.raise
      Decrement n -> do
        modify (subtract n)
        get >>= H.raise

---------------------------------------

newtype DebChanged = DebChanged Text

debComp :: Component VoidF () () IO
debComp = unsafeMkDebouncedComponent 0.5 $ ComponentSpec {initialState, render, eval}
  where
    initialState _ = ""

    render txt =
      HH.div
        [HP.style $ C.display C.flex <> C.flexDirection C.column, HP.classes [ HH.ClassName "test1", HH.ClassName "test2"]]
        [ HH.div_ [HH.text "The text below is debounced"]
        , HH.div_ [HH.text $ "Input content: " <> txt]
        , HH.input
            [ HP.type_ I.InputText
            , HP.value txt
            , HE.onInput $ \ev ->
                DebChanged $ fromMaybe txt $ do
                  trg <- coerce <$> currentTarget ev
                  readProp "value" (Just . foreignToString) trg
            ]
        ]

    eval = NT $ \case
      Initialize a -> pure a
      Finalize a -> pure a
      Receive _i a -> pure a
      Action (DebChanged str) a ->
        put str $> a
      Query (Coyoneda _req _fct) _f -> panic "Void2"
