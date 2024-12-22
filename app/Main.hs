{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription qualified as HS
import Protolude

#if defined(javascript_HOST_ARCH)
import Halogen.Aff.Util as HA
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.Driver (runUI)
#endif

attachComponent :: IO (HalogenSocket Query Int IO)
logStr :: Text -> IO ()

#if defined(javascript_HOST_ARCH)
attachComponent = HA.awaitBody >>= runUI component ()
logStr = DOM.log
#else
attachComponent = panic "This module can only be run on JavaScript"
logStr = putStrLn
#endif

main :: IO ()
main = do
  HalogenSocket {query, messages} <- attachComponent

  void $ HS.subscribe messages $ \st ->
    logStr $ "State changed: " <> show st

  forever $ do
    threadDelay 5_000_000
    void $ query (IncrementQ ())
    threadDelay 5_000_000
    void $ query (DecrementQ ())

data Action = Increment Int | Decrement Int

data Query a = IncrementQ a | DecrementQ a

component :: H.Component Query () Int IO
component =
  H.Component $
    H.ComponentSpec
      { initialState
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, handleQuery}
      }
  where
    initialState _ = 0

    render state =
      HH.div_ $
        [HH.button [HE.onClick $ const $ Decrement 1] [HH.text "-"]]
          <> [HH.button [HE.onClick $ const $ Decrement 2] [HH.text "--"] | state > 5]
          <> [ HH.div_ [HH.text $ show state]
             , HH.button [HE.onClick $ const $ Increment 1] [HH.text "+"]
             ]
          <> [HH.button [HE.onClick $ const $ Increment 2] [HH.text "++"] | state > 5]

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
