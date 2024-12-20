{-# LANGUAGE CPP #-}

module Main where

import Halogen as H
import Halogen.Aff.Util as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Protolude

main :: IO ()
#if defined(javascript_HOST_ARCH)
main = do
  body <- HA.awaitBody
  void $ runUI component () body
#else
main = panic "This module can only be run on JavaScript"
#endif

data Action = Increment Int | Decrement Int

component :: H.Component query () output IO
component =
  H.Component $
    H.ComponentSpec
      { initialState
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction}
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

    handleAction = \case
      Increment n -> modify (+ n)
      Decrement n -> modify (subtract n)
