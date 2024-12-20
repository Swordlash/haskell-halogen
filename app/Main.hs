{-# LANGUAGE CPP #-}
module Main where

import Protolude

import Halogen as H
import Halogen.Aff.Util as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: IO ()
#if defined(javascript_HOST_ARCH)
main = do
  body <- HA.awaitBody
  void $ runUI component () body
#else
main = panic "This module can only be run on JavaScript"
#endif

data Action = Increment | Decrement

component :: H.Component query () output IO
component =
  H.Component $
    H.ComponentSpec
      { initialState
      , render
      , eval = H.mkEval $ H.defaultEval { handleAction }
      }
    where
      initialState :: () -> Int
      initialState _ = 0

      render state =
        HH.div_
          [ HH.button [ HE.onClick $ const Decrement ] [ HH.text "-" ]
          , HH.div_ [ HH.text $ show state ]
          , HH.button [ HE.onClick $ const Increment ] [ HH.text "+" ]
          ]

      handleAction = \case
        Increment -> modify (+1)
        Decrement -> modify (subtract 1)