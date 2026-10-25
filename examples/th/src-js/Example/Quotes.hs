-- | The JavaScript backend's stand-in for the quoted list: ghc-lib-parser,
-- which parses the Haskell inside a quote, does not build there, so the
-- page says so. The example itself is in @src/@.
module Example.Quotes (component) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Protolude
import UnliftIO (MonadUnliftIO)

component :: forall m. (MonadUnliftIO m) => H.Component H.VoidF () () m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure ())
      , render = const page
      , eval = H.mkEval H.defaultEval
      }
  where
    page :: H.ComponentHTML Void Empty m
    page =
      HH.p_
        [ HH.text "The hh and clay quoters need ghc-lib-parser, which the JavaScript backend cannot build; "
        , HH.text "this example runs on its WebAssembly build."
        ]
