-- | The JavaScript backend's stand-in for the quoted list: that backend's
-- compiler runs a quote's quoter only after linking the quoter's
-- dependencies, the compiler's own @ghc@ library among them, into
-- JavaScript, which takes gigabytes for every few quotes (see th's cabal
-- file), so haskell-halogen-th is not built there and the page says so. The example
-- itself is in @src/@.
module Example.Quotes (component) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Protolude
import UnliftIO (MonadUnliftIO)

component :: forall m. (MonadUnliftIO m) => H.Component H.VoidF () () m
component =
  H.component (const (pure ())) (const page) []
  where
    page :: H.ComponentHTML Void Empty m
    page =
      HH.p_
        [ HH.text "The hh and clay quoters run GHC's own parser at compile time, which the JavaScript backend's compiler cannot hold in memory; "
        , HH.text "this example runs on its WebAssembly build."
        ]
