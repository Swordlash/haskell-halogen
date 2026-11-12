-- The th layer's program in the playground's test (../Playground.hs, which
-- says what it checks): an hh quote from haskell-halogen-th, with Haskell
-- inside.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text qualified as T
import Halogen qualified as H
import Halogen.IO.Util qualified as HA
import Halogen.TH (hh)
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

quoted :: H.Component q () o BrowserDOM
quoted =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (6 :: Int)
      , -- Whitespace on the quote's one line would be a text beside the
        -- element (JSX's rule), and hh takes one element: hence the lines.
        H.render = \n ->
          [hh|
            <p class="quoted">hello from hh, {{ T.pack (show (n * 7)) }}</p>
          |]
      , H.eval = H.mkEval H.defaultEval
      }

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI quoted () app
  HA.keepForNextRun socket.dispose
