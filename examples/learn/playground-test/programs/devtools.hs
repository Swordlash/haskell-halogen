-- The devtools layer's program in the playground's test (../Playground.hs,
-- which says what it checks): Halogen.DevTools' panel over the page,
-- counting a counter's click.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (modify)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.DevTools qualified as DevTools
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

counter :: H.Component q () o BrowserDOM
counter =
  H.named "Counter" $
    H.component (\_ -> pure (0 :: Int)) (\n -> HH.button [HE.onClick (const ())] [HH.text ("Counted " <> T.pack (show n))]) [H.onAction (\() -> modify (+ 1))]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  tools <- DevTools.open
  socket <- runUI counter () app
  HA.keepForNextRun (socket.dispose >> tools.dispose)
