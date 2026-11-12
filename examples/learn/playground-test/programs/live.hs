-- The live layer's program in the playground's test (../Playground.hs, which
-- says what it checks): a component sent over the live layer's wire to a
-- client in the page.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.IO.Util qualified as HA
import Halogen.Remote.Loopback qualified as Loopback
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

counter :: (Monad m) => H.Component q () o m
counter = H.component (\_ -> pure (0 :: Int)) (\n -> HH.button [HE.onClick (const ())] [HH.text ("remote clicks: " <> T.pack (show n))]) [H.onAction (\() -> modify (+ 1))]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  loopback <- liftIO (Loopback.mountIn app counter ())
  HA.keepForNextRun (liftIO loopback.dispose)
