-- The hooks layer's program in the playground's test (../Playground.hs,
-- which says what it checks): a counter written with haskell-halogen-hooks.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Hooks qualified as Hooks
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

counter :: H.Component q () o BrowserDOM
counter = Hooks.component @Empty $ \_ -> Hooks.do
  (n, nId) <- Hooks.useState (0 :: Int)
  Hooks.pure $
    HH.button [HE.onClick (const (Hooks.modify_ nId (+ 1)))] [HH.text ("Hooked " <> T.pack (show n) <> " times")]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI counter () app
  HA.keepForNextRun socket.dispose
