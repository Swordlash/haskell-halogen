-- The store layer's program in the playground's test (../Playground.hs,
-- which says what it checks): a store, updated by a click and shown by a
-- connected component.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (put)
import Data.Row (Empty, type (.==))
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.IO.Util qualified as HA
import Halogen.Store qualified as Store
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

newtype Counter = Counter Int

data Bump = Bump

type App = Store.StoreT Bump Counter BrowserDOM

shown :: H.Component q (Store.Connected Int ()) o App
shown = H.component pure render [H.onReceive put]
  where
    render :: Store.Connected Int () -> H.ComponentHTML () Empty App
    render connected = HH.p_ [HH.text ("stored " <> T.pack (show connected.context))]

page :: H.Component q () o App
page = H.component (\_ -> pure ()) render [H.onAction (\Bump -> Store.updateStore Bump)]
  where
    render :: () -> H.ComponentHTML Bump ("shown" .== H.Slot H.VoidF Void ()) App
    render _ =
      HH.div_
        [ HH.button [HE.onClick (const Bump)] [HH.text "bump"]
        , HH.slot_ "shown" () (Store.connect (Store.selectEq (\(Counter n) -> n)) shown) ()
        ]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  root <- Store.runStoreT (Counter 0) (\(Counter n) Bump -> Counter (n + 1)) page
  socket <- runUI root () app
  HA.keepForNextRun socket.dispose
