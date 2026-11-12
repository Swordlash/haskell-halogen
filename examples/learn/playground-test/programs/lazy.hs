-- The lazy layer's program in the playground's test (../Playground.hs, which
-- says what it checks): a component from the lazy layer's demo library,
-- linked while the program runs.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (put)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.IO.Util qualified as HA
import Halogen.Lazy qualified as Lazy
import Halogen.Lazy.Demo qualified as Demo
import Halogen.Lazy.Dyld qualified as Dyld
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

data Action = Heard Demo.Output | Tell

page :: H.Component q () o BrowserDOM
page = H.component (\_ -> pure "nothing heard") render [H.onAction handle]
  where
    render :: T.Text -> H.ComponentHTML Action ("feature" .== H.Slot Demo.Query Demo.Output ()) BrowserDOM
    render heard =
      HH.div_
        [ HH.slot "feature" () (Lazy.lazy loading feature) "playground" Heard
        , HH.button [HE.onClick (const Tell)] [HH.text "tell it"]
        , HH.p_ [HH.text heard]
        ]
    loading = Lazy.defaultLoading {Lazy.placeholder = HH.p_ [HH.text "linking"]}
    feature :: BrowserDOM (Demo.Feature BrowserDOM)
    feature = liftIO (Dyld.load (Dyld.library "layers/lazy/liblazy-demo.so" Demo.entry))
    handle action = case action of
      Heard (Demo.Waved n) -> put ("heard " <> T.pack (show n))
      Tell -> H.tell "feature" () Demo.Wave

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI page () app
  HA.keepForNextRun socket.dispose
