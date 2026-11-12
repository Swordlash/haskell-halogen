-- The net layer's program in the playground's test (../Playground.hs, which
-- says what it checks): a fetch of a file the test's server serves.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (put)
import Data.Text (Text)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.IO.Util qualified as HA
import Halogen.Net.Fetch qualified as Fetch
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

data Action = Load

fetched :: H.Component q () o BrowserDOM
fetched =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure ("waiting" :: Text)
      , H.render = \t -> HH.p_ [HH.text ("fetched: " <> t)]
      , H.eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = \Load -> do
                  result <- Fetch.fetch (Fetch.request Fetch.GET "learn-test/hello.txt")
                  body <- either (pure . Left) Fetch.text result
                  put (either (\e -> "failed: " <> e.name <> ": " <> e.message) id body)
              , H.initialize = Just Load
              }
      }

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI fetched () app
  HA.keepForNextRun socket.dispose
