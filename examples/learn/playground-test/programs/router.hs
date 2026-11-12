-- The router layer's program in the playground's test (../Playground.hs,
-- which says what it checks): a hash router: where the page is, and a link
-- that goes elsewhere.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (put)
import Data.Row ((.+), (.==))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.IO.Util qualified as HA
import Halogen.Router qualified as Router
import Halogen.Router.Duplex (RouteDuplex', noArgs, path, root)
import Halogen.Router.Duplex.Generic qualified as Duplex
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

data Route = Home | About
  deriving (Eq, Show, Generic)

route :: RouteDuplex' Route
route = root $ Duplex.sum $ #Home .== noArgs .+ #About .== path "about" noArgs

data Action = Start | Go (Router.Follow Route)

page :: Router.Router Route -> H.Component q () o BrowserDOM
page router =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure Home
      , H.render = \r ->
          HH.div_
            [ HH.p_ [HH.text ("route: " <> T.pack (show r))]
            , Router.link router Go About [] [HH.text "About"]
            ]
      , H.eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = \case
                  Start -> put =<< Router.current router
                  Go follow -> do
                    Router.follow router follow
                    put =<< Router.current router
              , H.initialize = Just Start
              }
      }

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  router <- Router.newHashRouter Home route
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI (page router) () app
  HA.keepForNextRun (socket.dispose >> Router.stop router)
