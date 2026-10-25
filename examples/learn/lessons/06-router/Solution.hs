{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (Route (..), route, component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets, modify)
import Data.Row ((.+), (.==))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Router qualified as Router
import Halogen.Router.Duplex qualified as RD
import Halogen.Router.Duplex.Generic qualified as RD

data Route = Home | About | Post Int
  deriving (Eq, Show, Generic)

route :: RD.RouteDuplex' Route
route =
  RD.root
    $ RD.sum
    $ (#Home .== RD.noArgs)
      .+ (#About .== RD.path "about" RD.noArgs)
      .+ (#Post .== RD.path "post" (RD.int RD.segment))

data State = State {router :: Router.Router Route, current :: Route}

data Action = Arrived Route | Go (Router.Follow Route)

-- The program around this module makes the router, with
-- Router.newHashRouter Home route, hands it to the component as its input,
-- and calls Router.stop on it before the next compile runs.
component :: forall q o m. (MonadIO m) => H.Component q (Router.Router Route) o m
component =
  H.component initialState render [H.onInitialize watchRoutes, H.onAction handleAction]
  where
    initialState router = State router <$> Router.current router
    render st =
      HH.div_
        [ HH.nav_
            [ link Home "Home"
            , HH.text " | "
            , link About "About"
            , HH.text " | "
            , link (Post 1) "Post 1"
            , HH.text " | "
            , link (Post 2) "Post 2"
            ]
        , HH.p_ [HH.text ("You are at " <> T.pack (show st.current))]
        ]
      where
        link r label = Router.link st.router Go r [] [HH.text label]
    watchRoutes = do
      router <- gets (.router)
      _ <- H.subscribe (Arrived <$> Router.changes router)
      pure ()
    handleAction action = case action of
      Arrived r -> modify (\st -> st {current = r})
      Go follow -> do
        router <- gets (.router)
        Router.follow router follow
