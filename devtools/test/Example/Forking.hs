{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

-- | A holder with a button that takes its child out, and a child that
-- forks a program which never ends: taking the child out cancels the
-- fork, which reports its end after the child was finalized.
module Example.Forking (component) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (put)
import Data.Row (Empty, type (.==))
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP

newtype Holder = Holder {holding :: Bool}

data Remove = Remove

component :: forall q i o m. (MonadIO m) => H.Component q i o m
component = H.component (\_ -> pure Holder {holding = True}) render [H.onAction (\Remove -> put Holder {holding = False})]
  where
    render :: Holder -> H.ComponentHTML Remove ("child" .== H.Slot H.VoidF Void ()) m
    render holder =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "remove"), HE.onClick (const Remove)] [HH.text "remove"]
        , if holder.holding then HH.slot_ "child" () worker () else HH.text ""
        ]

data Worker = Worker

worker :: forall m. (MonadIO m) => H.Component H.VoidF () Void m
worker =
  H.named "Worker" $
    H.component
      (\() -> pure Worker)
      (\Worker -> HH.text "working" :: H.ComponentHTML () Empty m)
      [H.onInitialize (void (H.fork (forever (liftIO (threadDelay 20000)))))]
