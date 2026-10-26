{-# LANGUAGE DeriveAnyClass #-}

-- | A component using a custom element made elsewhere: it renders
-- @\<x-signal>@ (which nothing here defines, and need not) and hears its
-- @ping@ events, reading their detail with 'CE.onEvent'.
module Example.Listener (listener, Ping (..)) where

import Control.Monad.State.Class (put)
import Data.Aeson (FromJSON)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.CustomElement qualified as CE
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP

-- | What a @ping@ carries.
data Ping = Ping {from :: Text, times :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

listener :: forall q i o m. (Monad m) => H.Component q i o m
listener = H.component (const (pure Nothing)) render [H.onAction put]
  where
    render :: Maybe Ping -> H.ComponentHTML (Maybe Ping) Empty m
    render heard =
      HH.div_
        [ HH.element (HH.ElemName "x-signal") [HP.attr (HH.AttrName "id") "signal", CE.onEvent "ping" (Just . Just)] []
        , HH.p [HP.class_ (HH.ClassName "heard")] [HH.text (maybe "nothing" (\p -> p.from <> " pinged " <> Text.pack (show p.times)) heard)]
        ]
