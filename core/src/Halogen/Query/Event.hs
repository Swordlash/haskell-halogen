module Halogen.Query.Event where

import HPrelude
import Halogen.Subscription
import Halogen.VDom.DOM.Monad
import Web.Event.Event (Event, EventType)

eventListener
  :: (MonadDOM m)
  => EventType
  -> DomEventTarget m
  -> (Event -> Maybe a)
  -> Emitter m a
eventListener eventType target f =
  makeEmitter $ \push -> do
    listener <- mkEventListener $ \ev -> traverse_ push (f ev)
    addEventListener eventType listener target
    pure $ removeEventListener eventType listener target
