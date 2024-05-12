module Halogen.Query.Event where

import Halogen.Subscription
import Halogen.VDom.DOM.Monad
import Protolude
import Web.Event.Event

eventListener
  :: MonadDOM m
  => EventType
  -> EventTarget
  -> (Event -> Maybe a)
  -> Emitter m a
eventListener eventType target f =
  makeEmitter $ \push -> do
    listener <- mkEventListener $ \ev -> traverse_ push (f ev)
    addEventListener eventType listener target
    pure $ removeEventListener eventType listener target
