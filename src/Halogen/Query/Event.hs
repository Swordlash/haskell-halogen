module Halogen.Query.Event where

import Halogen.Subscription
import Halogen.VDom.DOM.Monad
import Protolude
import Web.DOM.Types

eventListener
  :: MonadDOM m
  => EventType
  -> EventTarget m
  -> (Event m -> Maybe a)
  -> Emitter m a
eventListener eventType target f =
  makeEmitter $ \push -> do
    listener <- mkEventListener $ \ev -> traverse_ push (f ev)
    addEventListener eventType listener target
    pure $ removeEventListener eventType listener target
