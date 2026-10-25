module Halogen
  ( HalogenSocket (..)
  , component
  , (&)
  , module Halogen.Data.Slot
  , module Halogen.Component
  , module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.Query
  )
where

import HPrelude
import Halogen.Component (Component (..), ComponentSlot, ComponentSlotBox, ComponentSpec' (..), EvalSpec (..), Handler, componentSlot, componentSpec, defaultEval, hoist, mkComponent, mkEval, onAction, onFinalize, onInitialize, onQuery, onReceive)
import Halogen.Data.Slot (Slot, VoidF)
import Halogen.HTML (ComponentHTML)
import Halogen.HTML.Core (AttrName (..), ClassName (..), ElemName (..), HTML, Namespace (..), PropName (..))
import Halogen.IO.Driver (HalogenSocket (..))
import Halogen.Query (ForkId, HalogenF (..), HalogenM (..), HalogenQ (..), RefLabel (..), Request, SubscriptionId, Tell, fork, getHTMLElementRef, getRef, join, kill, liftEffect, mkRequest, mkTell, newRefLabel, query, queryAll, raise, request, requestAll, subscribe, subscribe', tell, tellAll, unsubscribe)

-- | A component from its initial state, its render and its handlers:
--
-- > component = H.component initialState render [H.onAction handleAction]
--
-- The handlers are 'onAction', 'onQuery', 'onInitialize', 'onFinalize' and
-- 'onReceive', in any order, each at most once; a component with none
-- renders its state and does nothing else. The same component spelled out
-- is @H.mkComponent (H.componentSpec initialState render & H.onAction
-- handleAction)@, and @H.mkComponent H.ComponentSpec {..}@ with an @eval@
-- written out or made by 'mkEval' is the way to answer the component's
-- messages with one function.
component
  :: forall state query action slots input output m
   . (input -> m state)
  -> (state -> HTML (ComponentSlot slots m action) action)
  -> [Handler state query action slots input output m]
  -> Component query input output m
component initialState render handlers = mkComponent (foldl' (&) (componentSpec initialState render) handlers)
