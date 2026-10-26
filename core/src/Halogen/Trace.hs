-- | A trace of what the program's component trees do: components made and
-- gone, renders and how long they took, actions, state changes, queries,
-- outputs, subscriptions and forks, as 'Event's that a listener receives as
-- they happen. It is what "Halogen.DevTools" shows, and what @hspec-halogen@
-- prints when a test fails.
--
-- It costs nothing when nobody listens: the driver asks 'listening' (one
-- read of a reference) before it builds an event or reads the clock, and
-- keeps no registry of components. With a listener every tree of the
-- program reports to it, whichever monad and renderer it runs in.
--
-- > stop <- Trace.subscribe $ \event ->
-- >   when (event.tag.label == "Example.Counter.State") $
-- >     putStrLn (Trace.summary event.what)
--
-- A component is known by its 'Tag': an id, its parent's, and a label, the
-- name of its state's type unless 'Halogen.named' gave it another. Where the
-- state's type is known ('Halogen.mkComponent' and 'Halogen.component' ask
-- for 'Data.Typeable.Typeable' for that), a 'StateChanged' carries the new
-- state as a 'Data.Dynamic.Dynamic', which 'restoreState' can put back
-- later.
module Halogen.Trace
  ( -- * Events
    Event (..)
  , Tag (..)
  , What (..)
  , summary

    -- * Listening
  , subscribe
  , listening

    -- * Reaching a component
  , restoreState
  , rootNode
  )
where

import Halogen.Trace.Internal
