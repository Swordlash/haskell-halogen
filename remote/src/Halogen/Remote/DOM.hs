-- | 'RemoteDOM', the backend whose DOM is on the other side of a wire.
--
-- It is one more backend of the same reconciler, as the in-memory DOM is
-- and Pixi's and Three.js's are: a component does not change to be sent
-- over a wire, only the monad its entry point mounts it in. Each operation
-- the reconciler and the driver make is applied to a mirror of the page,
-- which answers what they read back (a node's parent, its next sibling, a
-- property's value), and recorded, for the session ("Halogen.Remote.Session")
-- to send to the client; "Halogen.Remote.Protocol" says what is sent.
--
-- 'RemoteDOM' has every instance the driver asks of a component monad
-- ('MonadUnliftIO', 'Halogen.VDom.DOM.Monad.MonadBrowserDOM', forking,
-- killing, running in parallel, making UUIDs), so a component polymorphic
-- in its monad runs in it as it does in a page. Storage is the session's,
-- in memory, and goes with it.
module Halogen.Remote.DOM
  ( RemoteDOM
  )
where

import Halogen.Remote.Internal (RemoteDOM)
