-- | A component's DOM over a wire: Halogen as a LiveView.
--
-- A component runs somewhere other than the page that shows it (a native
-- server, the tutorial's own runtime), its DOM operations cross a wire to a
-- thin JavaScript client (@jsbits/remote.js@) that applies them to a real
-- page, and the page's events travel back to the component's handlers.
--
-- * "Halogen.Remote.DOM": 'RemoteDOM', the backend.
-- * "Halogen.Remote.Session": one client's side of the program.
-- * "Halogen.Remote.Protocol": what crosses the wire.
-- * "Halogen.Remote.Server": a component served over WebSockets (native).
-- * "Halogen.Remote.Loopback": the same pipeline inside one page.
module Halogen.Remote
  ( module Halogen.Remote.DOM
  , module Halogen.Remote.Session
  )
where

import Halogen.Remote.DOM
import Halogen.Remote.Session
