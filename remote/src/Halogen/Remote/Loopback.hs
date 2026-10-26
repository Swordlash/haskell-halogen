-- | The whole pipeline inside one page: a component mounted in 'RemoteDOM'
-- whose messages go to a client in an element of the same page, and whose
-- events come back from it, with every message also emitted for anyone
-- watching ('traffic').
--
-- It is "Halogen.Remote.Server" minus the socket: the same session, the
-- same protocol, the same client, so what a page shows here is what a page
-- served by a native program shows, and the messages are the ones that
-- would cross the network. That makes it the way to see the wire (the
-- tutorial's lesson shows its traffic) and to test a component meant to be
-- served, in a browser, without a server.
--
-- Only in a browser: natively 'mountIn' throws 'NotAvailable'.
module Halogen.Remote.Loopback
  ( Loopback (..)
  , Traffic (..)
  , LoopbackError (..)
  , mountIn
  , mountWatched
  )
where

import Halogen.Component (Component)
import Halogen.JSBits (freeCallback, inBrowser, jsValText, mkCallback)
import Halogen.Remote.Client qualified as Client
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Session qualified as Session
import Halogen.Subscription qualified as HS
import Protolude
import UnliftIO.IORef (newIORef, readIORef, writeIORef)
import Web.DOM.Internal.Types (HTMLElement (..))

-- | A message on the wire, as it crossed.
data Traffic
  = -- | The program's batch of operations, to the page.
    ToPage Text
  | -- | An event the page heard, to the program.
    ToProgram Text
  deriving stock (Eq, Show)

data Loopback = Loopback
  { dispose :: IO ()
  -- ^ End the session and the component's tree, and empty the element.
  , traffic :: HS.Emitter IO Traffic
  -- ^ Every message, both ways, as it crosses, from when the subscriber
  -- subscribes: the first render has usually gone by then, since it is
  -- sent as soon as 'mountIn' has mounted the component. To see every
  -- message, the first included, watch from the start with
  -- 'mountWatched'.
  }

data LoopbackError
  = -- | Not in a browser: there is no page to show the component in.
    NotAvailable
  deriving stock (Show)

instance Exception LoopbackError

-- | Show the component in the element, through a client the element gets
-- for itself: the element is the client's, which empties it, so it should
-- be one nothing else renders into. Throws 'NotAvailable' natively.
mountIn :: HTMLElement -> Component q i o RemoteDOM -> i -> IO Loopback
mountIn = mountWatched (const pass)

-- | 'mountIn', with every message also handed to the watcher as it
-- crosses, from the first: what a page that shows the wire's traffic wants,
-- since a subscriber to 'traffic', which only exists once 'mountIn' has
-- returned, may come after the first render has been sent. The watcher is
-- called on the thread that sends or receives the message, so it should
-- hand the message on rather than work on it (a 'HS.Listener''s
-- 'HS.notify', to an emitter a component subscribed to before mounting).
mountWatched :: (Traffic -> IO ()) -> HTMLElement -> Component q i o RemoteDOM -> i -> IO Loopback
mountWatched watch (HTMLElement element) component input = do
  unless inBrowser $ throwIO NotAvailable
  HS.Subscribe {emitter, listener} <- HS.create
  -- The client sends before the session is there to receive only if the
  -- page dispatches an event in the moment between the two, when no
  -- listener has been added yet: there is nothing to receive then.
  sessionRef <- newIORef Nothing
  callback <- mkCallback $ \value -> do
    let text = jsValText value
    watch (ToProgram text)
    HS.notify listener (ToProgram text)
    readIORef sessionRef >>= traverse_ (`Session.receive` text)
  client <- Client.attach element callback
  session <- Session.newSession $ \text -> do
    watch (ToPage text)
    HS.notify listener (ToPage text)
    Client.apply client text
  writeIORef sessionRef (Just session)
  _ <- Session.mount session component input
  pure
    Loopback
      { dispose = do
          Session.dispose session
          Client.detach client
          freeCallback callback
      , traffic = emitter
      }
