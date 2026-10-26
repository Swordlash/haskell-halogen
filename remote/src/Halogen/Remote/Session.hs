-- | One client's side of the program: a component mounted in 'RemoteDOM',
-- whose operations go out as messages and whose events come back in.
--
-- A session is made with what sends a message to the client
-- ('newSession'), mounts one component ('mount'), is handed every message
-- from the client ('receive') and ends with 'dispose'. What carries the
-- messages is the caller's: "Halogen.Remote.Server" makes a session per
-- WebSocket connection, "Halogen.Remote.Loopback" one per element of a page,
-- and a test one whose wire is a list.
--
-- __Batches.__ Operations are not sent one by one. The first operation
-- after a message has gone wakes the session's sender, which waits until no
-- render of the tree is under way, yields once (so that other work about to
-- record joins it), and sends everything recorded by then as one message. A
-- batch never ends in the middle of a render: 'mount' counts each render of
-- the tree as under way while it runs, and the sender takes nothing while
-- one is. That is what lets a node taken out and put back within a render
-- (the node of a child component whose parent replaced the element around
-- it) survive: a node taken out and not put back by the end of the batch is
-- gone, on both sides, and only then. The first message of a session holds
-- the first render, after a @clear@ that empties what the client showed
-- before.
--
-- __What cannot cross.__ The component's program runs here, the page there,
-- and the wire between them takes time, so:
--
-- * A ref ('Halogen.Query.HalogenM.getRef') is a node of the session's
--   mirror, not an element of the page: an effect that talks to the browser
--   through it (focusing, measuring, a canvas) does nothing useful, as under
--   the in-memory DOM.
-- * 'Web.Event.Event.preventDefault' in a handler comes too late: the page
--   has finished with the event before the program hears of it. The client
--   prevents the default itself, of every @submit@ it listens for and every
--   @click@ on an @a@ element it listens for, and of nothing else.
--   'Halogen.HTML.Events.preventingDefault' does not help: what a handler
--   does to its event is decided in the listener, which runs here.
-- * Focus and selection are the page's. The client sets an element's
--   @value@ only when it differs from what the element holds, so text being
--   typed is not undone, and every event reports the values of its target's
--   form controls, which the mirror takes as the element's.
-- * The client starts empty: a page is not hydrated from HTML rendered
--   ahead of the program.
module Halogen.Remote.Session
  ( Session
  , newSession
  , mount
  , receive
  , dispose
  , run
  , liveNodes
  , liveListeners
  , SessionError (..)
  )
where

import Control.Concurrent.STM qualified as STM
import Data.Aeson.Text (encodeToLazyText)
import Data.Foreign (toForeign)
import Data.Sequence qualified as Seq
import Data.Text.Lazy qualified as Lazy
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..), RenderSpec (..))
import Halogen.IO.Driver qualified as Driver
import Halogen.JSBits (inBrowser)
import Halogen.Remote.Client qualified as Client
import Halogen.Remote.Internal (Recorder, RemoteDOM, runRemoteDOM)
import Halogen.Remote.Internal qualified as Internal
import Halogen.Remote.Protocol (Batch (..), EventMessage (..), Op (..), decodeEventMessage, encodeBatch)
import Halogen.VDom.Driver (renderSpec)
import Protolude hiding (yield)
import UnliftIO (tryAny)
import UnliftIO.Concurrent (yield)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Web.Event.Internal.Types (Event (..))

-- | One client's side of the wire.
data Session = Session
  { recorder :: Recorder
  , send :: Text -> IO ()
  , held :: STM.TVar Bool
  -- ^ While the component is being mounted, nothing is sent, so that the
  -- first message holds its first render.
  , mounted :: IORef (Maybe (RemoteDOM ()))
  -- ^ What disposes of the component's tree, once it has one.
  , sender :: IORef (Maybe (Async ()))
  }

data SessionError
  = -- | A session shows one component; 'mount' was called a second time.
    AlreadyMounted
  deriving stock (Show)

instance Exception SessionError

-- | A session whose messages to the client go to @send@, which is called
-- from a thread of the session's own, one message at a time. A @send@ that
-- throws ends the sending (the client has gone); the caller disposes of the
-- session.
newSession :: (Text -> IO ()) -> IO Session
newSession send = do
  recorder <- Internal.newRecorder
  held <- STM.newTVarIO True
  session <- Session recorder send held <$> newIORef Nothing <*> newIORef Nothing
  STM.atomically $ STM.writeTVar recorder.pending (Seq.singleton Clear)
  thread <- async (sending session 1)
  writeIORef session.sender (Just thread)
  pure session

sending :: Session -> Int -> IO ()
sending session n = do
  woken <- STM.atomically $ do
    STM.readTVar session.held >>= STM.check . not
    Internal.ready session.recorder
  when woken $ do
    -- Let what is about to record join the batch. A render that starts
    -- meanwhile is waited for: the batch is taken between renders.
    yield
    Internal.takeBatch session.recorder >>= \case
      Nothing -> pass
      -- Everything it held was about nodes it forgot.
      Just [] -> sending session n
      Just ops -> do
        sent <- tryAny (session.send (encodeBatch (Batch n ops)))
        case sent of
          Left _ -> pass
          Right () -> sending session (n + 1)

-- | Mount the component in the session's page: the first message the client
-- gets is its first render. A session mounts one component, once.
--
-- It is 'Halogen.VDom.Driver.runUI' into the session's body, with each
-- render counted as under way while it runs, so that no batch ends inside
-- one (see the module's header).
mount :: Session -> Component q i o RemoteDOM -> i -> IO (HalogenSocket q o RemoteDOM)
mount session component input = do
  already <- isJust <$> readIORef session.mounted
  when already $ throwIO AlreadyMounted
  let recorder = session.recorder
      spec = counted (renderSpec (Internal.documentHandle recorder) (Internal.bodyHandle recorder))
  socket <-
    run session (Driver.runUI spec component input)
      `finally` STM.atomically (STM.writeTVar session.held False)
  writeIORef session.mounted (Just socket.dispose)
  pure socket
  where
    counted :: RenderSpec RemoteDOM r -> RenderSpec RemoteDOM r
    counted RenderSpec {render, renderChild, removeChild, dispose = disposeOf, sameElement, rootNode} =
      RenderSpec
        { render = \handler child html previous -> Internal.whileRendering (render handler child html previous)
        , renderChild
        , removeChild
        , dispose = disposeOf
        , sameElement
        , rootNode
        }

-- | A message from the client: an event for one of the component's
-- listeners, which runs on the caller's thread and hands its action to the
-- component's loop, as a page's callback does. A message that is not one, or
-- that comes for a listener the component has since taken away, is dropped.
receive :: Session -> Text -> IO ()
receive session text = for_ (decodeEventMessage text) $ \message -> do
  event <- eventOf message
  Internal.deliver session.recorder message event

-- | The event a handler reads: in a browser the JavaScript object the
-- client's JSON spells, natively the JSON itself, which "Data.Foreign"
-- reads there.
eventOf :: EventMessage -> IO Event
eventOf message
  | inBrowser = Event <$> Client.parse (Lazy.toStrict (encodeToLazyText message.event))
  | otherwise = pure (Event (toForeign message.event))

-- | End the session: nothing more is sent, the component's tree is
-- disposed of (what that removes is not sent: the client is going too), and
-- the mirror and the listeners are forgotten. Disposing of a session twice
-- does nothing more.
dispose :: Session -> IO ()
dispose session = do
  Internal.close session.recorder
  traverse_ cancel =<< atomicModifyIORef' session.sender (Nothing,)
  atomicModifyIORef' session.mounted (Nothing,) >>= traverse_ (void . tryAny . run session)
  Internal.close session.recorder

-- | Run a program in the session's monad: a query of the mounted component,
-- say, through its socket.
run :: Session -> RemoteDOM a -> IO a
run session = runRemoteDOM session.recorder

-- | How many nodes the session's mirror holds, the document and the body
-- included.
liveNodes :: Session -> IO Int
liveNodes session = Internal.liveNodes session.recorder

-- | How many listeners the page is listening for.
liveListeners :: Session -> IO Int
liveListeners session = Internal.liveListeners session.recorder
