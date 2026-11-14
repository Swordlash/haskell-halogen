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
-- gone, on both sides, and only then. The first batch of a session holds
-- the first render, after a @clear@ that empties what the client showed
-- before (only the probe's answers can come before it).
--
-- __What cannot cross.__ The component's program runs here, the page there,
-- and the wire between them takes time, so:
--
-- * A ref ('Halogen.Query.HalogenM.getRef') is a node of the session's
--   mirror, not an element of the page. What a component does to the page
--   through it with the @Web.*@ types does cross: natively a handle answers
--   for itself ("Halogen.JSBits.Value"'s @withNativeObject@), and a focus, @blur@,
--   @click@, @select@, @scrollIntoView@, a dialog's @showModal@, @show@ and
--   @close@, and the document's title are recorded as operations the client
--   performs from an allowlist ("Halogen.Remote.Protocol"), in the batch
--   being recorded (a render's, after the operations that made the node, or
--   the next). What only the page can answer cannot cross in time, and a
--   read keeps its inert answer: measuring, @document.activeElement@, a
--   dialog's @open@ (which Escape changes in the page), the clipboard, a
--   canvas, a method with a result. Anything off the allowlist, and a call
--   on a node the session has forgotten, does nothing.
-- * What a handler decides comes too late for the page: the page has
--   finished with an event before the program hears of it, so
--   'Web.Event.Event.preventDefault' called from an action does nothing, and
--   neither does a choice only the handler's function can make. What the
--   page has to decide while it dispatches the event has to be declared, as
--   data it can read: a handler with a match ('Halogen.HTML.Events.matching':
--   a plain left click, keys, any event) is sent only the events that
--   match, and the page prevents their default
--   ('Halogen.HTML.Events.preventingDefault') or stops them
--   ('Halogen.HTML.Events.stoppingPropagation') as declared. A handler that
--   declares 'Halogen.HTML.Events.preventingDefault' with no match has the
--   default of every event of its type prevented, since the page cannot know
--   which of them the handler would make an action for: over the wire,
--   declare the match. A "Halogen.Router" link declares the plain left
--   click. For a handler that declares nothing, the client prevents the
--   default of every @submit@ it listens for and of a @click@ on an @a@
--   element that the browser would follow in this tab (the main button, no
--   modifier key, a link with no other @target@ and no @download@), and of
--   nothing else.
-- * Focus and selection are the page's, except where the component moves
--   the focus itself (above). The client sets an element's
--   @value@ only when it differs from what the element holds, so text being
--   typed is not undone, and every event reports the values of its target's
--   form controls, which the mirror takes as the element's.
-- * The client starts empty: a page is not hydrated from HTML rendered
--   ahead of the program.
--
-- __Whether it is busy.__ 'activity' reads the session's tree and folds in
-- what the session has not sent: over a wire, a program is idle once the
-- page shows what it rendered. The client forwards the questions of a page's
-- probe (@__halogenProbe.activity()@, which @haskell-halogen-playwright@
-- asks after every action) and 'receive' answers them through the sender,
-- in order with the batches: "Halogen.Remote.Protocol" has the messages and
-- the argument for their order.
--
-- __Identity.__ Every session has a number unique in the process
-- ('sessionId'), which 'Eq', 'Ord' and 'Show' go by.
module Halogen.Remote.Session
  ( Session
  , newSession
  , sessionId
  , mount
  , isMounted
  , receive
  , activity
  , dispose
  , ended
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
import Halogen.Activity (Activity (..))
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..), RenderSpec (..))
import Halogen.IO.Driver qualified as Driver
import Halogen.JSBits (inBrowser)
import Halogen.Remote.Client qualified as Client
import Halogen.Remote.Internal (Recorder, RemoteDOM, runRemoteDOM)
import Halogen.Remote.Internal qualified as Internal
import Halogen.Remote.Protocol (Batch (..), EventMessage (..), FromPage (..), Op (..), decodeFromPage, encodeBatch, encodeProbeAnswer)
import Halogen.VDom.Driver (renderSpec)
import Protolude hiding (yield)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show qualified as Show
import UnliftIO (tryAny)
import UnliftIO.Concurrent (yield)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Web.Event.Internal.Types (Event (..))

-- | One client's side of the wire.
data Session = Session
  { ident :: Int
  , recorder :: Recorder
  , send :: Text -> IO ()
  , phase :: STM.TVar Phase
  -- ^ Until the component is mounted, nothing but the probe's answers is
  -- sent, so that the first batch holds its first render.
  , mounted :: IORef (Maybe Tree)
  -- ^ The component's tree, once it has one.
  , sender :: IORef (Maybe (Async ()))
  , flushes :: STM.TVar (Int, Int)
  -- ^ How many batches the sender has begun to take, and how many it has
  -- finished with (sent, or found empty): one is under way while they
  -- differ.
  , questions :: STM.TVar (Seq (Int, Int))
  -- ^ The probe's questions not yet answered, in the order they came: each
  -- with the number of batches that must be finished with first (as the
  -- recorder counts the batches taken).
  }

-- | What the session keeps of its component's tree.
data Tree = Tree
  { dispose :: RemoteDOM ()
  , activity :: RemoteDOM Activity
  }

data Phase = Unmounted | Mounting | Live
  deriving stock (Eq)

instance Eq Session where
  a == b = a.ident == b.ident

instance Ord Session where
  compare a b = compare a.ident b.ident

-- | The session's number.
instance Show Session where
  showsPrec d session = Show.showsPrec d session.ident

data SessionError
  = -- | A session shows one component; 'mount' was called a second time.
    AlreadyMounted
  deriving stock (Show)

instance Exception SessionError

-- | The numbers the process has given its sessions.
sessionNumbers :: IORef Int
sessionNumbers = unsafePerformIO (newIORef 1)
{-# NOINLINE sessionNumbers #-}

-- | The session's number, unique in the process: what tells two sessions
-- apart (a server's log, a test's map of its pages).
sessionId :: Session -> Int
sessionId session = session.ident

-- | A session whose messages to the client go to @send@, which is called
-- from a thread of the session's own, one message at a time. A @send@ that
-- throws ends the sending (the client has gone); the caller disposes of the
-- session.
newSession :: (Text -> IO ()) -> IO Session
newSession send = do
  ident <- atomicModifyIORef' sessionNumbers (\n -> (n + 1, n))
  recorder <- Internal.newRecorder
  session <-
    Session ident recorder send
      <$> STM.newTVarIO Unmounted
      <*> newIORef Nothing
      <*> newIORef Nothing
      <*> STM.newTVarIO (0, 0)
      <*> STM.newTVarIO Seq.empty
  STM.atomically $ STM.writeTVar recorder.pending (Seq.singleton Clear)
  thread <- async (sending session 1)
  writeIORef session.sender (Just thread)
  pure session

-- | What the sender does next.
data Next = Stop | Answer [Int] | Flush

-- | The sender: answer the questions that are due, first, then send what is
-- recorded, between renders, as the module's header says. A question is
-- due once the batches it waits for are finished with, so a render that
-- never stops recording (an animation) does not keep it from its answer.
sending :: Session -> Int -> IO ()
sending session n = do
  let recorder = session.recorder
  next <- STM.atomically $ do
    open <- STM.readTVar recorder.open
    if not open
      then pure Stop
      else do
        (started, finished) <- STM.readTVar session.flushes
        questions <- STM.readTVar session.questions
        let (due, later) = Seq.spanl (\(_, after) -> after <= finished) questions
        if not (Seq.null due)
          then STM.writeTVar session.questions later $> Answer (toList (fst <$> due))
          else do
            STM.readTVar session.phase >>= STM.check . (== Live)
            Internal.ready recorder >>= STM.check
            STM.writeTVar session.flushes (started + 1, finished)
            pure Flush
  case next of
    Stop -> pass
    Answer numbers -> do
      reading <- activity session
      sent <- tryAny (for_ numbers (\q -> session.send (encodeProbeAnswer q reading)))
      when (isRight sent) $ sending session n
    Flush -> do
      -- Let what is about to record join the batch. A render that starts
      -- meanwhile is waited for: the batch is taken between renders.
      yield
      let finished = STM.atomically (STM.modifyTVar' session.flushes (second (+ 1)))
      Internal.takeBatch recorder >>= \case
        Nothing -> finished
        -- Everything it held was about nodes it forgot.
        Just [] -> finished >> sending session n
        Just ops -> do
          sent <- tryAny (session.send (encodeBatch (Batch n ops)))
          finished
          when (isRight sent) $ sending session (n + 1)

-- | Mount the component in the session's page: the first batch the client
-- gets is its first render. A session mounts one component, once.
--
-- A component that fails to mount (its initial state or its first render
-- throws) ends the session: nothing has been sent, its sender stops, the
-- mirror is forgotten, and the exception is thrown on, so a caller has
-- nothing of the session left to free.
--
-- It is 'Halogen.VDom.Driver.runUI' into the session's body, with each
-- render counted as under way while it runs, so that no batch ends inside
-- one (see the module's header).
mount :: Session -> Component q i o RemoteDOM -> i -> IO (HalogenSocket q o RemoteDOM)
mount session component input = do
  fresh <- STM.atomically $ do
    now <- STM.readTVar session.phase
    when (now == Unmounted) $ STM.writeTVar session.phase Mounting
    pure (now == Unmounted)
  unless fresh $ throwIO AlreadyMounted
  let recorder = session.recorder
      spec = counted (renderSpec (Internal.documentHandle recorder) (Internal.bodyHandle recorder))
  socket <- run session (Driver.runUI spec component input) `onException` dispose session
  writeIORef session.mounted (Just Tree {dispose = socket.dispose, activity = socket.activity})
  STM.atomically (STM.writeTVar session.phase Live)
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

-- | Whether a component has been mounted (and the session not disposed of
-- since).
isMounted :: Session -> IO Bool
isMounted session = isJust <$> readIORef session.mounted

-- | A message from the client: an event for one of the component's
-- listeners, which runs on the caller's thread and hands its action to the
-- component's loop, as a page's callback does; or a question of the
-- probe's, which the sender answers in its turn (see
-- "Halogen.Remote.Protocol"). A message that is neither, or an event that
-- comes for a listener the component has since taken away, is dropped.
receive :: Session -> Text -> IO ()
receive session text = for_ (decodeFromPage text) $ \case
  PageEvent message -> do
    event <- eventOf message
    Internal.deliver session.recorder message event
  ProbeQuestion question -> asked session question

-- | Queue a question for the sender, after the batch that will take what is
-- recorded now, if anything is, and after the batch being sent.
asked :: Session -> Int -> IO ()
asked session question = STM.atomically $ do
  let recorder = session.recorder
  open <- STM.readTVar recorder.open
  when open $ do
    now <- STM.readTVar session.phase
    pending <- STM.readTVar recorder.pending
    -- The batch being sent, if one is, was the last taken; what is pending
    -- goes in the next.
    taken <- STM.readTVar recorder.taken
    let waitsFor = taken + (if now == Live && not (Seq.null pending) then 1 else 0)
    STM.modifyTVar' session.questions (Seq.|> (question, waitsFor))

-- | What the session is doing: its tree's reading ('trees' 0 before 'mount'
-- and after 'dispose'), with 'running' at least 1 while the session has
-- anything the client has not been sent (operations recorded and not yet
-- taken into a batch, a batch being sent, a render under way) or is
-- mounting. Idle means the client has been sent everything the component
-- rendered.
--
-- The tree is read first, then the session: work that ends between the
-- two recorded what it rendered before it ended, so it is seen unsent.
activity :: Session -> IO Activity
activity session = do
  tree <-
    readIORef session.mounted >>= \case
      Nothing -> pure Activity {trees = 0, running = 0, waiting = 0}
      Just t -> run session t.activity
  let recorder = session.recorder
  unsent <- STM.atomically $ do
    open <- STM.readTVar recorder.open
    now <- STM.readTVar session.phase
    pending <- STM.readTVar recorder.pending
    depth <- STM.readTVar recorder.rendering
    (started, finished) <- STM.readTVar session.flushes
    pure $
      open
        && ( now == Mounting
               || depth > 0
               || started /= finished
               || (now == Live && not (Seq.null pending))
           )
  pure tree {running = max tree.running (if unsent then 1 else 0)}

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
  atomicModifyIORef' session.mounted (Nothing,) >>= traverse_ (void . tryAny . run session . (.dispose))
  Internal.close session.recorder

-- | Return once the session has been disposed of: what a server waits for
-- to close the connection of a session someone else disposed of.
ended :: Session -> IO ()
ended session = STM.atomically (STM.readTVar session.recorder.open >>= STM.check . not)

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
