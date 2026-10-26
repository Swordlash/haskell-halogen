-- | The trace's machinery, which the driver feeds: the listeners, the
-- registry of live components and the counter their ids come from. Not
-- exposed; "Halogen.Trace" is its public face.
module Halogen.Trace.Internal
  ( Tag (..)
  , Event (..)
  , What (..)
  , summary
  , subscribe
  , listening
  , emit
  , newTagId
  , Entry (..)
  , register
  , deregister
  , restoreState
  , rootNode
  )
where

import Data.Dynamic (Dynamic)
import Data.IntMap.Strict qualified as IntMap
import GHC.Clock (getMonotonicTime)
import HPrelude
import Halogen.Query.HalogenM (ForkId, SubscriptionId)
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep)
import UnliftIO.Exception (tryAny)
import Web.DOM.Internal.Types (Node)

-- | Which component an event is about. The id is unique among every
-- component the program has mounted, in any tree, and never reused; the
-- parent is the id of the component that rendered this one, 'Nothing' for a
-- tree's root.
--
-- The label is what 'Halogen.named' gave the component, or else the name of
-- its state's type, qualified with its module (@Example.Counter.State@).
-- The state's type is known for a component made with 'Halogen.mkComponent'
-- or 'Halogen.component', and not for one whose state no
-- 'Data.Typeable.Typeable' instance describes (a hooks component, whose
-- state is the hooks' own).
data Tag = Tag
  { id :: Int
  , parent :: Maybe Int
  , label :: Text
  , stateType :: Maybe SomeTypeRep
  }
  deriving stock (Eq, Show)

-- | Something a component did, when (in milliseconds on a monotonic clock,
-- whose zero means nothing: subtract one event's time from another's), and
-- which component did it.
data Event = Event
  { at :: Double
  , tag :: Tag
  , what :: What
  }
  deriving stock (Show)

-- | What happened.
data What
  = -- | The component was made, before its first render (so a parent
    -- arrives before its children).
    Mounted
  | -- | Its @Initialize@ ran its synchronous part (up to its first wait, or
    -- its end), after its children's.
    Initialized
  | -- | It is going away: its programs are cancelled and its finalizer is
    -- to run.
    Finalized
  | -- | One walk of its HTML against the page, with the children it renders
    -- that were made or given new input, which took this many milliseconds.
    Rendered {took :: Double}
  | -- | It started handling an action. The action itself is not here: it
    -- would need a 'Show' instance no component is asked for.
    Action
  | -- | Its state changed, to this, when the state's type is known. The
    -- value is what 'restoreState' takes to put it back.
    StateChanged {state :: Maybe Dynamic}
  | -- | It started answering a query, its parent's or the socket's.
    Queried
  | -- | It raised an output.
    Raised
  | Subscribed SubscriptionId
  | Unsubscribed SubscriptionId
  | Forked ForkId
  | -- | A fork ended, however it did: finished, failed or killed.
    ForkEnded ForkId
  | -- | A fork was killed by its component.
    Killed ForkId
  deriving stock (Show)

-- | What happened, as a few words for a log: @action@, @state changed@,
-- @rendered in 0.4 ms@, @forked 3@.
summary :: What -> Text
summary = \case
  Mounted -> "mounted"
  Initialized -> "initialized"
  Finalized -> "finalized"
  Rendered took -> "rendered in " <> tenths took <> " ms"
  Action -> "action"
  StateChanged _ -> "state changed"
  Queried -> "queried"
  Raised -> "raised an output"
  Subscribed sid -> "subscribed " <> show sid
  Unsubscribed sid -> "unsubscribed " <> show sid
  Forked fid -> "forked " <> show fid
  ForkEnded fid -> "fork " <> show fid <> " ended"
  Killed fid -> "killed fork " <> show fid

-- | Milliseconds to one decimal place.
tenths :: Double -> Text
tenths ms = show whole <> "." <> show part
  where
    (whole, part) = (round (ms * 10) :: Integer) `quotRem` 10

-- | The listeners, under the numbers 'subscribe' gave them.
listeners :: IORef (Int, IntMap (Event -> IO ()))
listeners = unsafePerformIO (newIORef (0, mempty))
{-# NOINLINE listeners #-}

-- | Listen to every tree of the program: the listener is called with each
-- event as it happens, on the thread doing the work, in the middle of it.
-- So it should be quick and must not wait; one that keeps the events and
-- looks at them later (as "Halogen.DevTools" and @hspec-halogen@ do) is
-- what it is meant for. A listener that throws is reported on stderr and
-- left in place. Several may listen at once.
--
-- The result stops this listener. Once none is left, the driver goes back
-- to building no events at all.
subscribe :: (Event -> IO ()) -> IO (IO ())
subscribe listener = do
  key <- atomicModifyIORef' listeners $ \(n, ls) -> ((n + 1, IntMap.insert n listener ls), n)
  pure $ atomicModifyIORef' listeners $ \(n, ls) -> ((n, IntMap.delete key ls), ())

-- | Whether anyone listens: what the driver asks before it builds an event,
-- or reads the clock for one.
listening :: IO Bool
listening = not . IntMap.null . snd <$> readIORef listeners
{-# INLINE listening #-}

-- | Tell the listeners, if there are any. Inlined so that the event is not
-- even built when there are none.
emit :: Tag -> What -> IO ()
emit tag what = do
  heard <- listening
  when heard (emitNow tag what)
{-# INLINE emit #-}

emitNow :: Tag -> What -> IO ()
emitNow tag what = do
  at <- (* 1000) <$> getMonotonicTime
  ls <- snd <$> readIORef listeners
  let event = Event {at, tag, what}
  for_ ls $ \listener ->
    tryAny (listener event) >>= \case
      Left e -> hPutStrLn stderr ("Halogen.Trace: a listener failed: " <> show e :: Text)
      Right () -> pass

tagIds :: IORef Int
tagIds = unsafePerformIO (newIORef 1)
{-# NOINLINE tagIds #-}

-- | The id of a component being made.
newTagId :: IO Int
newTagId = atomicModifyIORef' tagIds (\n -> (n + 1, n))

-- | What the registry keeps for a live component: how to put a state into
-- it, and how to find what stands for it on the page.
data Entry = Entry
  { restore :: Dynamic -> IO Bool
  , node :: IO (Maybe Node)
  }

registry :: IORef (IntMap Entry)
registry = unsafePerformIO (newIORef mempty)
{-# NOINLINE registry #-}

-- | Keep a component that is being made, if anyone listens: a component
-- made while nobody does is never kept, so that a program with no listener
-- pays nothing for the registry.
register :: Int -> Entry -> IO ()
register key entry = do
  heard <- listening
  when heard $ atomicModifyIORef' registry (\r -> (IntMap.insert key entry r, ()))
{-# INLINE register #-}

-- | Forget a component that goes away.
deregister :: Int -> IO ()
deregister key = do
  kept <- IntMap.member key <$> readIORef registry
  when kept $ atomicModifyIORef' registry (\r -> (IntMap.delete key r, ()))

-- | Put this state into the component with this id and render it, as a
-- 'Control.Monad.State.put' in the component would: the state goes in
-- through the component's tree's loop, like any of the tree's work, the
-- listeners are told of the change ('StateChanged'), and the component
-- renders. 'True' once it has; 'False', with nothing done, when no live
-- component has the id, or the value is not of its state's type, or its
-- state's type is not known.
--
-- Only components made while someone listened ('subscribe') can be reached:
-- the driver keeps none while nobody does. Open a listener before mounting
-- what it is to look at.
--
-- It waits for the tree to have done it: called from a synchronous effect
-- ('Halogen.Query.liftEffect') of another tree, it holds that tree up until
-- then, so a panel calls it from a @liftIO@.
restoreState :: Int -> Dynamic -> IO Bool
restoreState key value =
  IntMap.lookup key <$> readIORef registry >>= \case
    Nothing -> pure False
    Just entry -> entry.restore value

-- | The node that stands for the component with this id on the page: the
-- root of what it rendered (an element, or a text node for a component that
-- renders only text). 'Nothing' if the component is not kept (see
-- 'restoreState') or renders to something that is not a DOM node.
rootNode :: Int -> IO (Maybe Node)
rootNode key =
  IntMap.lookup key <$> readIORef registry >>= \case
    Nothing -> pure Nothing
    Just entry -> entry.node
