{-# LANGUAGE TypeFamilies #-}

-- | The backend: a mirror of the page the client shows, and the operations
-- that change it, recorded for the session to send.
--
-- Exposed for tests and tools that want to look at a recorder without a
-- session's sender in the way ("Halogen.Remote.Session" is how a program
-- uses it); what it exports may change between minor versions.
--
-- Every handle the driver holds ('Node', 'Element', 'Window', …) is a value
-- holding the number of a node of the mirror, made with 'toJSInt', which is
-- a number in a browser and an 'Int' natively: the mirror is looked up by
-- it in the recorder's table. A handle is never anything but a value of the
-- kind its backend makes, so code that hands one to the browser (an effect
-- given a 'Halogen.Query.HalogenM.getRef' element) gets an error from the
-- browser about a number, rather than the browser reading a Haskell record
-- as a JavaScript value.
--
-- __A node taken out and not put back by the end of the batch is gone, on
-- both sides.__ The browser forgets a node when nothing holds it, which a
-- client keeping a table of nodes cannot know unless it is told, so the
-- mirror and the client forget such a node themselves, with its subtree and
-- the listeners on them. Not when it is taken out, though: the driver keeps
-- a child component across its parent's render, and when the parent's
-- render replaces the element around the child's slot (a @div@ that becomes
-- a @section@), the old element is taken out with the child's node still in
-- it, and that same node is then put into the new element. So a removal
-- only detaches the node, which the recorder notes, and an insertion
-- attaches it again wherever it goes; when the batch is taken
-- ('takeBatch'), each noted node that still has no parent is forgotten with
-- what is under it then. The client applies the same rule to the batch it
-- receives (@jsbits/remote.js@).
--
-- That rule needs a batch never to end in the middle of a render, or the
-- node would be forgotten between its removal and its insertion. The
-- session wraps every render of its tree in 'whileRendering', a count of
-- the renders under way (they nest, a parent's around its children's), and
-- a batch is taken only while it is zero.
--
-- What the batch would say about a node that is gone by its end (the halt
-- of a removed element's children, which takes each out of it and stops
-- its listeners, a property set on it) is left out of it: the client
-- forgets the node with everything under it, so none of it would show.
module Halogen.Remote.Internal
  ( -- * The monad
    RemoteDOM (..)
  , runRemoteDOM

    -- * The recorder
  , Recorder (..)
  , newRecorder
  , ready
  , takeBatch
  , whileRendering
  , close
  , bodyHandle
  , documentHandle
  , deliver
  , liveNodes
  , liveListeners

    -- * Ids
  , documentId
  , bodyId
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fork (MonadFork (..), MonadKill (..))
import Control.Monad.Parallel (MonadParallel (..))
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foreign (foreignToInt)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Scientific (floatingOrInteger)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Halogen.JSBits (JSVal, toJSInt)
import Halogen.Remote.Protocol (EventMessage (..), ListenerId, NodeId, Op (..))
import Halogen.VDom.DOM.Monad
  ( DomNodeKind (..)
  , MonadAttributes (..)
  , MonadBrowserDOM (..)
  , MonadDOM (..)
  , PropScalar (..)
  , StorageKind
  , propScalar
  )
import Halogen.VDom.Types (ElemName (..), Namespace (..))
import Protolude hiding (Handler, handle, join)
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Async qualified as Async
import UnliftIO.Exception qualified as Exception
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Web.DOM.Internal.Types (Document (..), Element (..), HTMLDocument (..), HTMLElement (..), Node (..), Window (..))
import Web.DOM.ParentNode (QuerySelector (..))
import Web.Event.Event (EventType (..))
import Web.Event.Internal.Types (Event, EventTarget (..))
import Web.HTML.Common (AttrName (..), PropName (..))
import Web.HTML.HTMLDocument.ReadyState (ReadyState (Complete))

-- | The monad a component sent over a wire runs in: everything the driver
-- asks of a component monad, over 'IO', with the session's recorder to
-- hand. A component should not name it (only an entry point does, as only
-- an entry point names 'Halogen.VDom.DOM.Monad.BrowserDOM'), so that the
-- same component can be mounted in a page or sent over a wire.
newtype RemoteDOM a = RemoteDOM (ReaderT Recorder IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

runRemoteDOM :: Recorder -> RemoteDOM a -> IO a
runRemoteDOM recorder (RemoteDOM program) = runReaderT program recorder

recorderHere :: RemoteDOM Recorder
recorderHere = RemoteDOM ask

instance PrimMonad RemoteDOM where
  type PrimState RemoteDOM = RealWorld
  primitive = RemoteDOM . primitive

instance MonadFork RemoteDOM where
  type Fork RemoteDOM = Async.Async
  fork program = withRunInIO $ \run -> Async.async (run program)
  join = liftIO . Async.wait

instance MonadKill RemoteDOM where
  kill e forked = liftIO (Async.cancelWith forked e)

instance MonadParallel RemoteDOM where
  type Parallel RemoteDOM = Async.Concurrently RemoteDOM
  parallel = Async.Concurrently
  sequential = Async.runConcurrently

instance MonadUUID RemoteDOM where
  generateV4 = liftIO generateV4

--------------------------------------------------------------------------------
-- The mirror
--------------------------------------------------------------------------------

data Kind
  = ElementNode (Maybe Namespace) ElemName
  | TextNode
  | DocumentNode

-- | One node of the page, as far as the program can know it: what it has
-- been told to be, and the values of its form controls as the last event
-- from the page reported them.
data RemoteNode = RemoteNode
  { ident :: NodeId
  , handle :: JSVal
  -- ^ The value every handle for this node holds, made once.
  , kind :: Kind
  , content :: IORef Text
  , kids :: IORef [RemoteNode]
  , parent :: IORef (Maybe RemoteNode)
  , attrs :: IORef (Map (Maybe Text, Text) Text)
  , props :: IORef (Map Text PropScalar)
  , listening :: IORef [(Text, ListenerId)]
  }

instance Eq RemoteNode where
  a == b = a.ident == b.ident

-- | A listener, before and after it listens: what it runs, under a number.
data RemoteListener = RemoteListener
  { ident :: ListenerId
  , fire :: Event -> RemoteDOM ()
  }

-- | A listener the page is listening for, and on how many nodes.
data Attached = Attached
  { fire :: Event -> RemoteDOM ()
  , count :: Int
  }

-- | One session's side of the wire: the mirror, the listeners, and the
-- operations not yet sent.
data Recorder = Recorder
  { nodes :: IORef (IntMap RemoteNode)
  , nextNode :: IORef NodeId
  , nextListener :: IORef ListenerId
  , listeners :: IORef (IntMap Attached)
  , pending :: STM.TVar (Seq Op)
  , detached :: STM.TVar (IntMap RemoteNode)
  -- ^ The nodes taken out of their parents since the last batch was taken:
  -- those still without one when the next is are forgotten.
  , rendering :: STM.TVar Int
  -- ^ How many renders are under way: a batch is taken only between them.
  , open :: STM.TVar Bool
  -- ^ Recording; 'False' once the session is over, when an operation has
  -- no one to go to.
  , storage :: IORef (Map StorageKind (Map Text Text))
  , document :: RemoteNode
  , body :: RemoteNode
  }

documentId, bodyId :: NodeId
documentId = 0
bodyId = 1

-- | A recorder with a document holding a body, which the client has as the
-- window and the container it attached to, and nothing pending.
newRecorder :: IO Recorder
newRecorder = do
  nodes <- newIORef IntMap.empty
  root <- blank documentId DocumentNode
  body <- blank bodyId (ElementNode Nothing (ElemName "body"))
  writeIORef root.kids [body]
  writeIORef body.parent (Just root)
  writeIORef nodes (IntMap.fromList [(documentId, root), (bodyId, body)])
  Recorder nodes
    <$> newIORef (bodyId + 1)
    <*> newIORef 1
    <*> newIORef IntMap.empty
    <*> STM.newTVarIO Seq.empty
    <*> STM.newTVarIO IntMap.empty
    <*> STM.newTVarIO 0
    <*> STM.newTVarIO True
    <*> newIORef Map.empty
    <*> pure root
    <*> pure body

blank :: NodeId -> Kind -> IO RemoteNode
blank ident kind =
  RemoteNode ident (toJSInt ident) kind
    <$> newIORef ""
    <*> newIORef []
    <*> newIORef Nothing
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef []

-- | The body, as the driver mounts into it.
bodyHandle :: Recorder -> HTMLElement
bodyHandle recorder = HTMLElement recorder.body.handle

-- | The document, which the driver renders with.
documentHandle :: Recorder -> Document
documentHandle recorder = Document recorder.document.handle

-- | Wait until there is something to send and no render is under way:
-- 'False' at once when the recorder is closed.
ready :: Recorder -> STM.STM Bool
ready recorder = do
  open <- STM.readTVar recorder.open
  if not open
    then pure False
    else do
      ops <- STM.readTVar recorder.pending
      depth <- STM.readTVar recorder.rendering
      STM.check (not (Seq.null ops) && depth == 0)
      pure True

-- | Take what is pending as a batch, once no render is under way (waiting
-- for the one there is to end), and forget the nodes it leaves without a
-- parent: 'Nothing' once the recorder is closed. What the batch says about
-- the nodes forgotten, other than their removal and the insertions that
-- moved nodes about, is left out (see the module's header).
takeBatch :: Recorder -> IO (Maybe [Op])
takeBatch recorder = do
  taken <- STM.atomically $ do
    open <- STM.readTVar recorder.open
    if not open
      then pure Nothing
      else do
        STM.readTVar recorder.rendering >>= STM.check . (== 0)
        ops <- STM.swapTVar recorder.pending Seq.empty
        out <- STM.swapTVar recorder.detached IntMap.empty
        pure (Just (ops, out))
  for taken $ \(ops, out) -> do
    gone <- fmap IntSet.unions . for (IntMap.elems out) $ \node ->
      readIORef node.parent >>= \case
        Nothing -> forget recorder node
        Just _ -> pure IntSet.empty
    pure (filter (not . within gone) (toList ops))
  where
    -- An operation whose effect stays inside what is forgotten. A removal
    -- from a forgotten node is one only when the child is forgotten too: a
    -- child that lives on is put somewhere else by an insertion the batch
    -- keeps, and the client takes it from where it was then.
    within gone = \case
      SetText n _ -> n `IntSet.member` gone
      Remove p c -> p `IntSet.member` gone && c `IntSet.member` gone
      SetAttribute n _ _ _ -> n `IntSet.member` gone
      RemoveAttribute n _ _ -> n `IntSet.member` gone
      SetProperty n _ _ -> n `IntSet.member` gone
      RemoveProperty n _ -> n `IntSet.member` gone
      Listen n _ _ -> n `IntSet.member` gone
      Unlisten n _ _ -> n `IntSet.member` gone
      _ -> False

-- | Run a render, counted as under way while it runs (and fails), so that no
-- batch is taken in the middle of it.
whileRendering :: RemoteDOM a -> RemoteDOM a
whileRendering program = do
  recorder <- recorderHere
  let by n = liftIO (STM.atomically (STM.modifyTVar' recorder.rendering (+ n)))
  Exception.bracket_ (by 1) (by (-1)) program

-- | Stop recording, and forget the mirror and the listeners.
close :: Recorder -> IO ()
close recorder = do
  STM.atomically $ do
    STM.writeTVar recorder.open False
    STM.writeTVar recorder.pending Seq.empty
    STM.writeTVar recorder.detached IntMap.empty
  writeIORef recorder.listeners IntMap.empty
  writeIORef recorder.nodes (IntMap.fromList [(documentId, recorder.document), (bodyId, recorder.body)])

liveNodes :: Recorder -> IO Int
liveNodes recorder = IntMap.size <$> readIORef recorder.nodes

liveListeners :: Recorder -> IO Int
liveListeners recorder = IntMap.size <$> readIORef recorder.listeners

record :: Op -> RemoteDOM ()
record op = do
  recorder <- recorderHere
  liftIO $ STM.atomically $ do
    open <- STM.readTVar recorder.open
    when open $ STM.modifyTVar' recorder.pending (Seq.|> op)

newNode :: Kind -> RemoteDOM RemoteNode
newNode kind = do
  recorder <- recorderHere
  liftIO $ do
    ident <- atomicModifyIORef' recorder.nextNode (\n -> (n + 1, n))
    node <- blank ident kind
    atomicModifyIORef' recorder.nodes (\m -> (IntMap.insert ident node m, ()))
    pure node

-- | The node a handle holds, if it is still there.
nodeOf :: JSVal -> RemoteDOM (Maybe RemoteNode)
nodeOf value = do
  recorder <- recorderHere
  IntMap.lookup (foreignToInt value) <$> readIORef recorder.nodes

-- | Act on the node a handle holds; on a forgotten one, do nothing.
withNode :: JSVal -> (RemoteNode -> RemoteDOM ()) -> RemoteDOM ()
withNode value act = nodeOf value >>= traverse_ act

-- | Take a node out of its parent, in the mirror only: the client's
-- insertion does the same by itself.
detach :: RemoteNode -> IO ()
detach node =
  readIORef node.parent >>= \case
    Nothing -> pass
    Just parent -> do
      modifyIORef' parent.kids (filter (/= node))
      writeIORef node.parent Nothing

-- | Put a child before a reference node of a parent (at the end for
-- 'Nothing', or when the reference is not the parent's), and say so.
insertAt :: RemoteNode -> Maybe RemoteNode -> RemoteNode -> RemoteDOM ()
insertAt child reference parent
  | child == parent = pass
  | otherwise = do
      index <- liftIO $ do
        detach child
        siblings <- readIORef parent.kids
        let (before, after) = case reference of
              Just ref | ref `elem` siblings -> break (== ref) siblings
              _ -> (siblings, [])
        writeIORef parent.kids (before <> [child] <> after)
        writeIORef child.parent (Just parent)
        pure (length before)
      record (Insert parent.ident index child.ident)

-- | Take a child out of its parent, say so, and note it: unless something
-- puts it back before the batch is taken, it is forgotten then.
removeFrom :: RemoteNode -> RemoteNode -> RemoteDOM ()
removeFrom child parent = do
  current <- readIORef child.parent
  when (current == Just parent) $ do
    recorder <- recorderHere
    liftIO $ do
      detach child
      STM.atomically (STM.modifyTVar' recorder.detached (IntMap.insert child.ident child))
    record (Remove parent.ident child.ident)

-- | Forget a node and its subtree, and let go of their listeners: the
-- numbers of the nodes forgotten. A node forgotten already is not again.
forget :: Recorder -> RemoteNode -> IO IntSet.IntSet
forget recorder node = do
  known <- atomicModifyIORef' recorder.nodes (\m -> (IntMap.delete node.ident m, IntMap.member node.ident m))
  if not known
    then pure IntSet.empty
    else do
      attached <- readIORef node.listening
      for_ attached $ \(_, listener) -> atomicModifyIORef' recorder.listeners (\m -> (IntMap.update release listener m, ()))
      below <- traverse (forget recorder) =<< readIORef node.kids
      pure (IntSet.insert node.ident (IntSet.unions below))
  where
    release a = if a.count <= 1 then Nothing else Just a {count = a.count - 1}

siblingOf :: ([RemoteNode] -> [RemoteNode] -> Maybe RemoteNode) -> RemoteNode -> IO (Maybe RemoteNode)
siblingOf pick node =
  readIORef node.parent >>= \case
    Nothing -> pure Nothing
    Just parent -> do
      siblings <- readIORef parent.kids
      let (before, after) = break (== node) siblings
      pure (pick before (drop 1 after))

asNode :: RemoteNode -> Node
asNode node = Node node.handle

nodeValue :: Node -> JSVal
nodeValue (Node value) = value

elementValue :: Element -> JSVal
elementValue (Element value) = value

--------------------------------------------------------------------------------
-- The instances
--------------------------------------------------------------------------------

instance MonadDOM RemoteDOM where
  type DomNode RemoteDOM = Node
  type DomElement RemoteDOM = Element
  type DomDocument RemoteDOM = Document
  type DomEventListener RemoteDOM = RemoteListener
  type DomEventTarget RemoteDOM = EventTarget

  mkEventListener fire = do
    recorder <- recorderHere
    ident <- liftIO $ atomicModifyIORef' recorder.nextListener (\n -> (n + 1, n))
    pure RemoteListener {ident, fire}

  elementToNode (Element value) = pure (Node value)
  elementToEventTarget (Element value) = pure (EventTarget value)
  sameNode a b = pure (foreignToInt (nodeValue a) == foreignToInt (nodeValue b))

  createTextNode text _ = do
    node <- newNode TextNode
    writeIORef node.content text
    record (CreateText node.ident text)
    pure (asNode node)

  -- As in the DOM: an element's text content replaces its children with one
  -- text node (none for empty text), sent as the removals and the insertion
  -- that are, so that the client and the mirror agree on the new node's
  -- number.
  setTextContent text value = withNode (nodeValue value) $ \node -> case node.kind of
    TextNode -> do
      writeIORef node.content text
      record (SetText node.ident text)
    DocumentNode -> pass
    ElementNode {} -> do
      children <- readIORef node.kids
      for_ children (`removeFrom` node)
      unless (Text.null text) $ do
        child <- newNode TextNode
        writeIORef child.content text
        record (CreateText child.ident text)
        insertAt child Nothing node

  createElement namespace name@(ElemName tag) _ = do
    node <- newNode (ElementNode namespace name)
    record (Create node.ident ((\(Namespace ns) -> ns) <$> namespace) tag)
    pure (Element node.handle)

  -- The browser backends' guard: an insertion that would not move the child
  -- does nothing.
  insertChildIx index child parent =
    withNode (nodeValue child) $ \c -> withNode (nodeValue parent) $ \p -> do
      occupant <- (`atMay` index) <$> readIORef p.kids
      when (occupant /= Just c) $ insertAt c occupant p

  removeChild child parent =
    withNode (nodeValue child) $ \c -> withNode (nodeValue parent) $ \p -> removeFrom c p

  parentNode value = nodeOf (nodeValue value) >>= maybe (pure Nothing) (fmap (fmap asNode) . readIORef . (.parent))
  nextSibling value = nodeOf (nodeValue value) >>= maybe (pure Nothing) (fmap (fmap asNode) . liftIO . siblingOf (\_ after -> head after))
  firstChild value = nodeOf (nodeValue value) >>= maybe (pure Nothing) (fmap (fmap asNode . head) . readIORef . (.kids))
  nodeKind value =
    nodeOf (nodeValue value) >>= \case
      Nothing -> pure OtherKind
      Just node -> case node.kind of
        ElementNode ns name -> pure (ElementKind ns name)
        TextNode -> TextKind <$> readIORef node.content
        DocumentNode -> pure OtherKind
  nodeToElement value =
    nodeOf (nodeValue value) <&> \case
      Just RemoteNode {kind = ElementNode {}, handle} -> Just (Element handle)
      _ -> Nothing

  addEventListener (EventType ty) listener (EventTarget target) = withNode target $ \node -> do
    recorder <- recorderHere
    liftIO $ do
      modifyIORef' node.listening ((ty, listener.ident) :)
      let attached = Attached {fire = listener.fire, count = 1}
          more _ old = old {count = old.count + 1}
      atomicModifyIORef' recorder.listeners (\m -> (IntMap.insertWith more listener.ident attached m, ()))
    record (Listen node.ident ty listener.ident)

  removeEventListener (EventType ty) listener (EventTarget target) = withNode target $ \node -> do
    recorder <- recorderHere
    present <- elem (ty, listener.ident) <$> readIORef node.listening
    when present $ do
      liftIO $ do
        modifyIORef' node.listening (deleteOnce (ty, listener.ident))
        let release a = if a.count <= 1 then Nothing else Just a {count = a.count - 1}
        atomicModifyIORef' recorder.listeners (\m -> (IntMap.update release listener.ident m, ()))
      record (Unlisten node.ident ty listener.ident)
    where
      deleteOnce x = \case
        [] -> []
        y : ys
          | x == y -> ys
          | otherwise -> y : deleteOnce x ys

instance MonadAttributes RemoteDOM where
  setAttribute namespace (AttrName name) value el = withNode (elementValue el) $ \node -> do
    let ns = (\(Namespace n) -> n) <$> namespace
    modifyIORef' node.attrs (Map.insert (ns, name) value)
    record (SetAttribute node.ident ns name value)

  removeAttribute namespace (AttrName name) el = withNode (elementValue el) $ \node -> do
    let ns = (\(Namespace n) -> n) <$> namespace
    present <- Map.member (ns, name) <$> readIORef node.attrs
    when present $ do
      modifyIORef' node.attrs (Map.delete (ns, name))
      record (RemoveAttribute node.ident ns name)

  hasAttribute namespace (AttrName name) el =
    nodeOf (elementValue el) >>= \case
      Nothing -> pure False
      Just node -> Map.member ((\(Namespace n) -> n) <$> namespace, name) <$> readIORef node.attrs

  setProperty (PropName name) value el = withNode (elementValue el) $ \node -> do
    let scalar = propScalar value
    atomicModifyIORef' node.props (\m -> (Map.insert name scalar m, ()))
    record (SetProperty node.ident name scalar)

  propertyEquals (PropName name) value el =
    nodeOf (elementValue el) >>= \case
      Nothing -> pure False
      Just node -> (== Just (propScalar value)) . Map.lookup name <$> readIORef node.props

  removeProperty (PropName name) el = withNode (elementValue el) $ \node -> do
    atomicModifyIORef' node.props (\m -> (Map.delete name m, ()))
    record (RemoveProperty node.ident name)

-- | The session's document stands for the window too, as the in-memory
-- DOM's does: a listener on the window is a listener on node 0, which the
-- client has as its window.
instance MonadBrowserDOM RemoteDOM where
  insertBefore inserted sibling parent =
    withNode (nodeValue inserted) $ \child -> withNode (nodeValue sibling) $ \ref -> withNode (nodeValue parent) $ \p -> do
      already <- liftIO (siblingOf (\before _ -> lastMay before) ref)
      when (already /= Just child) $ insertAt child (Just ref) p

  appendChild inserted parent =
    withNode (nodeValue inserted) $ \child -> withNode (nodeValue parent) $ \p -> do
      end <- lastMay <$> readIORef p.kids
      when (end /= Just child) $ insertAt child Nothing p

  replaceChild inserted old parent =
    withNode (nodeValue inserted) $ \new -> withNode (nodeValue old) $ \o -> withNode (nodeValue parent) $ \p ->
      when (new /= o) $ do
        insertAt new (Just o) p
        removeFrom o p

  window = Window . (.document.handle) <$> recorderHere
  windowToEventTarget (Window value) = pure (EventTarget value)
  document (Window value) = pure (HTMLDocument value)
  documentToNode (HTMLDocument value) = pure (Node value)

  querySelector (QuerySelector selector) value =
    nodeOf (nodeValue value) >>= \case
      Nothing -> pure Nothing
      Just root -> liftIO $ fmap (\n -> Element n.handle) <$> query selector root

  readyState _ = pure Complete

  readStorageItem kind key = do
    recorder <- recorderHere
    Map.lookup key . Map.findWithDefault Map.empty kind <$> readIORef recorder.storage
  writeStorageItem kind key text = do
    recorder <- recorderHere
    atomicModifyIORef' recorder.storage (\m -> (Map.insertWith Map.union kind (Map.singleton key text) m, ()))
  removeStorageItem kind key = do
    recorder <- recorderHere
    atomicModifyIORef' recorder.storage (\m -> (Map.adjust (Map.delete key) kind m, ()))
  storageItemKeys kind = do
    recorder <- recorderHere
    Map.keys . Map.findWithDefault Map.empty kind <$> readIORef recorder.storage

-- | Depth first, the first element below the root a selector matches: a tag
-- name, @#id@ or @.class@, the selectors the library itself uses, as the
-- in-memory DOM has them. Anything else finds nothing.
query :: Text -> RemoteNode -> IO (Maybe RemoteNode)
query selector = below
  where
    below node = readIORef node.kids >>= firstOf
    firstOf = \case
      [] -> pure Nothing
      c : cs -> do
        hit <- matches c
        if hit then pure (Just c) else below c >>= maybe (firstOf cs) (pure . Just)
    matches node = case node.kind of
      ElementNode _ (ElemName name) -> case Text.uncons selector of
        Just ('#', wanted) -> (== Just wanted) <$> named "id" node
        Just ('.', wanted) -> maybe False (elem wanted . Text.words) <$> named "className" node
        _ -> pure (name == selector)
      _ -> pure False
    -- An id or a class reaches the mirror as an attribute or as a property,
    -- depending on how the HTML was written.
    named property node = do
      attributes <- readIORef node.attrs
      let attribute = if property == "className" then "class" else property
      case Map.lookup (Nothing, attribute) attributes of
        Just v -> pure (Just v)
        Nothing ->
          Map.lookup property <$> readIORef node.props <&> \case
            Just (ScalarText t) -> Just t
            _ -> Nothing

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

-- | Run the listener an event is for, as the page's own callback would: on
-- the caller's thread, which hands the component's action to its tree's
-- loop. The mirror of the node it was heard on first takes the values the
-- page reports its form controls hold, since a reader typing changes them
-- without the program's doing, and the reconciler compares a @value@ it
-- renders with the element's. An event for a listener the program no longer
-- has (one whose removal crossed the wire after the event left) is dropped.
deliver :: Recorder -> EventMessage -> Event -> IO ()
deliver recorder message event = do
  attached <- IntMap.lookup message.listener <$> readIORef recorder.listeners
  for_ attached $ \a -> do
    node <- IntMap.lookup message.node <$> readIORef recorder.nodes
    for_ node (reported message.event)
    runRemoteDOM recorder (a.fire event)

-- | The values the event says its current target holds.
reported :: Aeson.Value -> RemoteNode -> IO ()
reported event node = case event of
  Aeson.Object fields
    | Just (Aeson.Object target) <- KeyMap.lookup "currentTarget" fields ->
        for_ [("value", text), ("checked", boolean), ("selectedIndex", int)] $ \(name, read') ->
          for_ (KeyMap.lookup (Key.fromText name) target >>= read') $ \v ->
            atomicModifyIORef' node.props (\m -> (Map.insert name v m, ()))
  _ -> pass
  where
    text = \case
      Aeson.String t -> Just (ScalarText t)
      _ -> Nothing
    boolean = \case
      Aeson.Bool b -> Just (ScalarBool b)
      _ -> Nothing
    int = \case
      Aeson.Number n -> either (const Nothing) (Just . ScalarInt) (floatingOrInteger n :: Either Double Integer)
      _ -> Nothing
