{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A panel over the page that shows what the program's components do, as
-- they do it: the tree of components, with their labels and ids and how
-- often each has rendered, handled an action and changed its state; a log
-- of the last 200 events, which can be narrowed to one component or one
-- kind of event; the element a component rendered outlined on the page when
-- its row is hovered; and, for a component whose state the panel can show,
-- each state it went through, with a button that puts it back.
--
-- > main = runBrowserDOM $ do
-- >   DevTools.showing @Counter.State
-- >   tools <- DevTools.open
-- >   body <- HA.awaitBody
-- >   void $ runUI Counter.component () body
--
-- It reads "Halogen.Trace", which every tree of the program reports to once
-- someone listens: open the panel first, before mounting what it is to look
-- at, so that it can outline and restore those components too (it lists and
-- counts the ones mounted before it all the same, from their next event on).
-- The panel is a Halogen component itself, in a tree of its own, and leaves
-- its own events out.
--
-- It updates at most ten times a second, gathering the events in between,
-- so that a component that renders on every frame does not make the panel
-- render on every frame too.
module Halogen.DevTools
  ( -- * Opening the panel
    open
  , openIn
  , panel
  , Query (..)

    -- * States
  , showing
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (get, gets, modify)
import Data.Coerce (coerce)
import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Core (AttrName (..), ClassName (..), ElemName (..))
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.HTML.Properties.ARIA qualified as HPA
import Halogen.IO.Util (awaitBody)
import Halogen.JSBits (JSVal, Safety (..), browserJS)
import Halogen.Subscription qualified as HS
import Halogen.Trace qualified as Trace
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, appendChild, createElement, document, elementToNode, removeChild, setAttribute, window)
import Halogen.VDom.Driver (runUI)
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep (..), typeRep)
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (Element (..), HTMLElement (..), Node (..), toDocument)

$( browserJS
     ["jsbits/devtools.js"]
     [ ("js_rect_left", "halogen_devtools_rect_left", Unsafe, [t|JSVal -> IO Double|])
     , ("js_rect_top", "halogen_devtools_rect_top", Unsafe, [t|JSVal -> IO Double|])
     , ("js_rect_width", "halogen_devtools_rect_width", Unsafe, [t|JSVal -> IO Double|])
     , ("js_rect_height", "halogen_devtools_rect_height", Unsafe, [t|JSVal -> IO Double|])
     ]
 )

--------------------------------------------------------------------------------
-- Opening the panel
--------------------------------------------------------------------------------

-- | What the panel can be asked: to forget what it has seen so far (its log
-- and its counts) and start again from now.
data Query a = Clear a

-- | Open the panel over the page, docked at the bottom right of the window,
-- in an element of its own at the end of the body. It listens to the trace
-- by the time this returns. The socket's @dispose@ closes it, takes the
-- element away and stops listening; under @npm run dev@, pass it to
-- 'Halogen.IO.Util.keepForNextRun'.
open :: (MonadBrowserDOM m, MonadUnliftIO m) => m (H.HalogenSocket Query Void m)
open = do
  body <- awaitBody
  doc <- toDocument <$> (document =<< window)
  host <- createElement Nothing (ElemName "div") doc
  setAttribute Nothing (AttrName "style") docked host
  hostNode <- elementToNode host
  bodyNode <- elementToNode (coerce body :: Element)
  appendChild hostNode bodyNode
  H.HalogenSocket {H.query = ask, H.messages, H.dispose} <- runUI panel () (coerce host :: HTMLElement)
  pure H.HalogenSocket {H.query = ask, H.messages, H.dispose = dispose >> removeChild hostNode bodyNode}
  where
    docked = "position: fixed; right: 0.5rem; bottom: 0.5rem; z-index: 2147483000; width: min(34rem, calc(100vw - 1rem)); max-height: 60vh; overflow: auto; box-shadow: 0 0.25rem 1rem rgb(0 0 0 / 30%);"

-- | The panel inside this element, in the flow of the page: beside an
-- application rather than over it, as the tutorial puts it under a lesson's
-- result.
openIn :: (MonadBrowserDOM m, MonadUnliftIO m) => HTMLElement -> m (H.HalogenSocket Query Void m)
openIn = runUI panel ()

-- | The panel as a component, for a program that mounts it itself, as a
-- test does. It starts listening as it is initialized, and stops as it is
-- finalized.
panel :: (MonadIO m) => H.Component Query () Void m
panel =
  H.named "DevTools" $
    H.component
      initialState
      render
      [ H.onInitialize initialize
      , H.onFinalize (gets (.stop) >>= H.liftEffect . liftIO . sequence_)
      , H.onAction handleAction
      , H.onQuery (\(Clear a) -> Just a <$ handleAction ClearAll)
      ]

--------------------------------------------------------------------------------
-- States
--------------------------------------------------------------------------------

-- | Let the panel show the states of this type, and put them back: a
-- component whose state is of a type no 'showing' has named still has its
-- state changes counted and listed, under the type's name. The panel cannot
-- show what it has no 'Show' for, and 'Show' is not asked of every state, so
-- this is opt-in, per type:
--
-- > DevTools.showing @Counter.State
showing :: forall s m. (Typeable s, Show s, MonadIO m) => m ()
showing = liftIO $ atomicModifyIORef' showers $ \known ->
  (Map.insert (SomeTypeRep (typeRep @s)) (fmap (T.pack . show) . fromDynamic @s) known, ())

showers :: IORef (Map SomeTypeRep (Dynamic -> Maybe Text))
showers = unsafePerformIO (newIORef mempty)
{-# NOINLINE showers #-}

--------------------------------------------------------------------------------
-- The panel
--------------------------------------------------------------------------------

-- | Where the trace's events wait for the panel: the listener only keeps
-- them, and, once per batch, has the panel take them a tenth of a second
-- later.
data Inbox = Inbox
  { events :: IORef (Seq Trace.Event)
  , scheduled :: IORef Bool
  , arrived :: HS.Subscribe IO ()
  }

data State = State
  { inbox :: Inbox
  , stop :: Maybe (IO ())
  -- ^ How to stop listening, once listening.
  , opened :: Double
  -- ^ When the panel started counting, which the log's times are from.
  , components :: IntMap Row
  , logged :: Seq Entry
  -- ^ Oldest first.
  , taken :: Int
  , selected :: Maybe Int
  , kind :: Maybe Kind
  , outline :: Maybe Rect
  , hidden :: Bool
  , status :: Maybe Text
  }

-- | A component, as the panel counts it.
data Row = Row
  { tag :: Trace.Tag
  , renders :: Int
  , actions :: Int
  , changes :: Int
  , lastRender :: Maybe Double
  }

-- | An event of the log, with its state as the panel shows it, if it is a
-- state change of a type 'showing' named.
data Entry = Entry
  { number :: Int
  -- ^ How many events the panel had taken before this one: the entry's key
  -- in the log, so that a new event at its top moves the rows below rather
  -- than rewrites them.
  , event :: Trace.Event
  , shown :: Maybe Text
  }

-- | The kinds of event the log can be narrowed to.
data Kind = Lifecycle | Renders | Actions | States | Messages | Effects
  deriving stock (Eq, Enum, Bounded)

data Rect = Rect {left :: Double, top :: Double, width :: Double, height :: Double}

data Action
  = Refresh
  | Select Int
  | Narrow (Maybe Kind)
  | Hover Int
  | Unhover
  | Restore Int Dynamic
  | ClearAll
  | ToggleHidden

-- | How many events the log keeps, and how many the inbox keeps for a panel
-- that has not taken them yet.
logLength, inboxLength :: Int
logLength = 200
inboxLength = 5000

initialState :: (MonadIO m) => () -> m State
initialState () = liftIO $ do
  events <- newIORef Seq.empty
  scheduled <- newIORef False
  arrived <- HS.create
  opened <- (* 1000) <$> getMonotonicTime
  pure
    State
      { inbox = Inbox {events, scheduled, arrived}
      , stop = Nothing
      , opened
      , components = mempty
      , logged = Seq.empty
      , taken = 0
      , selected = Nothing
      , kind = Nothing
      , outline = Nothing
      , hidden = False
      , status = Nothing
      }

-- | Listen, at once (in a synchronous effect, not a @liftIO@ that would let
-- 'open' return first), and take the inbox whenever it says it has events.
initialize :: (MonadIO m) => H.HalogenM State Action Empty Void m ()
initialize = do
  inbox <- gets (.inbox)
  stop <- H.liftEffect (liftIO (Trace.subscribe (listen inbox)))
  modify $ \st -> st {stop = Just stop}
  void $ H.subscribe (Refresh <$ inbox.arrived.emitter)

-- | The trace's listener: keep the event, unless it is the panel's own, and
-- have the panel take what is kept a tenth of a second from the first event
-- of a batch.
listen :: Inbox -> Trace.Event -> IO ()
listen inbox event = unless (event.tag.stateType == Just panelType) $ do
  atomicModifyIORef' inbox.events $ \kept ->
    (Seq.drop (Seq.length kept + 1 - inboxLength) (kept |> event), ())
  already <- atomicModifyIORef' inbox.scheduled (True,)
  unless already $ void $ forkIO $ do
    threadDelay 100000
    atomicWriteIORef inbox.scheduled False
    HS.notify inbox.arrived.listener ()

-- | Every panel's state is of this type, which is how a panel knows its
-- own events, and another panel's.
panelType :: SomeTypeRep
panelType = SomeTypeRep (typeRep @State)

handleAction :: (MonadIO m) => Action -> H.HalogenM State Action Empty Void m ()
handleAction = \case
  Refresh -> do
    inbox <- gets (.inbox)
    (new, known) <- H.liftEffect . liftIO $ (,) <$> atomicModifyIORef' inbox.events (Seq.empty,) <*> readIORef showers
    modify $ \st -> foldl (absorb known) st new
  Select key -> modify $ \st -> st {selected = if st.selected == Just key then Nothing else Just key}
  Narrow k -> modify $ \st -> st {kind = k}
  Hover key -> do
    rect <- H.liftEffect . liftIO $ Trace.rootNode key >>= traverse nodeRect
    modify $ \st -> st {outline = rect}
  Unhover -> modify $ \st -> st {outline = Nothing}
  Restore key state -> do
    put' <- liftIO (Trace.restoreState key state)
    st <- get
    let who = maybe ("#" <> showT key) (named . (.tag)) (IntMap.lookup key st.components)
    modify $ \s -> s {status = Just (if put' then "Put " <> who <> "'s state back." else who <> " is gone, or has another state now: nothing was put back.")}
  ClearAll -> do
    now <- liftIO ((* 1000) <$> getMonotonicTime)
    modify $ \st ->
      st
        { opened = now
        , logged = Seq.empty
        , components = (\row -> row {renders = 0, actions = 0, changes = 0, lastRender = Nothing}) <$> st.components
        , status = Nothing
        }
  ToggleHidden -> modify $ \st -> st {hidden = not st.hidden, outline = Nothing}

-- | One event into the counts and the log.
absorb :: Map SomeTypeRep (Dynamic -> Maybe Text) -> State -> Trace.Event -> State
absorb known st event =
  st
    { components = case event.what of
        Trace.Finalized -> IntMap.delete key st.components
        what -> IntMap.insert key (count what (IntMap.findWithDefault fresh key st.components)) st.components
    , logged = Seq.drop (Seq.length st.logged + 1 - logLength) (st.logged |> Entry {number = st.taken, event, shown})
    , taken = st.taken + 1
    }
  where
    key = event.tag.id
    fresh = Row {tag = event.tag, renders = 0, actions = 0, changes = 0, lastRender = Nothing}
    count what row = case what of
      Trace.Rendered took -> row {renders = row.renders + 1, lastRender = Just took}
      Trace.Action -> row {actions = row.actions + 1}
      Trace.StateChanged _ -> row {changes = row.changes + 1}
      _ -> row
    shown = case event.what of
      Trace.StateChanged (Just state) -> Map.lookup (dynTypeRep state) known >>= ($ state)
      _ -> Nothing

kindOf :: Trace.What -> Kind
kindOf = \case
  Trace.Mounted -> Lifecycle
  Trace.Initialized -> Lifecycle
  Trace.Finalized -> Lifecycle
  Trace.Rendered _ -> Renders
  Trace.Action -> Actions
  Trace.StateChanged _ -> States
  Trace.Queried -> Messages
  Trace.Raised -> Messages
  _ -> Effects

kindName :: Kind -> Text
kindName = \case
  Lifecycle -> "lifecycle"
  Renders -> "renders"
  Actions -> "actions"
  States -> "states"
  Messages -> "queries and outputs"
  Effects -> "subscriptions and forks"

-- | Where a node is in the window, by the browser's @getBoundingClientRect@.
nodeRect :: Node -> IO Rect
nodeRect node = do
  let value = coerce node :: JSVal
  Rect <$> js_rect_left value <*> js_rect_top value <*> js_rect_width value <*> js_rect_height value

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

render :: State -> H.ComponentHTML Action Empty m
render st =
  HH.div [HP.class_ (ClassName "halogen-devtools")] $
    [ HH.element (ElemName "style") [] [HH.text styles]
    , HH.div
        [HP.class_ (ClassName "halogen-devtools-bar")]
        [ HH.span [HP.class_ (ClassName "halogen-devtools-title")] [HH.text "Halogen DevTools"]
        , HH.button [HP.class_ (ClassName "halogen-devtools-clear"), HE.onClick (const ClearAll)] [HH.text "Clear"]
        , HH.button [HP.class_ (ClassName "halogen-devtools-toggle"), HE.onClick (const ToggleHidden)] [HH.text (if st.hidden then "Show" else "Hide")]
        ]
    ]
      <> if st.hidden
        then []
        else
          [ HH.ul [HP.class_ (ClassName "halogen-devtools-tree")] (map (componentRow st) (inTreeOrder st.components))
          , HH.div [HP.class_ (ClassName "halogen-devtools-kinds")] (kindButton st Nothing : map (kindButton st . Just) [minBound .. maxBound])
          ]
            <> [HH.p [HP.class_ (ClassName "halogen-devtools-status")] [HH.text s] | Just s <- [st.status]]
            <> [ HH.withKeys
                   HH.ol
                   [HP.class_ (ClassName "halogen-devtools-log")]
                   (map (\entry -> (showT entry.number, entryRow st entry)) (reverse (filter (wanted st) (toList st.logged))))
               ]
            <> [outlineBox r | Just r <- [st.outline]]

componentRow :: State -> (Int, Row) -> H.ComponentHTML Action Empty m
componentRow st (depth, row) =
  HH.li
    [ HP.classes (ClassName "halogen-devtools-component" : [ClassName "halogen-devtools-selected" | st.selected == Just key])
    , HP.attr (AttrName "data-id") (showT key)
    , HP.attr (AttrName "data-label") row.tag.label
    , HP.styleText ("padding-left: " <> showT (depth + 1) <> "ch")
    , HE.onClick (const (Select key))
    , HE.onMouseEnter (const (Hover key))
    , HE.onMouseLeave (const Unhover)
    ]
    [ HH.span [HP.class_ (ClassName "halogen-devtools-name")] [HH.text (named row.tag)]
    , number "halogen-devtools-renders" row.renders "renders"
    , number "halogen-devtools-actions" row.actions "actions"
    , number "halogen-devtools-changes" row.changes "changes"
    , HH.span [HP.class_ (ClassName "halogen-devtools-last")] [HH.text (maybe "" (\took -> "last " <> tenths took <> " ms") row.lastRender)]
    ]
  where
    key = row.tag.id
    number cls n what = HH.span [HP.class_ (ClassName cls), HP.title (showT n <> " " <> what)] [HH.text (showT n)]

kindButton :: State -> Maybe Kind -> H.ComponentHTML Action Empty m
kindButton st k =
  HH.button
    [ HP.class_ (ClassName "halogen-devtools-kind")
    , HPA.pressed (if st.kind == k then "true" else "false")
    , HE.onClick (const (Narrow k))
    ]
    [HH.text (maybe "all" kindName k)]

entryRow :: State -> Entry -> H.ComponentHTML Action Empty m
entryRow st Entry {event, shown} =
  HH.li
    [HP.class_ (ClassName "halogen-devtools-event"), HP.attr (AttrName "data-kind") (kindName (kindOf event.what))]
    $ [ HH.span [HP.class_ (ClassName "halogen-devtools-time")] [HH.text (tenths (event.at - st.opened) <> " ms")]
      , HH.span [HP.class_ (ClassName "halogen-devtools-who")] [HH.text (named event.tag)]
      , HH.span [HP.class_ (ClassName "halogen-devtools-what")] [HH.text (Trace.summary event.what)]
      ]
      <> case (event.what, shown) of
        (Trace.StateChanged (Just state), Just text) ->
          [ HH.code [HP.class_ (ClassName "halogen-devtools-state")] [HH.text text]
          , HH.button [HP.class_ (ClassName "halogen-devtools-restore"), HE.onClick (const (Restore event.tag.id state))] [HH.text "restore"]
          ]
        (Trace.StateChanged (Just state), Nothing) ->
          [HH.span [HP.class_ (ClassName "halogen-devtools-type")] [HH.text (T.pack (show (dynTypeRep state)))]]
        _ -> []

outlineBox :: Rect -> H.ComponentHTML Action Empty m
outlineBox r =
  HH.div
    [ HP.class_ (ClassName "halogen-devtools-outline")
    , HP.styleText ("left: " <> px r.left <> "; top: " <> px r.top <> "; width: " <> px r.width <> "; height: " <> px r.height <> ";")
    ]
    []
  where
    px v = tenths v <> "px"

-- | Whether the log shows this entry, by the component and the kind chosen.
wanted :: State -> Entry -> Bool
wanted st Entry {event} =
  maybe True (== event.tag.id) st.selected && maybe True (== kindOf event.what) st.kind

-- | The components, each followed by its children (by id, the order they
-- were made in), with how deep each is. One whose parent the panel has not
-- seen counts as a root.
inTreeOrder :: IntMap Row -> [(Int, Row)]
inTreeOrder rows = concatMap (from 0) roots
  where
    parentOf row = row.tag.parent >>= \p -> if IntMap.member p rows then Just p else Nothing
    roots = [row | row <- IntMap.elems rows, parentOf row == Nothing]
    children = IntMap.fromListWith (flip (<>)) [(p, [row]) | row <- IntMap.elems rows, Just p <- [parentOf row]]
    from depth row = (depth, row) : concatMap (from (depth + 1)) (sortOn (.tag.id) (IntMap.findWithDefault [] row.tag.id children))

named :: Trace.Tag -> Text
named tag = tag.label <> "#" <> showT tag.id

showT :: (Show a) => a -> Text
showT = T.pack . show

-- | A number to one decimal place.
tenths :: Double -> Text
tenths v = sign <> showT whole <> "." <> showT part
  where
    (whole, part) = (round (abs v * 10) :: Integer) `quotRem` 10
    sign = if v < 0 && (whole, part) /= (0, 0) then "-" else ""

-- | The panel's CSS, every rule under its class, so that it neither styles
-- the page nor is styled by much of it.
styles :: Text
styles =
  T.unlines
    [ ".halogen-devtools { font: 12px/1.4 ui-monospace, SFMono-Regular, Menlo, monospace; color: #1d1d1f; background: #fbfbfd; border: 1px solid #c7c7cc; border-radius: 6px; padding: 0.5em; box-sizing: border-box; text-align: left; }"
    , ".halogen-devtools * { box-sizing: border-box; }"
    , ".halogen-devtools button { font: inherit; padding: 0.1em 0.6em; border: 1px solid #c7c7cc; border-radius: 4px; background: #fff; color: inherit; cursor: pointer; }"
    , ".halogen-devtools button[aria-pressed=true] { background: #1d1d1f; color: #fff; }"
    , ".halogen-devtools-bar { display: flex; gap: 0.5em; align-items: center; margin-bottom: 0.5em; }"
    , ".halogen-devtools-title { font-weight: bold; margin-right: auto; }"
    , ".halogen-devtools ul, .halogen-devtools ol { list-style: none; margin: 0 0 0.5em; padding: 0; }"
    , ".halogen-devtools-component { display: flex; gap: 1ch; cursor: pointer; border-radius: 3px; }"
    , ".halogen-devtools-component:hover, .halogen-devtools-selected { background: #e8f0fe; }"
    , ".halogen-devtools-name { margin-right: auto; }"
    , ".halogen-devtools-renders::after { content: ' renders'; color: #6e6e73; }"
    , ".halogen-devtools-actions::after { content: ' actions'; color: #6e6e73; }"
    , ".halogen-devtools-changes::after { content: ' changes'; color: #6e6e73; }"
    , ".halogen-devtools-last { color: #6e6e73; min-width: 11ch; text-align: right; }"
    , ".halogen-devtools-kinds { display: flex; flex-wrap: wrap; gap: 0.3em; margin-bottom: 0.5em; }"
    , ".halogen-devtools-log { max-height: 16em; overflow: auto; border-top: 1px solid #e5e5ea; }"
    , ".halogen-devtools-event { display: flex; flex-wrap: wrap; gap: 1ch; padding: 0.1em 0; border-bottom: 1px solid #f2f2f7; }"
    , ".halogen-devtools-time { color: #6e6e73; min-width: 10ch; text-align: right; }"
    , ".halogen-devtools-state { flex-basis: 100%; white-space: pre-wrap; word-break: break-word; padding-left: 11ch; }"
    , ".halogen-devtools-type { color: #6e6e73; }"
    , ".halogen-devtools-status { margin: 0 0 0.5em; }"
    , ".halogen-devtools-outline { position: fixed; pointer-events: none; z-index: 2147483001; outline: 2px solid #0a84ff; background: rgb(10 132 255 / 15%); }"
    , "@media (prefers-color-scheme: dark) { .halogen-devtools { color: #f5f5f7; background: #1c1c1e; border-color: #3a3a3c; } .halogen-devtools button { background: #2c2c2e; border-color: #3a3a3c; } .halogen-devtools button[aria-pressed=true] { background: #f5f5f7; color: #1c1c1e; } .halogen-devtools-component:hover, .halogen-devtools-selected { background: #2c3e57; } }"
    ]
