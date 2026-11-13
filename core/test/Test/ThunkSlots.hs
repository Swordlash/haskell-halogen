{-# LANGUAGE CPP #-}

-- | Child components under a memoized node ('HH.memoized', 'HH.lazy'): a
-- render that skips the node, its input being equal, leaves the children it
-- rendered the last time where they are, alive, with their state, and
-- without a @Receive@. What happens to a child is logged in a synchronous
-- effect, so that it is in the log when the query that caused it returns.
-- Run against the in-memory DOM with the VDom driver,
-- which is what reconciles a thunk.
module Test.ThunkSlots (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "child components under a memoized node" $ pure ()

#else

import Control.Exception (ErrorCall (..), SomeException, throw, try)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, gets, modify)
import Data.Foldable (for_)
import Data.IORef
import Data.Row (Empty, type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.VDom.DOM.Monad (MemDOM, appendChild, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver qualified as VD
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)

type Log = IORef [Text]

tshow :: (Show a) => a -> Text
tshow = T.pack . show

----------------------------------------------------------------------
-- A child that counts, says what happens to it, and raises its count.

data ChildQuery a
  = Count (Int -> a)
  | Bump a
  | Reshape a

-- | The input is the child's name; the state is its name and a count. A
-- count below zero is shown in another element, so the child's root node
-- changes.
child :: Log -> H.Component ChildQuery Int Int MemDOM
child logRef =
  H.component
    (\n -> pure (n, 0 :: Int))
    (\(n, c) -> (if c < 0 then HH.strong_ else HH.em_) [HH.text (tshow n <> ":" <> tshow c)] :: H.ComponentHTML () Empty MemDOM)
    [ H.onInitialize (note "init")
    , H.onFinalize (note "fin")
    , H.onReceive (\_ -> note "receive")
    , H.onQuery $ \case
        Count k -> gets (Just . k . snd)
        Bump a -> do
          modify (fmap (+ 1))
          gets snd >>= H.raise
          pure (Just a)
        Reshape a -> modify (fmap (const (-1))) >> pure (Just a)
    ]
  where
    note what = do
      (n, _) <- get
      H.liftEffect . liftIO $ modifyIORef' logRef (<> [what <> " " <> tshow n])

----------------------------------------------------------------------
-- A parent that holds children under memoized nodes, in one of several
-- arrangements.

type Slots = ("card" .== H.Slot ChildQuery Int Int)

type Html = H.ComponentHTML Heard Slots MemDOM

newtype Heard = Heard Int

data Layout
  = -- | One memoized node of the cards named by @keys@.
    Flat
  | -- | A memoized node of the cards named by @keys@, with first in it a
    -- memoized node of those named by @inner@ (first, so that it stays
    -- where it is when the cards after it change).
    Nested
  | -- | Two memoized nodes side by side, of @keys@ and of @inner@.
    Side
  | -- | A keyed list with a memoized node of one card per key.
    KeyedList
  | -- | A keyed list with a memoized node per key whose HTML is the card
    -- itself, nothing around it.
    KeyedRoot

data State = State
  { tick :: Int
  , keys :: [Int]
  , inner :: [Int]
  , shown :: Bool
  , bad :: Bool
  }

data Query a
  = Tick a
  | SetKeys [Int] a
  | SetInner [Int] a
  | Hide a
  | Break Bool a
  | BumpCard Int a
  | ReshapeCard Int a
  | AskCard Int (Maybe Int -> a)

parent :: Log -> Layout -> [Int] -> [Int] -> H.Component Query () Void MemDOM
parent logRef layout keys0 inner0 =
  H.component
    (\_ -> pure State {tick = 0, keys = keys0, inner = inner0, shown = True, bad = False})
    render
    [ H.onAction (\(Heard n) -> H.liftEffect . liftIO $ modifyIORef' logRef (<> ["heard " <> tshow n]))
    , H.onQuery $ \case
        Tick a -> modify (\s -> s {tick = s.tick + 1}) >> pure (Just a)
        SetKeys ks a -> modify (\s -> s {keys = ks}) >> pure (Just a)
        SetInner ks a -> modify (\s -> s {inner = ks}) >> pure (Just a)
        Hide a -> modify (\s -> s {shown = False}) >> pure (Just a)
        Break b a -> modify (\s -> s {bad = b, tick = s.tick + 1}) >> pure (Just a)
        BumpCard k a -> H.query "card" k (Bump ()) >> pure (Just a)
        ReshapeCard k a -> H.query "card" k (Reshape ()) >> pure (Just a)
        AskCard k f -> Just . f <$> H.query "card" k (Count id)
    ]
  where
    -- Built once, with the component: a memoized node is skipped only while
    -- its render is the same function.
    cardsOf :: [Int] -> Html
    cardsOf ks = HH.div_ [HH.slot "card" k (child logRef) k Heard | k <- ks]
    memo :: [Int] -> Html
    memo = HH.memoized (==) cardsOf
    nestedOf :: ([Int], [Int]) -> Html
    nestedOf (ks, is) = HH.div_ (memo is : [HH.slot "card" k (child logRef) k Heard | k <- ks])
    memoNested :: ([Int], [Int]) -> Html
    memoNested = HH.memoized (==) nestedOf
    cardOf :: Int -> Html
    cardOf k = HH.slot "card" k (child logRef) k Heard
    memoCard :: Int -> Html
    memoCard = HH.memoized (==) cardOf

    render :: State -> Html
    render s =
      HH.div_
        $ [HH.text ("tick " <> tshow s.tick)]
        <> [body s | s.shown]
        <> [HH.text (if s.bad then throw (ErrorCall "no text") else "")]
    body s = case layout of
      Flat -> memo s.keys
      Nested -> memoNested (s.keys, s.inner)
      Side -> HH.div_ [memo s.keys, memo s.inner]
      KeyedList -> HH.keyed (ElemName "ul") [] [(tshow k, memo [k]) | k <- s.keys]
      KeyedRoot -> HH.keyed (ElemName "ul") [] [(tshow k, memoCard k) | k <- s.keys]

----------------------------------------------------------------------

data Mounted = Mounted
  { container :: N.NativeNode
  , ask :: forall a. Query a -> IO (Maybe a)
  , dispose :: IO ()
  , logRef :: Log
  }

mountWith :: (Log -> H.Component Query () Void MemDOM) -> IO Mounted
mountWith component = do
  logRef <- newIORef []
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.query = q, H.dispose = d} <- runMemDOM (VD.runUI (component logRef) () (N.fromNative container))
  pure Mounted {container, ask = runMemDOM . q, dispose = runMemDOM d, logRef}

mount :: Layout -> [Int] -> [Int] -> IO Mounted
mount layout keys inner = mountWith (\l -> parent l layout keys inner)

tell :: Mounted -> (() -> Query ()) -> IO ()
tell m q = () <$ askOf m (q ())

-- | The field is polymorphic, so not reached with a dot.
askOf :: Mounted -> Query a -> IO (Maybe a)
askOf Mounted {ask} = ask

-- | What happened since the last call.
taken :: Mounted -> IO [Text]
taken m = atomicModifyIORef' m.logRef ([],)

countOf :: Mounted -> Int -> IO (Maybe Int)
countOf m k = join <$> askOf m (AskCard k id)

markup :: Mounted -> IO Text
markup m = N.renderToText m.container

shows' :: Mounted -> [Text] -> IO ()
shows' m cards = do
  html <- markup m
  for_ cards $ \c -> assertWith ("the page shows " <> T.unpack c <> ": " <> T.unpack html) (("<em>" <> c <> "</em>") `T.isInfixOf` html)

-- | Every element under the container, with its identity, in order.
emIdents :: Mounted -> IO [Int]
emIdents m = map (.ident) . filter isEm <$> descendants m.container
  where
    isEm n = n.kind == N.ElementNode Nothing (ElemName "em")
    descendants node = do
      children <- N.childNodes node
      concat <$> traverse (\c -> (c :) <$> descendants c) children

----------------------------------------------------------------------

skipped :: IO ()
skipped = do
  m <- mount Flat [1, 2] []
  taken m >>= assertEqual "both cards started" ["init 1", "init 2"]
  tell m (BumpCard 1)
  taken m >>= assertEqual "the parent heard the bump" ["heard 1"]
  ems <- emIdents m
  tell m Tick
  tell m Tick
  taken m >>= assertEqual "nothing happened to the cards" []
  countOf m 1 >>= assertEqual "card 1 is answered, with its count" (Just 1)
  countOf m 2 >>= assertEqual "card 2 too" (Just 0)
  shows' m ["1:1", "2:0"]
  emIdents m >>= assertEqual "the same elements" ems
  -- Still the parent's children: a bump renders the card, and is heard.
  tell m (BumpCard 2)
  taken m >>= assertEqual "card 2's output is heard" ["heard 1"]
  shows' m ["1:1", "2:1"]
  m.dispose
  taken m >>= assertEqual "each finalized once, with the tree" ["fin 1", "fin 2"]

changed :: IO ()
changed = do
  m <- mount Flat [1, 2] []
  _ <- taken m
  tell m (BumpCard 2)
  _ <- taken m
  tell m (SetKeys [2, 3])
  log' <- taken m
  assertEqual "card 2 received, card 1 went, card 3 came" ["fin 1", "init 3", "receive 2"] (sortOn' log')
  countOf m 2 >>= assertEqual "card 2 kept its count" (Just 1)
  countOf m 1 >>= assertEqual "card 1 is gone" Nothing
  shows' m ["2:1", "3:0"]
  -- And skipped again afterwards, with what the last run rendered.
  tell m Tick
  taken m >>= assertEqual "nothing happened to the cards" []
  shows' m ["2:1", "3:0"]
  m.dispose
  where
    sortOn' = foldr insert' []
    insert' x [] = [x]
    insert' x (y : ys) = if x <= y then x : y : ys else y : insert' x ys

nested :: IO ()
nested = do
  m <- mount Nested [1] [2]
  taken m >>= assertEqual "both cards started" ["init 2", "init 1"]
  tell m (BumpCard 2)
  _ <- taken m
  tell m Tick
  taken m >>= assertEqual "skipping the outer node keeps both" []
  countOf m 2 >>= assertEqual "the inner card is answered" (Just 1)
  shows' m ["1:0", "2:1"]
  -- The outer node runs, the inner one is skipped within it.
  tell m (SetKeys [1, 3])
  taken m >>= assertEqual "card 1 received, card 3 came, the inner card left alone" ["receive 1", "init 3"]
  tell m Tick
  taken m >>= assertEqual "and the outer skipped again keeps all three" []
  countOf m 2 >>= assertEqual "the inner card still answers" (Just 1)
  shows' m ["1:0", "2:1", "3:0"]
  m.dispose

sideBySide :: IO ()
sideBySide = do
  m <- mount Side [1] [2]
  _ <- taken m
  tell m (SetInner [2, 3])
  taken m >>= assertEqual "the second node ran, the first was skipped" ["receive 2", "init 3"]
  countOf m 1 >>= assertEqual "card 1 is still there" (Just 0)
  shows' m ["1:0", "2:0", "3:0"]
  m.dispose

keyedMove :: IO ()
keyedMove = do
  m <- mount KeyedList [1, 2, 3] []
  _ <- taken m
  ems <- emIdents m
  tell m (SetKeys [3, 1, 2])
  taken m >>= assertEqual "moved, not made again" []
  emIdents m >>= assertEqual "the elements, in the new order" (map (ems !!) [2, 0, 1])
  tell m Tick
  taken m >>= assertEqual "and kept on" []
  emIdents m >>= assertEqual "where they were moved to" (map (ems !!) [2, 0, 1])
  m.dispose

-- | The card's root node changes while the memoized node around it, all
-- of whose HTML the card is, is skipped: the node stands for what the card
-- shows now, so a move takes that.
keyedRootMove :: IO ()
keyedRootMove = do
  m <- mount KeyedRoot [1, 2] []
  _ <- taken m
  tell m (ReshapeCard 1)
  markup m >>= assertEqual "card 1 reshaped in place" "<div><div>tick 0<ul><strong>1:-1</strong><em>2:0</em></ul></div></div>"
  tell m (SetKeys [2, 1])
  taken m >>= assertEqual "moved, not made again" []
  markup m >>= assertEqual "what card 1 shows now is moved" "<div><div>tick 0<ul><em>2:0</em><strong>1:-1</strong></ul></div></div>"
  tell m Tick
  markup m >>= assertEqual "and stays" "<div><div>tick 1<ul><em>2:0</em><strong>1:-1</strong></ul></div></div>"
  m.dispose

removed :: IO ()
removed = do
  m <- mount Nested [1] [2]
  _ <- taken m
  tell m Tick
  tell m Hide
  taken m >>= assertEqual "both finalized" ["fin 1", "fin 2"]
  html <- markup m
  assertWith ("no card on the page: " <> T.unpack html) (not ("<em>" `T.isInfixOf` html))
  m.dispose
  taken m >>= assertEqual "and not again" []

failedPass :: IO ()
failedPass = do
  m <- mount Flat [1] []
  _ <- taken m
  tell m (BumpCard 1)
  _ <- taken m
  failed <- try (tell m (Break True))
  assertEqual "the render failed" True (either (\(_ :: SomeException) -> True) (const False) failed)
  taken m >>= assertEqual "nothing happened to the card" []
  countOf m 1 >>= assertEqual "it is still there" (Just 1)
  tell m (Break False)
  -- The page is built again, so the card is rendered again, with no new
  -- start.
  log' <- taken m
  assertWith ("not made again: " <> show log') ("init 1" `notElem` log' && "fin 1" `notElem` log')
  countOf m 1 >>= assertEqual "with its count" (Just 1)
  shows' m ["1:1"]
  tell m Tick
  taken m >>= assertEqual "and skipped afterwards" []
  countOf m 1 >>= assertEqual "still there" (Just 1)
  m.dispose

-- | A tree adopted from a page rendered before it: its memoized node's
-- children are kept too.
hydrated :: IO ()
hydrated = do
  logRef <- newIORef []
  let component = parent logRef Flat [1, 2] []
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.dispose = gone} <- runMemDOM (VD.runUI component () (N.fromNative container))
  rendered <- N.childNodes container
  runMemDOM gone
  for_ rendered $ \node -> do
    stripListeners node
    runMemDOM (appendChild (N.fromNative node) (N.fromNative container))
  writeIORef logRef []
  H.HalogenSocket {H.query = q, H.dispose = d} <- runMemDOM (VD.hydrateUI component () (N.fromNative container))
  let m = Mounted {container, ask = runMemDOM . q, dispose = runMemDOM d, logRef}
  taken m >>= assertEqual "both cards started" ["init 1", "init 2"]
  ems <- emIdents m
  tell m Tick
  taken m >>= assertEqual "nothing happened to the cards" []
  countOf m 1 >>= assertEqual "card 1 is answered" (Just 0)
  emIdents m >>= assertEqual "the adopted elements" ems
  m.dispose
  where
    stripListeners node = do
      writeIORef node.listeners []
      mapM_ stripListeners =<< N.childNodes node

spec :: Spec
spec =
  describe "child components under a memoized node" $ do
    it "keeps them when the node is skipped: alive, answered, heard, no Receive" skipped
    it "reconciles them as any others when the node runs again" changed
    it "keeps those of a skipped node inside a skipped node" nested
    it "keeps a skipped node's beside one that runs" sideBySide
    it "keeps them when a keyed list moves their node" keyedMove
    it "moves what a child shows now when its node is moved" keyedRootMove
    it "finalizes them once when the node goes" removed
    it "loses none of them in a pass that fails" failedPass
    it "keeps those of a node adopted from the page" hydrated

#endif
