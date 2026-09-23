{-# LANGUAGE QualifiedDo #-}

-- | What the hooks actually do, watched from outside the component.
--
-- Every test drives a component the way a parent would — through queries —
-- and looks at what it rendered, what it raised, and what its effects wrote
-- into a probe. Nothing reaches inside the interpreter.
module Test.Hooks (spec) where

import Data.IORef
import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Types (HookK (UseQuery, UseState))
import Halogen.Subscription qualified as HS
import Protolude
import System.IO.Error (userError)
import System.IO.Unsafe (unsafePerformIO)
import Test.Harness (Harness (..), dispose, eventually, lastRender, query, start)
import Test.Hspec (Spec, anyIOException, describe, it, shouldBe, shouldThrow)

----------------------------------------------------------------------
-- A component that reports on itself.
----------------------------------------------------------------------

-- | What the component's effects write down as they happen.
data Probe = Probe
  { lifecycleRuns :: IORef Int
  , cleanupRuns :: IORef Int
  , tickLog :: IORef [TickEvent]
  , memoRuns :: IORef Int
  }

newProbe :: IO Probe
newProbe = do
  lifecycleRuns <- newIORef 0
  cleanupRuns <- newIORef 0
  tickLog <- newIORef []
  memoRuns <- newIORef 0
  pure Probe {lifecycleRuns, cleanupRuns, tickLog, memoRuns}

-- | What a tick effect did, and when — the order of these is the point.
data TickEvent
  = Ran Int
  | Cleaned Int
  deriving stock (Eq, Show)

data Q a
  = Bump a
  | BumpLater a
  | BumpAndWait (MVar ()) (MVar ()) a
  | BumpOther a
  | CurrentCount (Int -> a)
  | WriteRef Int a
  | ReadRef (Int -> a)

newtype Out = Counted Int
  deriving stock (Eq, Show)

-- | The memoised computation: the value it produces says how many times it has
-- been run, so a render that shows an unchanged value is a render that reused
-- the memo. Kept out of line so nothing can float the effect out of it.
countedMemo :: IORef Int -> Int -> (Int, Int)
countedMemo ref deps = unsafePerformIO $ atomicModifyIORef' ref $ \n -> (n + 1, (deps, n + 1))
{-# NOINLINE countedMemo #-}

probeComponent :: Probe -> H.Component Q () Out IO
probeComponent probe = Hooks.component @Empty $ \_input -> Hooks.do
  (count, countId) <- Hooks.useState (0 :: Int)
  (other, otherId) <- Hooks.useState (0 :: Int)
  (_, ref) <- Hooks.useRef (0 :: Int)
  memoed <- Hooks.useMemo count (countedMemo probe.memoRuns)

  Hooks.useLifecycleEffect $ do
    liftIO $ modifyIORef' probe.lifecycleRuns (+ 1)
    pure $ Just $ liftIO $ modifyIORef' probe.cleanupRuns (+ 1)

  Hooks.useTickEffect count $ do
    liftIO $ modifyIORef' probe.tickLog (<> [Ran count])
    pure $ Just $ liftIO $ modifyIORef' probe.tickLog (<> [Cleaned count])

  Hooks.useQuery $ \case
    Bump a -> do
      c <- Hooks.modify countId (+ 1)
      Hooks.raise (Counted c)
      pure (Just a)
    -- Forked, so it lands long after the query that started it has been
    -- answered and nothing else is going to run the program again.
    BumpLater a -> do
      void $ Hooks.fork $ do
        liftIO $ threadDelay 20_000
        Hooks.modify_ countId (+ 1)
      pure (Just a)
    BumpAndWait changed release a -> do
      void $ Hooks.fork $ do
        liftIO $ threadDelay 20_000
        Hooks.modify_ countId (+ 1)
        liftIO $ putMVar changed ()
        liftIO $ takeMVar release
      pure (Just a)
    BumpOther a -> Hooks.modify_ otherId (+ 1) $> Just a
    CurrentCount k -> Just . k <$> Hooks.get countId
    WriteRef v a -> liftIO (writeIORef ref v) $> Just a
    ReadRef k -> Just . k <$> liftIO (readIORef ref)

  Hooks.pure $
    HH.div_
      [HH.text ("count=" <> show count <> " other=" <> show other <> " memo=" <> show memoed)]

----------------------------------------------------------------------
-- A parent and a child, to see a hooks component query another one.
----------------------------------------------------------------------

newtype ChildQ a = ChildValue (Int -> a)

newtype ParentQ a = AskChild (Int -> a)

type PSlots = "child" .== H.Slot ChildQ Void ()

child :: H.Component ChildQ Int Void IO
child = Hooks.component @Empty $ \input -> Hooks.do
  Hooks.useQuery $ \case
    ChildValue k -> pure $ Just $ k (input * 2)
  Hooks.pure $ HH.text ("child=" <> show input)

parent :: H.Component ParentQ () Void IO
parent = Hooks.component parentHooks

-- | Written out to show what a hook program's type looks like when it is not
-- inferred: the hooks it uses, in the order it uses them.
parentHooks
  :: forall scope
   . ()
  -> Hooks.Hook scope ParentQ PSlots Void IO (UseState Int : UseQuery : '[]) '[] (Hooks.HookHTML scope PSlots Void IO)
parentHooks _input = Hooks.do
  (seen, seenId) <- Hooks.useState (0 :: Int)

  Hooks.useQuery $ \case
    AskChild k -> do
      value <- fromMaybe (-1) <$> Hooks.query "child" () (ChildValue identity)
      Hooks.put seenId value
      pure (Just (k value))

  Hooks.pure $
    HH.div_
      [ HH.text ("seen=" <> show seen)
      , HH.slot_ "child" () child 21
      ]

----------------------------------------------------------------------
-- A component driven by something other than a query.
----------------------------------------------------------------------

-- | Subscribes to an emitter of numbers and adds them up. Every hook program a
-- handler runs arrives the same way a DOM event's would.
subscriber :: HS.Emitter IO Int -> H.Component H.VoidF () Void IO
subscriber emitter = Hooks.component @Empty $ \_input -> Hooks.do
  (total, totalId) <- Hooks.useState (0 :: Int)

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (\n -> Hooks.modify_ totalId (+ n)) emitter
    pure Nothing

  Hooks.pure $ HH.text ("total=" <> show total)

-- A subscription can deliver an action synchronously from inside an effect.
-- The next pass must wait for that effect to return its cleanup.
reentrantComponent :: IORef [TickEvent] -> HS.Subscribe IO () -> H.Component H.VoidF () Void IO
reentrantComponent logRef source = Hooks.component @Empty $ \_ -> Hooks.do
  (count, countId) <- Hooks.useState (0 :: Int)
  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (\() -> Hooks.put countId 1) source.emitter
    pure Nothing
  Hooks.useTickEffect count $ do
    liftIO $ modifyIORef' logRef (<> [Ran count])
    when (count == 0) $ liftIO $ HS.notify source.listener ()
    pure $ Just $ liftIO $ modifyIORef' logRef (<> [Cleaned count])
  Hooks.pure $ HH.text (show count)

throwingComponent :: IORef [Int] -> HS.Emitter IO Int -> H.Component H.VoidF () Void IO
throwingComponent logRef source = Hooks.component @Empty $ \_ -> Hooks.do
  (count, countId) <- Hooks.useState (0 :: Int)
  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (Hooks.put countId) source
    pure Nothing
  Hooks.useTickEffect count $ do
    when (count == 1) $ liftIO $ throwIO (userError "effect failed")
    liftIO $ modifyIORef' logRef (<> [count])
    pure Nothing
  Hooks.pure $ HH.text (show count)

----------------------------------------------------------------------
-- Spec.
----------------------------------------------------------------------

withProbe :: (Probe -> Harness Q Out IO -> IO a) -> IO a
withProbe k = do
  probe <- newProbe
  harness <- start (probeComponent probe) ()
  a <- k probe harness
  dispose harness
  pure a

spec :: Spec
spec = describe "hooks" $ do
  it "renders what the hook program returns" $ withProbe $ \_probe harness -> do
    -- The driver renders once before it can run the program, so the first
    -- render of any hooks component is empty.
    readIORef harness.renders >>= (`shouldBe` ["", "count=0 other=0 memo=(0,1)"])

  it "re-renders when a handler changes state" $ withProbe $ \_probe harness -> do
    void $ query harness (H.mkTell Bump)
    lastRender harness >>= (`shouldBe` "count=1 other=0 memo=(1,2)")
    query harness (H.mkRequest CurrentCount) >>= (`shouldBe` Just 1)

  it "re-renders after a forked program changes state" $ withProbe $ \_probe harness -> do
    void $ query harness (H.mkTell BumpLater)
    lastRender harness >>= (`shouldBe` "count=0 other=0 memo=(0,1)")
    eventually "count=1 other=0 memo=(1,2)" (lastRender harness)

  it "renders a fork's state and effects before the fork finishes" $ withProbe $ \probe harness -> do
    changed <- newEmptyMVar
    release <- newEmptyMVar
    void $ query harness (H.mkTell (BumpAndWait changed release))
    takeMVar changed
    rendered <- lastRender harness
    ticks <- readIORef probe.tickLog
    putMVar release ()
    rendered `shouldBe` "count=1 other=0 memo=(1,2)"
    ticks `shouldBe` [Ran 0, Cleaned 0, Ran 1]

  it "finishes an effect before processing a reentrant state update" $ do
    ticks <- newIORef []
    source <- HS.create
    harness <- start (reentrantComponent ticks source) ()
    readIORef ticks >>= (`shouldBe` [Ran 0, Cleaned 0, Ran 1])
    lastRender harness >>= (`shouldBe` "1")
    dispose harness
    eventually [Ran 0, Cleaned 0, Ran 1, Cleaned 1] (readIORef ticks)

  it "allows another pass after an effect throws" $ do
    runs <- newIORef []
    source <- HS.create
    harness <- start (throwingComponent runs source.emitter) ()
    HS.notify source.listener 1 `shouldThrow` anyIOException
    HS.notify source.listener 2
    lastRender harness >>= (`shouldBe` "2")
    readIORef runs >>= (`shouldBe` [0, 2])
    dispose harness

  it "raises output from a handler" $ withProbe $ \_probe harness -> do
    void $ query harness (H.mkTell Bump)
    void $ query harness (H.mkTell Bump)
    readIORef harness.outputs >>= (`shouldBe` [Counted 1, Counted 2])

  it "runs a lifecycle effect once, however many renders there are" $ withProbe $ \probe harness -> do
    void $ query harness (H.mkTell Bump)
    void $ query harness (H.mkTell BumpOther)
    readIORef probe.lifecycleRuns >>= (`shouldBe` 1)

  it "runs a tick effect only when its dependencies change" $ withProbe $ \probe harness -> do
    readIORef probe.tickLog >>= (`shouldBe` [Ran 0])
    -- The previous run is cleaned up before the next one, not after it.
    void $ query harness (H.mkTell Bump)
    readIORef probe.tickLog >>= (`shouldBe` [Ran 0, Cleaned 0, Ran 1])
    -- A render caused by other state leaves the effect alone.
    void $ query harness (H.mkTell BumpOther)
    readIORef probe.tickLog >>= (`shouldBe` [Ran 0, Cleaned 0, Ran 1])

  it "recomputes a memo only when its dependencies change" $ withProbe $ \probe harness -> do
    void $ query harness (H.mkTell BumpOther)
    lastRender harness >>= (`shouldBe` "count=0 other=1 memo=(0,1)")
    readIORef probe.memoRuns >>= (`shouldBe` 1)
    void $ query harness (H.mkTell Bump)
    lastRender harness >>= (`shouldBe` "count=1 other=1 memo=(1,2)")

  it "keeps a ref across renders without asking for one" $ withProbe $ \_probe harness -> do
    before <- readIORef harness.renders
    void $ query harness (H.mkTell (WriteRef 7))
    readIORef harness.renders >>= (`shouldBe` before)
    query harness (H.mkRequest ReadRef) >>= (`shouldBe` Just 7)
    -- and it survives a render caused by something else
    void $ query harness (H.mkTell Bump)
    query harness (H.mkRequest ReadRef) >>= (`shouldBe` Just 7)

  it "runs effect cleanups when the component is finalized" $ do
    probe <- newProbe
    harness <- start (probeComponent probe) ()
    void $ query harness (H.mkTell Bump)
    readIORef probe.cleanupRuns >>= (`shouldBe` 0)
    dispose harness
    eventually 1 (readIORef probe.cleanupRuns)
    readIORef probe.tickLog >>= (`shouldBe` [Ran 0, Cleaned 0, Ran 1, Cleaned 1])

  it "handles actions from a subscription" $ do
    source <- HS.create
    harness <- start (subscriber source.emitter) ()
    HS.notify source.listener 3
    HS.notify source.listener 4
    lastRender harness >>= (`shouldBe` "total=7")
    dispose harness

  it "queries a child component" $ do
    harness <- start parent ()
    query harness (H.mkRequest AskChild) >>= (`shouldBe` Just 42)
    -- A parent's render renders its children too, so the parent's own text is
    -- not the last line of the log.
    rendered <- readIORef harness.renders
    ("seen=42" `elem` rendered) `shouldBe` True
    dispose harness
