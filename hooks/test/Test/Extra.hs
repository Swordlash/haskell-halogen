{-# LANGUAGE QualifiedDo #-}

-- | The hooks in "Halogen.Hooks.Extra.Hooks".
--
-- Each component here is driven by an emitter rather than by queries, because
-- what these hooks are for is programs that outlive the render that made them:
-- a subscription handler is built once, on the first render, and everything
-- interesting follows from that.
module Test.Extra (spec) where

import Data.IORef
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Hooks (EventApi (..), useDebouncer, useEvent, useGet, useModifyState_, usePrevious, usePutState, useThrottle)
import Halogen.Subscription qualified as HS
import Protolude
import Test.Harness (dispose, eventually, lastRender, start)
import Test.Hspec (Spec, describe, it, shouldBe)

----------------------------------------------------------------------
-- useGet and usePutState.
----------------------------------------------------------------------

data Staleness
  = Bump
  | Report

-- | Records what a handler built on the first render can see of the state as
-- it is now: through 'useGet', and through the closure it was born with.
stalenessComponent :: IORef (Int, Int) -> HS.Emitter IO Staleness -> H.Component H.VoidF () Void IO
stalenessComponent probe events = Hooks.component @Empty $ \_input -> Hooks.do
  (count, setCount) <- usePutState (0 :: Int)
  getCount <- useGet count

  Hooks.useLifecycleEffect $ do
    -- Built once. @count@ in here is the first render's 0 for ever after.
    void $ Hooks.subscribe $ flip map events $ \case
      Bump -> getCount >>= setCount . (+ 1)
      Report -> do
        latest <- getCount
        liftIO $ writeIORef probe (latest, count)
    pure Nothing

  Hooks.pure $ HH.text ("count=" <> show count)

----------------------------------------------------------------------
-- useDebouncer and useThrottle.
----------------------------------------------------------------------

-- | Everything pushed at the emitter goes through the hook, and what comes out
-- the far side lands in the probe.
debouncerComponent :: IORef [Text] -> HS.Emitter IO Text -> H.Component H.VoidF () Void IO
debouncerComponent probe events = Hooks.component @Empty $ \_input -> Hooks.do
  push <- useDebouncer 0.05 $ \t -> liftIO $ modifyIORef' probe (<> [t])

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map push events
    pure Nothing

  Hooks.pure $ HH.text "debouncer"

-- | The debounced value goes into hook state rather than an 'IORef', which is
-- what a page would do with it. Whether it reaches the screen is then a
-- question about the hook, not about the probe.
settlingComponent :: HS.Emitter IO Text -> H.Component H.VoidF () Void IO
settlingComponent events = Hooks.component @Empty $ \_input -> Hooks.do
  (settled, setSettled) <- usePutState ""
  push <- useDebouncer 0.05 setSettled

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map push events
    pure Nothing

  Hooks.pure $ HH.text ("settled=" <> settled)

throttleComponent :: IORef [Text] -> HS.Emitter IO Text -> H.Component H.VoidF () Void IO
throttleComponent probe events = Hooks.component @Empty $ \_input -> Hooks.do
  push <- useThrottle 0.05 $ \t -> liftIO $ modifyIORef' probe (<> [t])

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map push events
    pure Nothing

  Hooks.pure $ HH.text "throttle"

----------------------------------------------------------------------
-- usePrevious.
----------------------------------------------------------------------

-- | Renders the count and what the count was on the render before.
previousComponent :: HS.Emitter IO () -> H.Component H.VoidF () Void IO
previousComponent events = Hooks.component @Empty $ \_input -> Hooks.do
  (count, bump) <- useModifyState_ (0 :: Int)
  previous <- usePrevious count

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (\() -> bump (+ 1)) events
    pure Nothing

  Hooks.pure $ HH.text ("count=" <> show count <> " previous=" <> show previous)

----------------------------------------------------------------------
-- useEvent.
----------------------------------------------------------------------

data EventTest
  = Push Text
  | Listen

eventComponent :: IORef [Text] -> HS.Emitter IO EventTest -> H.Component H.VoidF () Void IO
eventComponent probe events = Hooks.component @Empty $ \_input -> Hooks.do
  api <- useEvent

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ flip map events $ \case
      Push t -> api.push t
      Listen -> void $ api.setCallback $ \unsubscribe t -> do
        liftIO $ modifyIORef' probe (<> [t])
        -- A handler can take itself off, which is what the program it is
        -- handed is for.
        when (t == "last") unsubscribe
    pure Nothing

  Hooks.pure $ HH.text "event"

----------------------------------------------------------------------
-- Spec.
----------------------------------------------------------------------

spec :: Spec
spec = describe "hooks-extra" $ do
  it "reads the latest value through useGet, not the rendered one" $ do
    probe <- newIORef (-1, -1)
    source <- HS.create
    harness <- start (stalenessComponent probe source.emitter) ()
    HS.notify source.listener Bump
    HS.notify source.listener Bump
    HS.notify source.listener Report
    lastRender harness >>= (`shouldBe` "count=2")
    readIORef probe >>= (`shouldBe` (2, 0))
    dispose harness

  it "debounces: only the last of a burst runs, and only once" $ do
    probe <- newIORef []
    source <- HS.create
    harness <- start (debouncerComponent probe source.emitter) ()
    traverse_ (HS.notify source.listener) ["a", "b", "c"]
    readIORef probe >>= (`shouldBe` [])
    eventually ["c"] (readIORef probe)
    threadDelay 150_000
    readIORef probe >>= (`shouldBe` ["c"])
    dispose harness

  it "shows a debounced state change without waiting for another event" $ do
    source <- HS.create
    harness <- start (settlingComponent source.emitter) ()
    traverse_ (HS.notify source.listener) ["a", "b", "c"]
    lastRender harness >>= (`shouldBe` "settled=")
    eventually "settled=c" (lastRender harness)
    dispose harness

  it "throttles: the first of a burst runs at once, the last after the period" $ do
    probe <- newIORef []
    source <- HS.create
    harness <- start (throttleComponent probe source.emitter) ()
    traverse_ (HS.notify source.listener) ["a", "b", "c"]
    readIORef probe >>= (`shouldBe` ["a"])
    eventually ["a", "c"] (readIORef probe)
    threadDelay 150_000
    readIORef probe >>= (`shouldBe` ["a", "c"])
    dispose harness

  it "remembers what a value was on the render before" $ do
    source <- HS.create
    harness <- start (previousComponent source.emitter) ()
    lastRender harness >>= (`shouldBe` "count=0 previous=Nothing")
    HS.notify source.listener ()
    lastRender harness >>= (`shouldBe` "count=1 previous=Just 0")
    HS.notify source.listener ()
    lastRender harness >>= (`shouldBe` "count=2 previous=Just 1")
    dispose harness

  it "pushes events to a handler that can remove itself" $ do
    probe <- newIORef []
    source <- HS.create
    harness <- start (eventComponent probe source.emitter) ()
    -- Nothing is listening yet.
    HS.notify source.listener (Push "dropped")
    readIORef probe >>= (`shouldBe` [])
    HS.notify source.listener Listen
    HS.notify source.listener (Push "a")
    HS.notify source.listener (Push "last")
    HS.notify source.listener (Push "after")
    readIORef probe >>= (`shouldBe` ["a", "last"])
    dispose harness
