-- | Selectors: what part of the store a component cares about, and when
-- that part counts as changed.
module Halogen.Store.Select
  ( Selector (..)
  , select
  , selectEq
  , selectAll
  , selectEmitter
  )
where

import Halogen.Subscription qualified as HS
import Protolude hiding (Selector)
import UnliftIO.IORef (atomicModifyIORef', newIORef)

-- | A part of the store and a test of whether it changed. A component
-- connected through a selector is told of an update only when 'select'
-- gives a value that 'eq' says differs from the last one it was told of,
-- so a change elsewhere in the store does not render it again.
data Selector store a = Selector
  { eq :: a -> a -> Bool
  , select :: store -> a
  }

-- | A selector that compares what it selects with the function given: for
-- a part of the store with no 'Eq' instance, or one compared by less than
-- all of it (by an identifier, say).
select :: forall store a. (a -> a -> Bool) -> (store -> a) -> Selector store a
select eq selecting = Selector {eq, select = selecting}

-- | A selector that compares what it selects with '=='. The usual one:
-- @selectEq (.count)@.
selectEq :: forall store a. (Eq a) => (store -> a) -> Selector store a
selectEq = select (==)

-- | The whole store, which counts as changed on every update.
--
-- purescript-halogen-store compares the store with @unsafeRefEq@ here, so
-- that an update which hands back the very same value tells no one.
-- Haskell has no honest equivalent (whether two values are one pointer
-- depends on what the optimiser and the garbage collector happen to have
-- done), so this selector says every update is a change. For a store with
-- an 'Eq' instance, @selectEq identity@ is the selector that notices when
-- an update changed nothing.
selectAll :: forall store. Selector store store
selectAll = select (\_ _ -> False) identity

-- | The values a selector picks out of an emitter of stores, each only when
-- it differs from the last one pushed.
--
-- What was last pushed is kept per subscription, so each subscriber is
-- compared with what it was itself told, and the first update a subscriber
-- hears of always reaches it. The emitter carries updates only: what the
-- store holds when one subscribes is read with
-- 'Halogen.Store.Monad.getStore'.
selectEmitter :: forall store a m. (MonadIO m) => Selector store a -> HS.Emitter m store -> HS.Emitter m a
selectEmitter selector emitter = HS.makeEmitter $ \push -> do
  previous <- liftIO (newIORef Nothing)
  subscription <- HS.subscribe emitter $ \store -> do
    let derived = selector.select store
    unchanged <- liftIO $ atomicModifyIORef' previous $ \case
      Just old | selector.eq old derived -> (Just old, True)
      _ -> (Just derived, False)
    unless unchanged (push derived)
  pure (HS.unsubscribe subscription)
