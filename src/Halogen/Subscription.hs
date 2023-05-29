module Halogen.Subscription where

import Control.Arrow ((&&&))
import Control.Monad.Ref
import Control.Monad.UUID
import Data.Coerce
import Data.Functor.Contravariant
import Protolude

-- | A paired `Listener` and `Emitter` produced with the `create` function.
data Subscribe m a = Subscribe
  { listener :: Listener m a
  , emitter :: Emitter m a
  }

create :: (MonadRef m, MonadUUID m) => m (Subscribe m a)
create = do
  subscribers <- newRef []
  pure $
    Subscribe
      { emitter = Emitter $ \handler -> do
          uuid <- generateV4
          modifyRef' subscribers (<> [(handler, uuid)])
          pure $
            Subscription
              { unsubscribe = modifyRef' subscribers (Protolude.filter (\(_, uuid') -> uuid /= uuid'))
              }
      , listener = Listener {notify = \a -> readRef subscribers >>= traverse_ (\(k, _) -> k a)}
      }

newtype Emitter m a = Emitter {registerHandler :: (a -> m ()) -> m (Subscription m)}
  deriving (Functor)

instance MonadRef m => Applicative (Emitter m) where
  pure a = Emitter $ \k -> do
    k a
    pure (Subscription (pure ()))

  (Emitter e1) <*> (Emitter e2) = Emitter $ \k -> do
    latestA <- newRef Nothing
    latestB <- newRef Nothing
    Subscription c1 <- e1 $ \a -> do
      writeRef latestA (Just a)
      readRef latestB >>= traverse_ (k . a)
    Subscription c2 <- e2 $ \b -> do
      writeRef latestB (Just b)
      readRef latestA >>= traverse_ (k . ($ b))
    pure (Subscription (c1 *> c2))

instance MonadRef m => Alternative (Emitter m) where
  empty = Emitter $ \_ -> pure (Subscription (pure ()))
  (Emitter f) <|> (Emitter g) = Emitter $ \k -> do
    Subscription c1 <- f k
    Subscription c2 <- g k
    pure (Subscription (c1 *> c2))

makeEmitter
  :: Functor m
  => ((a -> m ()) -> m (m ()))
  -> Emitter m a
makeEmitter f = Emitter (fmap Subscription . f)

----------------------------------------------------------------------

newtype Listener m a = Listener {notify :: a -> m ()}

instance Contravariant (Listener m) where
  contramap f (Listener g) = coerce (g . f)

newtype Subscription m = Subscription {unsubscribe :: m ()}

subscribe
  :: Functor m
  => Emitter m a
  -> (a -> m r)
  -> m (Subscription m)
subscribe em k = em.registerHandler (void . k)

fold :: MonadAtomicRef m => (a -> b -> b) -> Emitter m a -> b -> Emitter m b
fold f (Emitter e) b = Emitter $ \k -> do
  result <- newRef b
  e $ \a -> atomicModifyRef' result (f a &&& f a) >>= k

filter :: Applicative m => (a -> Bool) -> Emitter m a -> Emitter m a
filter p (Emitter e) = Emitter $ \k -> e $ \a -> when (p a) (k a)

{-
-- | Compute a fixed point.
fix :: forall i o. (Emitter i -> (Emitter i, Emitter o)) -> Emitter o
fix f = Emitter $ \k -> do
  Subscription c1 <- subscribe input (notify listener)
  Subscription c2 <- subscribe output k
  pure (Subscription (c1 *> c2))
  where
  SubscribeIO { emitter, listener } = unsafePerformIO create
  (input, output) = f emitter
  -}
