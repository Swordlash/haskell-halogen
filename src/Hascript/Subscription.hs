module Hascript.Subscription where

import Protolude
import Data.Functor.Contravariant
import Data.IORef
import Data.Coerce
import Control.Arrow ((&&&))
import Data.UUID.V4


-- | A paired `Listener` and `Emitter` produced with the `create` function.
data SubscribeIO a = SubscribeIO
  { listener :: Listener a
  , emitter :: Emitter a
  }

create :: forall a. IO (SubscribeIO a)
create = do
  subscribers <- newIORef []
  pure $
    SubscribeIO
    { emitter = Emitter $ \k -> do
        uuid <- nextRandom
        modifyIORef' subscribers (<> [(k, uuid)])
        pure $ Subscription $ do
          modifyIORef' subscribers (Protolude.filter (\(_, uuid') -> uuid /= uuid'))
    , listener = Listener $ \a -> do
        readIORef subscribers >>= traverse_ (\(k, _) -> k a)
    }

newtype Emitter a = Emitter ((a -> IO ()) -> IO Subscription)
  deriving Functor

instance Applicative Emitter where
  pure a = Emitter $ \k -> do
    k a
    pure (Subscription (pure ()))

  (Emitter e1) <*> (Emitter e2) = Emitter $ \k -> do
    latestA <- newIORef Nothing
    latestB <- newIORef Nothing
    Subscription c1 <- e1 $ \a -> do
      writeIORef latestA (Just a)
      readIORef latestB >>= traverse_ (k . a)
    Subscription c2 <- e2 $ \b -> do
      writeIORef latestB (Just b)
      readIORef latestA >>= traverse_ (k . ($ b))
    pure (Subscription (c1 *> c2))


instance Alternative Emitter where
  empty = Emitter $ \_ -> pure (Subscription (pure ()))
  (Emitter f) <|> (Emitter g) = Emitter $ \k -> do
    Subscription c1 <- f k
    Subscription c2 <- g k
    pure (Subscription (c1 *> c2))


instance Semigroup a => Semigroup (Emitter a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Emitter a) where
  mempty = Emitter mempty

makeEmitter
  :: forall a
   . ((a -> IO ()) -> IO (IO ()))
  -> Emitter a
makeEmitter = coerce

----------------------------------------------------------------------

newtype Listener a = Listener (a -> IO ())

instance Contravariant Listener where
  contramap f (Listener g) = coerce (g . f)


notify :: forall a. Listener a -> a -> IO ()
notify (Listener f) = f

newtype Subscription = Subscription (IO ())
  deriving newtype (Semigroup, Monoid)

subscribe
  :: forall r a
   . Emitter a
  -> (a -> IO r)
  -> IO Subscription
subscribe (Emitter e) k = e (void . k)

unsubscribe :: Subscription -> IO ()
unsubscribe (Subscription unsub) = unsub

fold :: (a -> b -> b) -> Emitter a -> b -> Emitter b
fold f (Emitter e) b = Emitter $ \k -> do
  result <- newIORef b
  e $ \a -> atomicModifyIORef' result (f a &&& f a) >>= k

filter :: forall a. (a -> Bool) -> Emitter a -> Emitter a
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