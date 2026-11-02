{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A JavaScript promise, listened to from the moment it is made and
-- waited for when the program wants its value: what
-- "Control.Concurrent.Async"'s @Async@ is to a thread, with the same
-- vocabulary ('wait', 'waitCatch', 'poll', their 'STM' forms, 'waitAny',
-- 'waitEither', 'waitBoth', 'mapConcurrently', 'concurrently', 'race').
--
-- A binding that starts asynchronous work (a request, a timer, a dialog)
-- returns a 'Promise' at once: the JavaScript has started the work by the
-- time it returns, and nothing has waited.
--
-- > $(browserJS ["jsbits/net.js"]
-- >   [("js_fetch_text", "halogen_net_fetch_text", Safe, [t| JSText -> IO (Promise Text) |])])
--
-- 'Halogen.JSBits.browserJS' and the fields of "Halogen.JSBits.Fields" make
-- a 'Promise' of the JavaScript promise a function returns with 'promise',
-- which hands it two callbacks at once. So a promise is always listened to,
-- and one rejected before anyone waits for it is not an unhandled rejection
-- (which a browser logs and Node ends its process for).
--
-- A component can start work in @H.liftEffect@, inside the dispatch of the
-- event that asked for it, keep the 'Promise' in its state, then 'wait' for
-- it in @lift@ (on a worker, while the rest of the tree goes on), or never.
-- Waiting puts the calling thread to sleep, and the runtime goes on with the
-- rest of the program and gives the page back to the browser meanwhile; so a
-- wait belongs where a thread may wait, not in @H.liftEffect@.
--
-- Unlike an @Async@, a promise cannot be cancelled: there is no @cancel@,
-- and 'race' leaves the loser to finish. Killing the thread that waits stops
-- the wait, not the work. A binding for an API that takes an @AbortSignal@
-- (@fetch@ does) should take one and hand it on.
--
-- A 'Promise' says the type of what it resolves with, read with 'FromJS'
-- when it settles; @'Promise' 'JSVal'@ leaves the decoding to the
-- binding's wrapper. A rejection, and an exception a function throws, is a
-- 'JSError'. Natively every promise has been rejected with
-- @'JSError' "NotInBrowserError" _@.
module Halogen.JSBits.Promise
  ( -- * Promises
    Promise (..)
  , promise
  , resolved
  , rejected
  , FromJS (..)
  , JSError (..)
  , checkThrown

    -- * Waiting for one
  , wait
  , waitCatch
  , poll
  , waitSTM
  , waitCatchSTM
  , pollSTM

    -- * Waiting for several
  , waitAny
  , waitAnyCatch
  , waitEither
  , waitEitherCatch
  , waitBoth
  , waitAll
  , waitAllSTM

    -- * Starting several
  , mapConcurrently
  , forConcurrently
  , mapConcurrently_
  , forConcurrently_
  , concurrently
  , race
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, void, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (STM, atomically, newTVarIO, orElse, readTVar, retry, throwSTM, writeTVar)
import Halogen.JSBits.Inline (inlineJS)
import Halogen.JSBits.Value (Callback, Inert (..), JSError (..), JSVal, checkThrown, freeCallback, fromJSBytes, inBrowser, jsNull, jsValText, mkCallback)

-- | A JavaScript promise and the wait for its outcome.
data Promise a = Promise
  { rawPromise :: JSVal
  -- ^ The promise itself, to hand back to JavaScript (@Promise.race@, a
  -- binding that takes one). A 'fmap' leaves it as it is.
  , settled :: STM (Either JSError a)
  -- ^ Its outcome: blocks ('retry') until it has settled, then gives what
  -- it resolved with or what it was rejected with, as often as asked
  -- ('waitCatchSTM').
  }

instance Functor Promise where
  fmap f p = Promise {rawPromise = p.rawPromise, settled = fmap f <$> p.settled}

-- | A promise that has resolved with this value, made in Haskell: its
-- 'rawPromise' is @null@, not a JavaScript promise.
resolved :: a -> Promise a
resolved a = Promise {rawPromise = jsNull, settled = pure (Right a)}

-- | A promise that has been rejected with this error, made in Haskell.
rejected :: JSError -> Promise a
rejected e = Promise {rawPromise = jsNull, settled = pure (Left e)}

-- What every wait gives natively, where no engine runs a promise.
notInBrowser :: JSError
notInBrowser = JSError {name = "NotInBrowserError", message = "there is no JavaScript engine here to run a promise"}

-- | Natively, where no engine makes one: a promise that has already been
-- rejected, with @'JSError' "NotInBrowserError" _@.
instance Inert (Promise a) where
  inert = rejected notInBrowser

$( inlineJS
     "js_then"
     "Promise.resolve($1).then($2, (error) => $3({ ok: false, error: error }))"
     [t|JSVal -> Callback -> Callback -> IO ()|]
 )
$(inlineJS "js_as_int" "Number($1)" [t|JSVal -> IO Int|])
$(inlineJS "js_as_double" "Number($1)" [t|JSVal -> IO Double|])
$(inlineJS "js_as_bool" "$1 === true" [t|JSVal -> IO Bool|])
$(inlineJS "js_absent" "$1 === null || $1 === undefined" [t|JSVal -> IO Bool|])
$(inlineJS "js_array_length" "Array.isArray($1) ? $1.length : 0" [t|JSVal -> IO Int|])
$(inlineJS "js_array_index" "$1[$2]" [t|JSVal -> Int -> IO JSVal|])

-- | How a promise's resolved value is read as a Haskell value. A newtype
-- over a 'JSVal' (an object's type) derives it:
-- @deriving newtype (FromJS)@.
class FromJS a where
  fromJS :: JSVal -> IO a

instance FromJS JSVal where fromJS = pure

-- | Whatever it resolved with is ignored.
instance FromJS () where fromJS _ = pure ()

-- | A string.
instance FromJS Text where fromJS = pure . jsValText

-- | A @Uint8Array@, copied out.
instance FromJS ByteString where fromJS = fromJSBytes

-- | A number, as an 'Int' the way the FFI converts one.
instance FromJS Int where fromJS = js_as_int

instance FromJS Double where fromJS = js_as_double

instance FromJS Bool where fromJS = js_as_bool

-- | An array, each element read as @a@. Read through the array's length and
-- its elements by index, the same on both browser backends: the JavaScript
-- backend's @fromJSArray@ has no WebAssembly counterpart. A value that is
-- not an array reads as an empty one.
instance (FromJS a) => FromJS [a] where
  fromJS v = do
    count <- js_array_length v
    traverse (fromJS <=< js_array_index v) [0 .. count - 1]

-- | @null@ or @undefined@ as 'Nothing'.
instance (FromJS a) => FromJS (Maybe a) where
  fromJS v = do
    absent <- js_absent v
    if absent then pure Nothing else Just <$> fromJS v

-- | A 'Promise' of a JavaScript value: a promise, or any value, which then
-- stands for one that has resolved with it. Its @then@ gets two callbacks at
-- once; when one runs, both are freed and the outcome is kept for 'wait'
-- (what @async@ is to @Async@).
-- A value that 'FromJS' cannot read is a rejection with the name
-- @FromJSError@.
--
-- A promise that never settles keeps its two callbacks. Natively the
-- promise has already been rejected (@NotInBrowserError@).
promise :: (FromJS a) => JSVal -> IO (Promise a)
promise raw
  | not inBrowser = pure (rejected notInBrowser) {rawPromise = raw}
  | otherwise = do
      box <- newTVarIO Nothing
      made <- newEmptyMVar
      let settle outcome = do
            (resolve, reject) <- readMVar made
            freeCallback resolve
            freeCallback reject
            atomically (writeTVar box (Just outcome))
      resolve <- mkCallback $ \value ->
        try (fromJS value)
          >>= settle . \case
            Left (e :: SomeException) -> Left JSError {name = "FromJSError", message = Text.pack (displayException e)}
            Right a -> Right a
      reject <- mkCallback $ \outcome ->
        try (checkThrown outcome)
          >>= settle . \case
            Left e -> Left e
            Right () -> Left JSError {name = "Error", message = "rejected"}
      putMVar made (resolve, reject)
      js_then raw resolve reject
      pure Promise {rawPromise = raw, settled = readTVar box >>= maybe retry pure}

-- | Wait for the promise to settle and give what it resolved with, or throw
-- what it was rejected with.
wait :: (MonadIO m) => Promise a -> m a
wait = liftIO . atomically . waitSTM

-- | Wait for the promise to settle and give its outcome; never throws.
waitCatch :: (MonadIO m) => Promise a -> m (Either JSError a)
waitCatch = liftIO . atomically . waitCatchSTM

-- | The outcome, if the promise has settled; without waiting.
poll :: (MonadIO m) => Promise a -> m (Maybe (Either JSError a))
poll = liftIO . atomically . pollSTM

waitSTM :: Promise a -> STM a
waitSTM p = p.settled >>= either throwSTM pure

waitCatchSTM :: Promise a -> STM (Either JSError a)
waitCatchSTM p = p.settled

pollSTM :: Promise a -> STM (Maybe (Either JSError a))
pollSTM p = (Just <$> p.settled) `orElse` pure Nothing

-- | Wait for whichever promise settles first, and give it with what it
-- resolved with, or throw what it was rejected with. With no promises it
-- waits for ever.
waitAny :: (MonadIO m) => [Promise a] -> m (Promise a, a)
waitAny ps = liftIO . atomically $ foldr (\p rest -> ((p,) <$> waitSTM p) `orElse` rest) retry ps

-- | 'waitAny', giving the outcome rather than throwing.
waitAnyCatch :: (MonadIO m) => [Promise a] -> m (Promise a, Either JSError a)
waitAnyCatch ps = liftIO . atomically $ foldr (\p rest -> ((p,) <$> waitCatchSTM p) `orElse` rest) retry ps

-- | Wait for whichever of the two settles first.
waitEither :: (MonadIO m) => Promise a -> Promise b -> m (Either a b)
waitEither a b = liftIO . atomically $ (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

waitEitherCatch :: (MonadIO m) => Promise a -> Promise b -> m (Either (Either JSError a) (Either JSError b))
waitEitherCatch a b = liftIO . atomically $ (Left <$> waitCatchSTM a) `orElse` (Right <$> waitCatchSTM b)

-- | Wait for both, or throw the first rejection as soon as it comes, even
-- while the other is still pending.
waitBoth :: (MonadIO m) => Promise a -> Promise b -> m (a, b)
waitBoth a b = liftIO . atomically $ do
  x <- waitSTM a `orElse` (waitSTM b >> retry)
  y <- waitSTM b
  pure (x, y)

-- | Wait for every promise and give what each resolved with, in order, or
-- throw a rejection as soon as one comes.
waitAll :: (MonadIO m) => [Promise a] -> m [a]
waitAll = liftIO . atomically . waitAllSTM

waitAllSTM :: [Promise a] -> STM [a]
waitAllSTM ps = do
  -- A rejection already there is thrown at once; otherwise the wait goes
  -- on until all have settled, and is tried again as each one does.
  forM_ ps $ \p -> (p.settled >>= either throwSTM (const (pure ()))) `orElse` pure ()
  traverse waitSTM ps

-- | Start the work for each element (each call returns its promise at once,
-- so the work runs side by side in the page), then wait for all of it, as
-- 'waitAll' does: the results in order, or the first rejection.
--
-- > pages <- mapConcurrently (fetchText . pageUrl) ids
mapConcurrently :: (MonadIO m, Traversable t) => (a -> m (Promise b)) -> t a -> m (t b)
mapConcurrently f xs = do
  ps <- traverse f xs
  liftIO . atomically $ do
    forM_ ps $ \p -> (p.settled >>= either throwSTM (const (pure ()))) `orElse` pure ()
    traverse waitSTM ps

forConcurrently :: (MonadIO m, Traversable t) => t a -> (a -> m (Promise b)) -> m (t b)
forConcurrently = flip mapConcurrently

mapConcurrently_ :: (MonadIO m, Foldable f) => (a -> m (Promise b)) -> f a -> m ()
mapConcurrently_ f = void . mapConcurrently f . foldr (:) []

forConcurrently_ :: (MonadIO m, Foldable f) => f a -> (a -> m (Promise b)) -> m ()
forConcurrently_ = flip mapConcurrently_

-- | Start both, and wait for both ('waitBoth').
concurrently :: (MonadIO m) => m (Promise a) -> m (Promise b) -> m (a, b)
concurrently a b = do
  pa <- a
  pb <- b
  waitBoth pa pb

-- | Start both, and give the result of whichever settles first
-- ('waitEither'). The other is not cancelled, since a promise cannot be: its
-- work goes on to the end, its outcome unread.
race :: (MonadIO m) => m (Promise a) -> m (Promise b) -> m (Either a b)
race a b = do
  pa <- a
  pb <- b
  waitEither pa pb
