{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A JavaScript promise, listened to from the moment it is made, and
-- awaited when the program wants its value: as "Control.Concurrent.Async"'s
-- @Async@ is to a thread.
--
-- A binding that starts asynchronous work (a request, a timer, a dialog)
-- returns a 'Promise' at once: the JavaScript has started the work by the
-- time it returns, and nothing has waited.
--
-- > $(browserJS ["jsbits/net.js"]
-- >   [("js_fetch_text", "halogen_net_fetch_text", Unsafe, [t| JSText -> IO (Promise Text) |])])
--
-- 'Halogen.JSBits.browserJS' and the fields of "Halogen.JSBits.Fields" make
-- a 'Promise' of the JavaScript promise a function returns with 'promise',
-- which hands it two callbacks at once. So a promise is always listened to,
-- and one rejected before anyone awaits it is not an unhandled rejection
-- (which a browser logs and Node ends its process for).
--
-- 'await' is the one place that waits, and gives a rejection back as a
-- value. A component can start work in @H.liftEffect@, inside the dispatch
-- of the event that asked for it, keep the 'Promise' in its state, then
-- 'await' it in @lift@ (on a worker, while the rest of the tree goes on),
-- or never. 'wait' is the same in 'STM', so that waits compose: 'awaitAny'
-- takes whichever of several settles first.
--
-- A 'Promise' says the type of what it resolves with, read with 'FromJS'
-- when it settles; @'Promise' 'JSVal'@ leaves the decoding to the
-- binding's wrapper. It is a 'Functor'.
module Halogen.JSBits.Promise
  ( Promise (..)
  , FromJS (..)
  , JSError (..)
  , promise
  , await
  , awaitOrThrow
  , awaitAll
  , awaitAny
  , checkThrown
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (STM, atomically, newTVarIO, orElse, readTVar, retry, writeTVar)
import Halogen.JSBits.Inline (inlineJS)
import Halogen.JSBits.Value (Callback, Inert (..), JSError (..), JSVal, checkThrown, freeCallback, fromJSBytes, inBrowser, jsNull, jsValText, mkCallback)

-- | A JavaScript promise and the wait for its outcome.
data Promise a = Promise
  { rawPromise :: JSVal
  -- ^ The promise itself, to hand back to JavaScript (@Promise.race@, a
  -- binding that takes one). A 'fmap' leaves it as it is.
  , wait :: STM (Either JSError a)
  -- ^ Its outcome: blocks ('retry') until it has settled, then gives what
  -- it resolved with or what it was rejected with, as often as asked.
  }

instance Functor Promise where
  fmap f p = Promise {rawPromise = p.rawPromise, wait = fmap f <$> p.wait}

-- What every wait gives natively, where no engine runs a promise.
notInBrowser :: JSError
notInBrowser = JSError {name = "NotInBrowserError", message = "there is no JavaScript engine here to run a promise"}

-- | Natively, where no engine makes one: a promise that has already been
-- rejected, with @'JSError' "NotInBrowserError" _@.
instance Inert (Promise a) where
  inert = Promise {rawPromise = jsNull, wait = pure (Left notInBrowser)}

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
-- once; when one runs, both are freed and the outcome is kept for 'wait'.
-- A value that 'FromJS' cannot read is a rejection with the name
-- @FromJSError@.
--
-- A promise that never settles keeps its two callbacks. Natively the
-- promise has already been rejected (@NotInBrowserError@).
promise :: (FromJS a) => JSVal -> IO (Promise a)
promise raw
  | not inBrowser = pure Promise {rawPromise = raw, wait = pure (Left notInBrowser)}
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
      pure Promise {rawPromise = raw, wait = readTVar box >>= maybe retry pure}

-- | Wait for the promise to settle, and give what it resolved with, or what
-- it was rejected with. It never throws a rejection, on any backend.
--
-- The calling thread sleeps until the outcome is there, and the runtime
-- goes on with the rest of the program and gives the page back to the
-- browser meanwhile. So it waits only where a thread may wait: in @lift@,
-- not in @H.liftEffect@.
--
-- Waiting is all it does. Killing the thread (or the component's fiber)
-- that waits stops the wait, not the work: a promise cannot be cancelled,
-- and whatever started it goes on to the end. A binding for an API that
-- takes an @AbortSignal@ (@fetch@ does) should take one itself and hand it
-- on, so that the program can abort the work as well as stop waiting.
--
-- Any number of waits on the same promise each get what it settled with.
-- Natively it gives @'JSError' "NotInBrowserError" _@ at once.
await :: (MonadIO m) => Promise a -> m (Either JSError a)
await p = liftIO (atomically p.wait)

-- | 'await', throwing a rejection as the 'JSError' it is.
awaitOrThrow :: (MonadIO m) => Promise a -> m a
awaitOrThrow p = await p >>= either (liftIO . throwIO) pure

-- | Wait for every promise and give each one's outcome, in order. Their work
-- runs at once, so this takes as long as the slowest.
awaitAll :: (MonadIO m) => [Promise a] -> m [Either JSError a]
awaitAll = traverse await

-- | Wait for whichever promise settles first, and give its outcome. With no
-- promises it waits for ever.
awaitAny :: (MonadIO m) => [Promise a] -> m (Either JSError a)
awaitAny ps = liftIO (atomically (foldr (orElse . (.wait)) retry ps))
