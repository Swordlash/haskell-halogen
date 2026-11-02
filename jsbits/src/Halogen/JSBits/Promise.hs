{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A JavaScript promise, kept to be awaited later.
--
-- A binding that starts asynchronous work (a request, a timer, a dialog)
-- returns a 'Promise' at once, as an 'Unsafe' binding: the JavaScript has
-- started the work by the time it returns, and nothing has waited.
--
-- > $(browserJS ["jsbits/net.js"]
-- >   [("js_fetch_text", "halogen_net_fetch_text", Unsafe, [t| JSText -> IO (Promise Text) |])])
--
-- 'await' is the one place that waits for it, on every backend the same
-- way, and gives a rejection back as a value. That separates starting the
-- work from waiting for it, which a 'Safe' binding ties together. A
-- component can start work in @H.liftEffect@, inside the dispatch of the
-- event that asked for it, and keep the 'Promise' in its state, then 'await'
-- it in @lift@ (on a worker, while the rest of the tree goes on), or never.
-- It can start several and 'awaitAll' of them at once.
--
-- A 'Promise' says the type of what it resolves with, and 'await' reads the
-- resolved value as that type ('FromJS'). @'Promise' 'JSVal'@ leaves the
-- decoding to the binding's wrapper.
--
-- A module declaring such a binding imports @Promise (..)@: the FFI passes a
-- newtype only when its constructor is in scope.
--
-- A promise that is rejected and never awaited is reported by the engine as
-- an unhandled rejection, which a browser logs and Node ends its process
-- for, so the JavaScript of a binding whose result may be dropped, and whose
-- failure does not matter then, should handle the rejection itself.
--
-- This module declares its own bindings with 'browserJS', so it sits above
-- "Halogen.JSBits" rather than being re-exported from it; import both.
module Halogen.JSBits.Promise
  ( Promise (..)
  , FromJS (..)
  , JSError (..)
  , awaitPromise
  , await
  , awaitOrThrow
  , awaitAll
  , checkThrown
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (throwIO, try)
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.JSBits.Value (Callback, Inert, JSError (..), JSVal, checkThrown, freeCallback, fromJSBytes, inBrowser, jsValText, mkCallback)

-- | A JavaScript promise of a value of type @a@, or any value, which then
-- stands for a promise that has resolved with it. The type says what 'await'
-- reads the resolved value as ('FromJS'); @'Promise' 'JSVal'@ leaves it as
-- it is, @'Promise' ()@ ignores it. Natively, where no engine makes one, a
-- placeholder, which 'await' never waits for.
--
-- A binding returns one from a synchronous import, at once
-- (@[t|Text -> IO (Promise Text)|]@), and the program awaits it where it
-- wants the value.
newtype Promise a = Promise JSVal
  deriving newtype (Inert)

-- A wait is a pair of callbacks handed to the promise's @then@ ('awaitPromise').
-- A rejection reaches the second as @{ ok: false, error }@, the outcome
-- 'checkThrown' reads, so its name and message are read as a synchronous
-- call's exception is.
$( browserJS
     ["jsbits/promise.js"]
     [ ("js_then", "halogen_promise_then", Unchecked, [t|JSVal -> Callback -> Callback -> IO ()|])
     , ("js_as_int", "halogen_promise_number", Unchecked, [t|JSVal -> IO Int|])
     , ("js_as_double", "halogen_promise_number", Unchecked, [t|JSVal -> IO Double|])
     , ("js_as_bool", "halogen_promise_boolean", Unchecked, [t|JSVal -> IO Bool|])
     , ("js_absent", "halogen_promise_absent", Unchecked, [t|JSVal -> IO Bool|])
     , ("js_array_length", "halogen_promise_array_length", Unchecked, [t|JSVal -> IO Int|])
     , ("js_array_index", "halogen_promise_array_index", Unchecked, [t|JSVal -> Int -> IO JSVal|])
     ]
 )

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

-- | A @Uint8Array@ (or an @ArrayBuffer@'s bytes in one), copied out.
instance FromJS ByteString where fromJS = fromJSBytes

-- | A number, as an 'Int' the way the FFI converts one.
instance FromJS Int where fromJS = js_as_int

instance FromJS Double where fromJS = js_as_double

instance FromJS Bool where fromJS = js_as_bool

-- | An array, each element read as @a@. Read through the array's length and
-- its elements by index, the same on both browser backends: the JavaScript
-- backend's @fromJSArray@ has no WebAssembly counterpart.
instance (FromJS a) => FromJS [a] where
  fromJS v = do
    count <- js_array_length v
    traverse (fromJS <=< js_array_index v) [0 .. count - 1]

-- | @null@ or @undefined@ as 'Nothing'.
instance (FromJS a) => FromJS (Maybe a) where
  fromJS v = do
    absent <- js_absent v
    if absent then pure Nothing else Just <$> fromJS v

-- | Hand the promise two actions: the first runs with what it resolves
-- with, read as @a@, the second with what it is rejected with. Exactly one
-- runs, once, on a thread of its own, after the call returns (a promise's
-- callbacks always run later). The callbacks are freed when one of them has
-- run, so nothing is left behind once the promise settles; a promise that
-- never settles keeps its two.
--
-- Natively, where there is no promise, the second runs at once with
-- @'JSError' "NotInBrowserError" _@.
awaitPromise :: (FromJS a) => (a -> IO ()) -> (JSError -> IO ()) -> Promise a -> IO ()
awaitPromise onResolve onReject (Promise promise)
  | not inBrowser = onReject notInBrowser
  | otherwise = do
      made <- newEmptyMVar
      let done = readMVar made >>= \(resolve, reject) -> freeCallback resolve >> freeCallback reject
      resolve <- mkCallback $ \value -> done >> (fromJS value >>= onResolve)
      reject <- mkCallback $ \outcome -> do
        done
        try (checkThrown outcome) >>= \case
          Left e -> onReject e
          Right () -> onReject JSError {name = "Error", message = "rejected"}
      putMVar made (resolve, reject)
      js_then promise resolve reject

-- | Wait for the promise to settle, and give what it resolved with, or what
-- it was rejected with. It never throws a rejection, on any backend.
--
-- It is 'awaitPromise' with an 'MVar': the calling thread sleeps on it, and
-- the runtime goes on with the rest of the program and gives the page back
-- to the browser meanwhile. So it waits only where a thread may wait: in
-- @lift@, not in @H.liftEffect@.
--
-- Waiting is all it does. Killing the thread (or the component's fiber)
-- that waits stops the wait, not the work: a promise cannot be cancelled,
-- and whatever started it goes on to the end, its outcome unread. A binding
-- for an API that takes an @AbortSignal@ (@fetch@ does) should take one
-- itself and hand it on, so that the program can abort the work as well as
-- stop waiting for it.
--
-- Any number of waits on the same promise each get what it settled with,
-- and so does one that starts after it has settled.
--
-- Natively it waits for nothing and gives @'JSError' "NotInBrowserError" _@.
await :: (MonadIO m, FromJS a) => Promise a -> m (Either JSError a)
await promise = liftIO $ do
  box <- newEmptyMVar
  awaitPromise (void . tryPutMVar box . Right) (void . tryPutMVar box . Left) promise
  takeMVar box

-- | 'await', throwing a rejection as the 'JSError' it is.
awaitOrThrow :: (MonadIO m, FromJS a) => Promise a -> m a
awaitOrThrow promise = await promise >>= either (liftIO . throwIO) pure

-- | Wait for every promise and give each one's outcome, in order, as 'await'
-- would. The promises' work runs at once whatever the order of the waits,
-- so waiting for one after another takes as long as the slowest.
awaitAll :: (MonadIO m, FromJS a) => [Promise a] -> m [Either JSError a]
awaitAll = traverse await

notInBrowser :: JSError
notInBrowser = JSError {name = "NotInBrowserError", message = "there is no JavaScript engine here to run a promise"}
