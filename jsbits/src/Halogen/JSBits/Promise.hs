{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InterruptibleFFI #-}
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
  , await
  , awaitOrThrow
  , awaitAll
  , checkThrown
  )
where

import Control.Exception (evaluate, throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.JSBits.Value (Inert, JSError (..), JSText, JSVal, checkThrown, fromJSBytes, fromJSText, inBrowser, jsValText)

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

-- The outcome of a wait is an object of jsbits/promise.js's, holding the
-- promise until it settles and then what it settled with: the one awaited
-- binding on the JavaScript backend may only return nothing (see
-- 'browserJS'), so what it resolved with is read afterwards.
$( browserJS
     ["jsbits/promise.js"]
     [ ("js_outcome", "halogen_promise_outcome", Unchecked, [t|JSVal -> IO JSVal|])
     , ("js_settle", "halogen_promise_settle", Safe, [t|JSVal -> IO ()|])
     , ("js_outcomes", "halogen_promise_outcomes", Unchecked, [t|IO JSVal|])
     , ("js_push", "halogen_promise_push", Unchecked, [t|JSVal -> JSVal -> IO ()|])
     , ("js_settle_all", "halogen_promise_settle_all", Safe, [t|JSVal -> IO ()|])
     , ("js_ok", "halogen_promise_ok", Unchecked, [t|JSVal -> IO Bool|])
     , ("js_value", "halogen_promise_value", Unchecked, [t|JSVal -> IO JSVal|])
     , ("js_name", "halogen_promise_name", Unchecked, [t|JSVal -> IO JSText|])
     , ("js_message", "halogen_promise_message", Unchecked, [t|JSVal -> IO JSText|])
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

-- | Wait for the promise to settle, and give what it resolved with, or what
-- it was rejected with. It never throws a rejection, on any backend.
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
await (Promise promise)
  | not inBrowser = pure (Left notInBrowser)
  | otherwise = liftIO $ do
      outcome <- js_outcome promise
      settle (js_settle outcome)
      outcomeOf outcome

-- | 'await', throwing a rejection as the 'JSError' it is.
awaitOrThrow :: (MonadIO m, FromJS a) => Promise a -> m a
awaitOrThrow promise = await promise >>= either (liftIO . throwIO) pure

-- | Wait for every promise to settle, with one wait for all of them rather
-- than one after another, and give each one's outcome, in order, as 'await'
-- would.
awaitAll :: (MonadIO m, FromJS a) => [Promise a] -> m [Either JSError a]
awaitAll promises
  | not inBrowser = pure (map (const (Left notInBrowser)) promises)
  | otherwise = liftIO $ do
      outcomes <- js_outcomes
      each <- traverse (\(Promise p) -> js_outcome p >>= \o -> o <$ js_push outcomes o) promises
      settle (js_settle_all outcomes)
      traverse outcomeOf each

-- An awaited import on WebAssembly returns a thunk, and the thread waits for
-- the promise only when it is forced; forcing it here means no caller has
-- to. On the JavaScript backend the import is interruptible and has waited
-- by the time it returns.
settle :: IO () -> IO ()
settle wait = wait >>= evaluate

outcomeOf :: (FromJS a) => JSVal -> IO (Either JSError a)
outcomeOf outcome = do
  ok <- js_ok outcome
  if ok
    then Right <$> (fromJS =<< js_value outcome)
    else do
      name <- fromJSText <$> js_name outcome
      message <- fromJSText <$> js_message outcome
      pure (Left JSError {name, message})

notInBrowser :: JSError
notInBrowser = JSError {name = "NotInBrowserError", message = "there is no JavaScript engine here to run a promise"}
