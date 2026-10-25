{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | The values a binding passes, under one name on every backend. This is
-- where the backends differ, so that the modules declaring bindings need not:
-- a 'JSVal' is the engine's value in a browser and an opaque placeholder
-- natively, a 'JSText' is a 'JSVal' holding a string in a browser and 'Text'
-- natively.
module Halogen.JSBits.Value
  ( JSVal
  , JSText
  , toJSText
  , fromJSText
  , jsValText
  , isNull
  , jsNull
  , toJSInt
  , toJSDouble
  , toJSBool
  , toJSBytes
  , fromJSBytes
  , Callback
  , mkCallback
  , mkSyncCallback
  , inSyncCallback
  , freeCallback
  , invokeCallback
  , inBrowser
  , Inert (..)
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (finally, mask)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO.Unsafe (unsafePerformIO)

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Data.ByteString.Internal qualified as ByteString (create)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
#endif
#if defined(javascript_HOST_ARCH)
import GHC.JS.Foreign.Callback qualified as JS
import GHC.JS.Prim (JSVal, fromJSString, toJSString)
import GHC.JS.Prim qualified as JS
import Unsafe.Coerce (unsafeCoerce)
#elif defined(wasm32_HOST_ARCH)
import Control.Concurrent (forkIO, yield)
import Control.Exception (evaluate)
import GHC.Conc (ThreadStatus (..), threadStatus)
import GHC.Wasm.Prim (JSString (..), JSVal, freeJSVal, fromJSString, toJSString)
#else
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Whether the program runs in a JavaScript engine. Natively every binding
-- is an 'inert' stub, and the few wrappers that must do something else there
-- ask this.
inBrowser :: Bool

-- | A string as a binding passes it.
toJSText :: Text -> JSText
fromJSText :: JSText -> Text

-- | A value known to be a string, as text. Natively empty.
jsValText :: JSVal -> Text

-- | Whether the value is @null@. Natively, where nothing comes back from an
-- engine, every value is.
isNull :: JSVal -> Bool

-- | @null@; natively the placeholder.
jsNull :: JSVal

-- | A number or a boolean as JavaScript holds it, for a binding that takes
-- a value of any kind (a property's).
toJSInt :: Int -> JSVal
toJSDouble :: Double -> JSVal
toJSBool :: Bool -> JSVal

-- | Bytes as a @Uint8Array@ of their own, copied out of the Haskell heap, and
-- a @Uint8Array@ copied back into a 'ByteString'. A representation
-- conversion, so it is here: each backend keeps a 'ByteString' differently
-- (in the wasm instance's memory, or in a JavaScript array the JavaScript
-- backend allocates), and the only way to move many bytes at once is to know
-- which. Natively there is no array: 'toJSBytes' gives the placeholder and
-- 'fromJSBytes' no bytes.
toJSBytes :: ByteString -> IO JSVal
fromJSBytes :: JSVal -> IO ByteString

-- | A Haskell function JavaScript can call with one argument. Free it with
-- 'freeCallback' once nothing will call it again.
mkCallback :: (JSVal -> IO ()) -> IO Callback

-- | A callback that runs at once when called, so that a handler can still,
-- say, prevent an event's default: what it does before it first waits
-- happens during the call, and the rest afterwards, on a thread of its own.
-- On WebAssembly it is a sync export, which is what re-entering Haskell
-- from a synchronous JSFFI import (a component that dispatches an event
-- from a synchronous effect) needs; on the JavaScript backend a
-- @ContinueAsync@ callback.
--
-- While its body runs, its thread is one 'inSyncCallback' answers 'True'
-- for.
mkSyncCallback :: (JSVal -> IO ()) -> IO Callback

-- | Whether this thread is running the body of a callback made with
-- 'mkSyncCallback': one JavaScript called, perhaps from the middle of a
-- synchronous import this program made (a component that dispatches an
-- event from a synchronous effect). A thread the program forked itself, or
-- one an asynchronous callback ('mkCallback') runs on, is not, and neither
-- is one the body forks. A body that waits stays one until it ends, though
-- by then JavaScript has had the call back.
--
-- haskell-halogen-core asks it to tell an event the page dispatches, which
-- may arrive in the middle of the component's work that dispatched it, from
-- a notification any other thread makes.
inSyncCallback :: IO Bool
inSyncCallback = do
  me <- myThreadId
  any (sameThread me) <$> readIORef syncCallbackThreads

-- | The threads running a sync callback's body, one entry for each body
-- (natively a callback runs on its caller's thread, which may be in one
-- already).
syncCallbackThreads :: IORef [ThreadId]
syncCallbackThreads = unsafePerformIO (newIORef [])
{-# NOINLINE syncCallbackThreads #-}

-- | Run a sync callback's body with its thread noted in
-- 'syncCallbackThreads'.
asSyncCallback :: IO () -> IO ()
asSyncCallback body = mask $ \restore -> do
  me <- myThreadId
  atomicModifyIORef' syncCallbackThreads (\ts -> (me : ts, ()))
  restore body `finally` atomicModifyIORef' syncCallbackThreads (\ts -> (dropOne me ts, ()))
  where
    dropOne me = \case
      [] -> []
      t : ts
        | sameThread me t -> ts
        | otherwise -> t : dropOne me ts

-- | 'compare', not '==': GHC's JavaScript runtime has the @cmp_thread@ that
-- 'Ord' 'ThreadId' calls, but not the @eq_thread@ that 'Eq' calls.
sameThread :: ThreadId -> ThreadId -> Bool
sameThread a b = compare a b == EQ

freeCallback :: Callback -> IO ()

-- | Call a callback from Haskell.
invokeCallback :: Callback -> JSVal -> IO ()

-- | The value a native stub returns: nothing to report, nothing done.
class Inert a where
  inert :: a

instance Inert () where inert = ()

instance Inert Bool where inert = False

instance Inert Int where inert = 0

instance Inert Double where inert = 0

instance Inert Text where inert = Text.empty

instance Inert (Maybe a) where inert = Nothing

instance (Inert b) => Inert (a -> b) where inert = const inert

instance (Inert a) => Inert (IO a) where inert = pure inert

#if defined(javascript_HOST_ARCH)
inBrowser = True

type JSText = JSVal

toJSText = toJSString . Text.unpack
fromJSText = Text.pack . fromJSString
jsValText = fromJSText
isNull = JS.isNull
jsNull = JS.jsNull
toJSInt = JS.toJSInt
toJSDouble = js_to_double
toJSBool = js_to_bool

-- An 'Addr#', and so a 'Ptr', arrives as two values: the byte array the
-- JavaScript backend allocated, whose @u8@ is a @Uint8Array@ over all of it,
-- and the offset into it. An empty 'ByteString' may point nowhere, which is
-- why a length of 0 does not look at the array.
toJSBytes bytes = unsafeUseAsCStringLen bytes $ \(ptr, len) -> js_bytes_copy (castPtr ptr) len
fromJSBytes array = do
  len <- js_bytes_length array
  if len == 0 then pure ByteString.empty else ByteString.create len (js_bytes_into array)

foreign import javascript unsafe "((buffer, offset, length) => { return length === 0 ? new Uint8Array(0) : buffer.u8.slice(offset, offset + length); })"
  js_bytes_copy :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "((array) => { return array.length; })" js_bytes_length :: JSVal -> IO Int

foreign import javascript unsafe "((array, buffer, offset) => { buffer.u8.set(array, offset); })"
  js_bytes_into :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe "(($1) => { return $1; })" js_to_double :: Double -> JSVal

foreign import javascript unsafe "(($1) => { return $1; })" js_to_bool :: Bool -> JSVal

type Callback = JS.Callback (JSVal -> IO ())

mkCallback = JS.asyncCallback1
mkSyncCallback f = JS.syncCallback1 JS.ContinueAsync (asSyncCallback . f)
freeCallback = JS.releaseCallback
invokeCallback callback = js_invoke (unsafeCoerce callback)

foreign import javascript unsafe "(($1, $2) => { $1($2); })" js_invoke :: JSVal -> JSVal -> IO ()

instance Inert JSVal where inert = JS.jsNull
#elif defined(wasm32_HOST_ARCH)
inBrowser = True

-- A JSString does not cross this toolchain's JSFFI: the value inside it does.
type JSText = JSVal

toJSText value = case toJSString (Text.unpack value) of JSString string -> string
fromJSText = Text.pack . fromJSString . JSString
jsValText = fromJSText

isNull = js_is_null
jsNull = js_null
toJSInt = js_to_int
toJSDouble = js_to_double
toJSBool = js_to_bool

foreign import javascript unsafe "$1 === null" js_is_null :: JSVal -> Bool

foreign import javascript unsafe "$1" js_to_int :: Int -> JSVal

foreign import javascript unsafe "$1" js_to_double :: Double -> JSVal

foreign import javascript unsafe "$1" js_to_bool :: Bool -> JSVal

-- A 'ByteString' is in the instance's memory, pinned, so its address is an
-- offset into @memory.buffer@. The view is made afresh in each call, because
-- the buffer is replaced whenever the memory grows; and nothing allocates
-- while an unsafe import runs, so the address holds for the copy.
toJSBytes bytes = unsafeUseAsCStringLen bytes $ \(ptr, len) -> js_bytes_copy (castPtr ptr) len
fromJSBytes array = do
  len <- js_bytes_length array
  if len == 0 then pure ByteString.empty else ByteString.create len (\ptr -> js_bytes_into array ptr len)

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $1 >>> 0, $2).slice()"
  js_bytes_copy :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "$1.length" js_bytes_length :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2 >>> 0, $3).set($1)"
  js_bytes_into :: JSVal -> Ptr Word8 -> Int -> IO ()

type Callback = JSVal

mkCallback = js_callback

foreign import javascript "wrapper" js_callback :: (JSVal -> IO ()) -> IO Callback

mkSyncCallback f = js_sync_callback (continueAsync . asSyncCallback . f)

-- A sync export: the browser runs listeners while it dispatches an event,
-- and re-entering Haskell from a synchronous JSFFI import is only
-- supported for sync exports.
foreign import javascript "wrapper sync" js_sync_callback :: (JSVal -> IO ()) -> IO Callback

-- | Run a callback on a thread of its own until it ends or waits, and
-- return to JavaScript then: what it does before it first waits happens
-- during the call ('preventDefault' counts), and the rest afterwards. A
-- sync export itself must not wait for the page's event loop, which cannot
-- run until it returns.
--
-- What the callback leaves to run (its own rest, the threads it started)
-- needs the scheduler to run again after the export has returned, which a
-- sync export does not arrange by itself: a thread waiting for an async
-- import brings the runtime back once the promise settles, and the
-- scheduler then runs whatever is runnable.
continueAsync :: IO () -> IO ()
continueAsync io = do
  t <- forkIO io
  resume <- forkIO (evaluate =<< js_resume_later)
  untilWaiting t
  untilWaiting resume
  where
    untilWaiting t =
      threadStatus t >>= \case
        ThreadRunning -> yield >> untilWaiting t
        _ -> pure ()

foreign import javascript safe "undefined" js_resume_later :: IO ()

freeCallback = freeJSVal

invokeCallback = js_invoke

foreign import javascript unsafe "$1($2)" js_invoke :: Callback -> JSVal -> IO ()

foreign import javascript unsafe "null" js_null :: JSVal

instance Inert JSVal where inert = js_null
#else
inBrowser = False

-- | Natively a value no engine made: a placeholder, passed around and never
-- read.
newtype JSVal = JSVal Any

type JSText = Text

toJSText = id
fromJSText = id
jsValText _ = Text.empty
isNull _ = True
jsNull = JSVal (unsafeCoerce ())
toJSInt _ = jsNull
toJSDouble _ = jsNull
toJSBool _ = jsNull
toJSBytes _ = pure jsNull
fromJSBytes _ = pure ByteString.empty

newtype Callback = Callback (JSVal -> IO ())

mkCallback = pure . Callback
mkSyncCallback f = mkCallback (asSyncCallback . f)
freeCallback _ = pure ()
invokeCallback (Callback f) = f

instance Inert JSVal where inert = JSVal (unsafeCoerce ())
#endif
