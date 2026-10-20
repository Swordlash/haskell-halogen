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
  , Callback
  , mkCallback
  , mkSyncCallback
  , freeCallback
  , invokeCallback
  , inBrowser
  , Inert (..)
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

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
mkSyncCallback :: (JSVal -> IO ()) -> IO Callback
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

foreign import javascript unsafe "(($1) => { return $1; })" js_to_double :: Double -> JSVal

foreign import javascript unsafe "(($1) => { return $1; })" js_to_bool :: Bool -> JSVal

type Callback = JS.Callback (JSVal -> IO ())

mkCallback = JS.asyncCallback1
mkSyncCallback = JS.syncCallback1 JS.ContinueAsync
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

type Callback = JSVal

mkCallback = js_callback

foreign import javascript "wrapper" js_callback :: (JSVal -> IO ()) -> IO Callback

mkSyncCallback f = js_sync_callback (continueAsync . f)

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

newtype Callback = Callback (JSVal -> IO ())

mkCallback = pure . Callback
mkSyncCallback = mkCallback
freeCallback _ = pure ()
invokeCallback (Callback f) = f

instance Inert JSVal where inert = JSVal (unsafeCoerce ())
#endif
