{-# LANGUAGE CPP #-}

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
import GHC.JS.Prim (JSVal, fromJSString, jsNull, toJSString)
import GHC.JS.Prim qualified as JS
import Unsafe.Coerce (unsafeCoerce)
#elif defined(wasm32_HOST_ARCH)
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

-- | A Haskell function JavaScript can call with one argument. Free it with
-- 'freeCallback' once nothing will call it again.
mkCallback :: (JSVal -> IO ()) -> IO Callback

-- | A callback that runs at once when called, so that a handler can still,
-- say, prevent an event's default. On WebAssembly it is 'mkCallback'.
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

type Callback = JS.Callback (JSVal -> IO ())

mkCallback = JS.asyncCallback1
mkSyncCallback = JS.syncCallback1 JS.ContinueAsync
freeCallback = JS.releaseCallback
invokeCallback callback = js_invoke (unsafeCoerce callback)

foreign import javascript unsafe "(($1, $2) => { $1($2); })" js_invoke :: JSVal -> JSVal -> IO ()

instance Inert JSVal where inert = jsNull
#elif defined(wasm32_HOST_ARCH)
inBrowser = True

-- A JSString does not cross this toolchain's JSFFI: the value inside it does.
type JSText = JSVal

toJSText value = case toJSString (Text.unpack value) of JSString string -> string
fromJSText = Text.pack . fromJSString . JSString
jsValText = fromJSText

isNull = js_is_null

foreign import javascript unsafe "$1 === null" js_is_null :: JSVal -> Bool

type Callback = JSVal

mkCallback = js_callback

foreign import javascript "wrapper" js_callback :: (JSVal -> IO ()) -> IO Callback

mkSyncCallback = mkCallback
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

newtype Callback = Callback (JSVal -> IO ())

mkCallback = pure . Callback
mkSyncCallback = mkCallback
freeCallback _ = pure ()
invokeCallback (Callback f) = f

instance Inert JSVal where inert = JSVal (unsafeCoerce ())
#endif
