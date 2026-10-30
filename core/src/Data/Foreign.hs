{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Data.Foreign where

import GHC.Base (isTrue#, reallyUnsafePtrEquality, reallyUnsafePtrEquality#)
import HPrelude

#if defined(javascript_HOST_ARCH) && !defined(HALOGEN_WASI)
import GHC.Base (Int#)
import GHC.JS.Prim hiding (JSVal)
import Halogen.JSBits (Inert (..), JSVal)
import Unsafe.Coerce (unsafeCoerce)

type Foreign tag = JSVal

newtype Nullable tag = Nullable (Foreign tag)

instance Inert (Nullable tag) where inert = Nullable inert

foreign import javascript unsafe "(($1) => { return (!!$1 ? 1 : 0); })" foreignToBool' :: Foreign tag -> Int#

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe (Nullable o) = if isNull o || isUndefined o then Nothing else Just o

toForeign :: a -> Foreign tag
toForeign = unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign = unsafeCoerce

stringToForeign :: Text -> Foreign tag
stringToForeign = toJSString . toS

readProp :: Text -> (Foreign tag -> Maybe a) -> Foreign tag' -> Maybe a
readProp key f o = nullableToMaybe (Nullable $ unsafeGetProp o (toS key)) >>= f

foreignToString :: Foreign tag -> Text
foreignToString = toS . fromJSString

foreignToInt :: Foreign tag -> Int
foreignToInt = fromJSInt

foreignToBool :: Foreign tag -> Bool
foreignToBool x = isTrue# (foreignToBool' x)

foreign import javascript unsafe "(($1) => { return +$1; })"
  foreignToNumber :: Foreign tag -> Double
#elif defined(wasm32_HOST_ARCH) && !defined(HALOGEN_WASI)
import GHC.Wasm.Prim hiding (JSVal)
import Halogen.JSBits (Inert (..), JSVal)
import Unsafe.Coerce (unsafeCoerce)

type Foreign tag = JSVal

newtype Nullable tag = Nullable (Foreign tag)

instance Inert (Nullable tag) where inert = Nullable inert

foreign import javascript unsafe "$1 == null"
  foreignIsNullish :: Foreign tag -> Bool

foreign import javascript unsafe "$1[$2]"
  unsafeGetProp :: Foreign tag -> JSString -> Foreign tag'

foreign import javascript unsafe "$1"
  foreignToInt :: Foreign tag -> Int

foreign import javascript unsafe "$1"
  foreignToBool :: Foreign tag -> Bool

foreign import javascript unsafe "+$1"
  foreignToNumber :: Foreign tag -> Double

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe (Nullable o)
  | foreignIsNullish o = Nothing
  | otherwise = Just o

-- These coercions are retained for compatibility with the JavaScript backend
-- API. They are representation-unsafe on wasm; typed conversion functions
-- such as 'foreignToString' should be preferred.
toForeign :: a -> Foreign tag
toForeign = unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign = unsafeCoerce

stringToForeign :: Text -> Foreign tag
stringToForeign value = case toJSString (toS value) of
  JSString result -> result

readProp :: Text -> (Foreign tag -> Maybe a) -> Foreign tag' -> Maybe a
readProp key f o = f $ unsafeGetProp o (toJSString $ toS key)

foreignToString :: Foreign tag -> Text
foreignToString = toS . fromJSString . JSString
#else
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Control.Exception (throw)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Scientific (toRealFloat)
import Halogen.JSBits.Native (JSVal (..))

-- | What an engine would hold. Natively a 'JSVal' is a 'Data.Dynamic.Dynamic'
-- holding what the value was made from ("Halogen.JSBits.Value"), so
-- 'toForeign' and 'unsafeFromForeign' ask for 'Typeable' here, which the
-- browsers' coercions do not need.
--
-- A value made from JSON (an aeson 'Aeson.Value', as an event a test fires
-- or a remote client sends is) has properties: 'readProp' reads an
-- object's, and the readers below read its scalars, so a handler that reads
-- the event ('Halogen.HTML.Events.onValueInput' and the rest) reads it
-- natively as in a browser. Whatever else a value holds has no properties,
-- and reads as the stubs' answers: 'Nothing', empty, 0, 'False'.
type Foreign tag = JSVal

type Nullable tag = Maybe (Foreign tag)

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe = identity

toForeign :: (Typeable a) => a -> Foreign tag
toForeign = JSVal . toDyn

-- | The value held, which must be of the type asked for: a value that holds
-- something else is a mistake in the program, and is reported as one (with
-- both types) rather than read as what it is not.
unsafeFromForeign :: forall a tag. (Typeable a) => Foreign tag -> a
unsafeFromForeign (JSVal held) = case fromDynamic held of
  Just value -> value
  Nothing ->
    throw
      $ ErrorCall
      $ "Data.Foreign.unsafeFromForeign: the value holds "
      <> show held
      <> ", not "
      <> show (typeRep @a)

stringToForeign :: Text -> Foreign tag
stringToForeign = toForeign

-- | A property of a JSON object the value holds, as JavaScript's @o[key]@
-- reads it, with @null@ read as no property, as the JavaScript backend
-- reads it. A value that is not an object has none.
readProp :: Text -> (Foreign tag -> Maybe a) -> Foreign tag' -> Maybe a
readProp key f (JSVal held) = case fromDynamic held of
  Just (Aeson.Object fields) -> case KeyMap.lookup (Key.fromText key) fields of
    Nothing -> Nothing
    Just Aeson.Null -> Nothing
    Just field -> f (toForeign field)
  _ -> Nothing

-- | The text the value holds, or the JSON string; empty for anything else.
foreignToString :: Foreign tag -> Text
foreignToString (JSVal held)
  | Just text <- fromDynamic held = text
  | Just (Aeson.String text) <- fromDynamic held = text
  | otherwise = ""

-- | The number the value holds, or the JSON number, truncated as
-- JavaScript's @|0@ truncates it; 0 for anything else.
foreignToInt :: Foreign tag -> Int
foreignToInt (JSVal held)
  | Just int <- fromDynamic held = int
  | Just (Aeson.Number number) <- fromDynamic held = truncate number
  | otherwise = 0

-- | The boolean the value holds, or the JSON one; 'False' for anything else.
foreignToBool :: Foreign tag -> Bool
foreignToBool (JSVal held)
  | Just b <- fromDynamic held = b
  | Just (Aeson.Bool b) <- fromDynamic held = b
  | otherwise = False

-- | The number the value holds (a 'Double' or an 'Int'), or the JSON
-- number, as JavaScript's numbers are all doubles; 0 for anything else.
foreignToNumber :: Foreign tag -> Double
foreignToNumber (JSVal held)
  | Just double <- fromDynamic held = double
  | Just int <- fromDynamic held = fromIntegral (int :: Int)
  | Just (Aeson.Number number) <- fromDynamic held = toRealFloat number
  | otherwise = 0
#endif

unsafeRefEq :: a -> a -> Bool
unsafeRefEq p q = isTrue# (reallyUnsafePtrEquality p q)

unsafeRefEq' :: a -> b -> Bool
unsafeRefEq' p q = isTrue# (reallyUnsafePtrEquality# p q)
