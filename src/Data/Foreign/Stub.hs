module Data.Foreign.Stub (module Data.Foreign.Stub) where

import HPrelude

newtype Foreign tag = Foreign Any

type Nullable tag = Maybe (Foreign tag)

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe = identity

toForeign :: a -> Foreign tag
toForeign = Foreign . unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign (Foreign o) = unsafeCoerce o

readProp :: Text -> (Foreign tag -> Maybe a) -> Foreign tag' -> Maybe a
readProp = panic "Unavailable in GHC" -- TODO

foreignToString :: Foreign tag -> Text
foreignToString = unsafeCoerce

foreignToInt :: Foreign tag -> Int
foreignToInt = unsafeCoerce

foreignToBool :: Foreign tag -> Bool
foreignToBool = unsafeFromForeign