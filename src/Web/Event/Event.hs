module Web.Event.Event
  ( EventType (..)
  , Event (..)
  , EventTarget (..)
  , currentTarget
  )
where

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Data.Foreign
#endif 

import HPrelude
import Web.Event.Internal.Types

newtype EventType = EventType Text

currentTarget :: Event -> Maybe EventTarget
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "js_current_target" js_current_target :: Event -> Nullable EventTarget
currentTarget e = EventTarget <$> nullableToMaybe (js_current_target e)
#else
currentTarget _ = panic "currentTarget: not available in GHC" -- TODO
#endif
