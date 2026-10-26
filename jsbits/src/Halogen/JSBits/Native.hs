-- | What a value is natively, where no engine makes values: compiled only
-- natively, for the code there that builds a value or takes one apart
-- (haskell-halogen-core's "Data.Foreign", which reads an event made from
-- JSON, and its in-memory DOM, which keeps its nodes and listeners in their
-- handles' values). "Halogen.JSBits.Value" exports the two types, without
-- their constructors, on every backend.
module Halogen.JSBits.Native
  ( JSVal (..)
  , Callback (..)
  )
where

import Data.Dynamic (Dynamic)

-- | A value is whatever Haskell made it from, kept with its type: a number
-- from 'Halogen.JSBits.Value.toJSInt', the text a stub was given, a node of
-- the in-memory DOM, or the JSON object a test or a remote client sent as an
-- event. Reading one ("Data.Foreign", 'Halogen.JSBits.Value.jsValText')
-- looks for the type it wants and, when the value holds something else,
-- gives the answer a stub would, so nothing ever reads a value as what it
-- does not hold.
newtype JSVal = JSVal Dynamic

-- | A callback is a value holding its function, as on WebAssembly it is the
-- JavaScript function: a value, so that what it holds can also be something
-- a backend made for itself (the in-memory DOM's listeners, which carry
-- their identity), in which 'Halogen.JSBits.Value.invokeCallback' then
-- finds no function and does nothing.
newtype Callback = Callback JSVal
