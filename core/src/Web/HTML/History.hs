{-# LANGUAGE TemplateHaskell #-}

-- | The session history, @window.history@: the entries Back and Forward move
-- between. Meant to be imported qualified (@History.go@).
--
-- haskell-halogen-router keeps the route in the history itself; these are
-- for what a router does not do, such as keeping a number in an entry's
-- state.
module Web.HTML.History
  ( History (..)
  , length
  , state
  , go
  , back
  , forward
  , pushState
  , replaceState
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (length, state)
import Halogen.JSBits (Inert, JSVal)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)

-- | A window's history: @w.history@ ("Web.HTML.Window").
newtype History = History (Foreign History)
  deriving newtype (Inert)

$( jsFields
     ''History
     [ -- How many entries the session history has.
       Live "length" ''Int
     , -- The state the current entry was pushed or replaced with: @null@
       -- for an entry nobody gave one.
       Live "state" ''JSVal
     , -- Move that many entries, back for a negative number, as Back and
       -- Forward do; 0 reloads the page. The move happens later, with a
       -- @popstate@ event on the window.
       Method "go" [t|Int -> IO ()|]
     , Method "back" [t|IO ()|]
     , Method "forward" [t|IO ()|]
     , -- A new entry after the current one, with a state, a title browsers
       -- ignore, and an address, or 'Nothing' for the address the page is
       -- at. The browser throws, as a 'Halogen.JSBits.Promise.JSError',
       -- for a state it cannot copy (a function) or an address of another
       -- origin.
       Method "pushState" [t|JSVal -> Text -> Maybe Text -> IO ()|]
     , -- The current entry replaced, as with 'pushState'.
       Method "replaceState" [t|JSVal -> Text -> Maybe Text -> IO ()|]
     ]
 )

$(jsFieldFunctions ''History [])
