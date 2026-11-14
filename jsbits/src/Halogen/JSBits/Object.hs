{-# LANGUAGE NoFieldSelectors #-}

-- | What a value does for itself, natively, when a member of it is used:
-- see 'Halogen.JSBits.Value.answering'.
module Halogen.JSBits.Object
  ( NativeObject (..)
  )
where

import Data.Text (Text)

-- | What a value answers natively, where there is no engine to ask, when the
-- program uses a member of it through "Halogen.JSBits.Fields": a backend
-- that is not a browser makes its handles of these, so that what a program
-- does to the page through the @Web.*@ types reaches the backend instead of
-- doing nothing (a remote client's page, a list of calls a test reads).
--
-- Only what can be answered at once is asked: a method's call or a
-- property's assignment, which the object may carry out or record, a
-- read of a member that is an object the backend already has (the window's
-- @document@), and the class the value is an instance of, for a checked
-- cast ('Halogen.JSBits.Fields.jsCast'). A read of anything else, and a
-- method that returns a value, keep their inert answer.
--
-- @v@ is 'Halogen.JSBits.Value.JSVal'; the type is a parameter only so that
-- this module can be below the one that defines it.
data NativeObject v = NativeObject
  { call :: Text -> [v] -> IO ()
  -- ^ A method that returns nothing, by its JavaScript name, with its
  -- arguments.
  , set :: Text -> v -> IO ()
  -- ^ An assignment to a property, by its JavaScript name.
  , get :: Text -> Maybe v
  -- ^ A member that is an object and does not change while the value
  -- lives ('Halogen.JSBits.Fields.Fixed'), if the object has it: the
  -- window's @document@, say. 'Nothing' gives the member's inert answer.
  , instanceOf :: Text -> Bool
  -- ^ Whether the value is an instance of a JavaScript class, by its name
  -- (@\"HTMLDialogElement\"@): what a checked cast asks.
  }
