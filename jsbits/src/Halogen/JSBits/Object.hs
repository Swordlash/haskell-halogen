{-# LANGUAGE NoFieldSelectors #-}

-- | What a value does for itself, natively, when a member of it is used:
-- see 'Halogen.JSBits.Value.withNativeObject'.
module Halogen.JSBits.Object
  ( NativeObject (..)
  )
where

import Data.Text (Text)

-- | What a value answers natively, where there is no engine to ask, when the
-- program uses a member of it through "Halogen.JSBits.Fields": a backend
-- that is not a browser makes its handles of these, so that what a program
-- does to the page through the @Web.*@ types reaches the backend instead of
-- doing nothing (a remote client's page, a list of calls a test reads), and
-- what it reads of the page is answered where the backend has the answer
-- (the in-memory DOM's tree).
--
-- Only what can be answered at once is asked: a method's call or a
-- property's assignment, which the object may carry out or record, a
-- method with a result, a read of a member that does not change and of one
-- that does, and the class the value is an instance of, for a checked cast
-- ('Halogen.JSBits.Fields.jsCast'). Every question but the cast can be
-- declined with 'Nothing', and a declined read or method keeps its inert
-- answer: an object answers what it knows, and a backend whose page is
-- elsewhere (haskell-halogen-remote's) declines what it would have to ask
-- the page.
--
-- Natively "Data.Foreign"'s @readProp@ asks 'live' too, so a property a
-- handler reads of an element (@currentTarget.value@) is answered as the
-- field would be.
--
-- @v@ is 'Halogen.JSBits.Value.JSVal'; the type is a parameter only so that
-- this module can be below the one that defines it.
data NativeObject v = NativeObject
  { call :: Text -> [v] -> IO ()
  -- ^ A method that returns nothing, by its JavaScript name, with its
  -- arguments.
  , invoke :: Text -> [v] -> IO (Maybe v)
  -- ^ A method that returns a value (a text, a boolean, a number, an
  -- object, or 'Nothing''s @null@, 'Halogen.JSBits.Value.jsNull'), by its
  -- JavaScript name, with its arguments: @getAttribute@, @closest@,
  -- @contains@. 'Nothing' declines, and the method gives its inert answer.
  -- A method that returns a Promise or bytes is not asked.
  , set :: Text -> v -> IO ()
  -- ^ An assignment to a property, by its JavaScript name.
  , get :: Text -> Maybe v
  -- ^ A member that does not change while the value lives
  -- ('Halogen.JSBits.Fields.Fixed'), if the object has it: the window's
  -- @document@, an event's @type@. 'Nothing' gives the member's inert
  -- answer.
  , live :: Text -> IO (Maybe v)
  -- ^ A member that changes ('Halogen.JSBits.Fields.Live', or a
  -- 'Halogen.JSBits.Fields.Writable' one's read), if the object keeps its
  -- value: a text, a boolean, a number, an object, or 'Nothing''s @null@
  -- ('Halogen.JSBits.Value.jsNull'). 'Nothing' gives the member's inert
  -- answer, as for every read the object does not keep.
  , instanceOf :: Text -> Bool
  -- ^ Whether the value is an instance of a JavaScript class, by its name
  -- (@\"HTMLDialogElement\"@): what a checked cast asks.
  }
