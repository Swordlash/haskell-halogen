{-# LANGUAGE TemplateHaskell #-}

-- | What a handler declares about the events it hears, as data: what its
-- listener does to an event itself ('HandlerOptions'), and for which events
-- it makes an action at all ('EventMatch').
--
-- A handler's function is Haskell, which only the program can run. Where
-- the program runs in the page, its listener runs it while the event is
-- being dispatched, and can prevent the default of exactly the events it
-- makes an action for. Where the page is somewhere else
-- ("Halogen.Remote.Session" in haskell-halogen-remote), the page has
-- finished with an event before the program hears of it: what the page has
-- to decide while it dispatches the event has to be something the page can
-- read, and an 'EventMatch' is. The backend is told of a listener's
-- options, and of every change to them, through
-- 'Halogen.VDom.DOM.Monad.setListenerOptions'.
module Halogen.VDom.DOM.HandlerOptions
  ( HandlerOptions (..)
  , handlerOptions
  , EventMatch (..)
  , KeyMatch (..)
  , keyMatch
  , matchesEvent
  , isPlainLeftClick
  )
where

import Data.Foreign (foreignToBool, foreignToInt, foreignToString, readProp)
import Data.Text qualified as T
import HPrelude
import Halogen.JSBits (Safety (..), browserJS, inBrowser)
import Web.Event.Internal.Types (Event (..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent (..))
import Web.UIEvent.KeyboardEvent qualified as KE

$( browserJS
     ["jsbits/events.js"]
     [("js_plain_left_click", "halogen_plain_left_click", Unsafe, [t|Event -> Bool|])]
 )

-- | What a handler does to its event in the listener itself, before its
-- message goes anywhere, and which events it is asked about.
--
-- With no 'matching' declared, a handler is asked about every event of its
-- type, and 'preventDefault' and 'stopPropagation' apply to each event it
-- makes a message for (one that makes none leaves the event alone). With
-- one declared, it is asked only about the events that match, and the two
-- apply to every event that matches, whether or not it then makes a
-- message: a match is what lets a page that is not the program's decide in
-- time.
--
-- A message is handled by its component's program on the tree's loop
-- ("Halogen.IO.Driver.Runtime"), which runs one piece of work at a time: an
-- event that arrives while the loop is busy elsewhere is handled once the
-- loop gets to it, by when the browser has finished dispatching the event
-- and has, say, followed the link or submitted the form. What has to happen
-- while the event is being dispatched whatever the loop is doing belongs
-- here.
data HandlerOptions = HandlerOptions
  { preventDefault :: Bool
  -- ^ Keep the browser from taking the event's default action: following
  -- a link, submitting a form, typing a key.
  , stopPropagation :: Bool
  -- ^ Stop the event from reaching the listeners of the elements around.
  , matching :: Maybe EventMatch
  -- ^ The events the handler makes an action for, if it says.
  }
  deriving stock (Eq, Show)

-- | Neither, and no match declared: the event is left as it is.
handlerOptions :: HandlerOptions
handlerOptions = HandlerOptions {preventDefault = False, stopPropagation = False, matching = Nothing}

-- | For which events a handler makes an action, said as data that a page
-- can read without the program.
data EventMatch
  = -- | Every event of the handler's type.
    AnyEvent
  | -- | A click the browser would follow as it is, in this tab
    -- ('isPlainLeftClick'): a router's link's.
    PlainLeftClick
  | -- | A keyboard event whose key is one of these.
    Keys [KeyMatch]
  deriving stock (Eq, Show)

-- | A key, as the event's @key@ names it (@"\/"@, @"ArrowLeft"@,
-- @"Escape"@, @"s"@), and the modifiers it is pressed with: 'Nothing' for
-- either way, @'Just' 'True'@ for held, @'Just' 'False'@ for not held.
data KeyMatch = KeyMatch
  { key :: Text
  , ctrlKey :: Maybe Bool
  , shiftKey :: Maybe Bool
  , altKey :: Maybe Bool
  , metaKey :: Maybe Bool
  }
  deriving stock (Eq, Show)

-- | A key, with any modifiers or none.
--
-- > (keyMatch "s") {ctrlKey = Just True}
keyMatch :: Text -> KeyMatch
keyMatch key = KeyMatch {key, ctrlKey = Nothing, shiftKey = Nothing, altKey = Nothing, metaKey = Nothing}

-- | Whether an event matches, read as it is now: in the listener, while the
-- event is being dispatched. Natively it is read from the event's JSON, as
-- an event a remote client sends or a test makes carries it.
matchesEvent :: EventMatch -> Event -> Bool
matchesEvent match event = case match of
  AnyEvent -> True
  PlainLeftClick -> isPlainLeftClick event
  Keys wanted ->
    let keyboard = KeyboardEvent event
        pressed = KE.key keyboard
        held want field = maybe True (== field keyboard) want
        matches k =
          k.key
            == pressed
            && held k.ctrlKey KE.ctrlKey
            && held k.shiftKey KE.shiftKey
            && held k.altKey KE.altKey
            && held k.metaKey KE.metaKey
     in any matches wanted

-- | A click with the main button and no modifier key, that nothing has
-- prevented already, on a link that opens in its own tab and is not a
-- download: the one a page follows itself. The link is the event's current
-- target, the element whose listener is asking.
--
-- Natively it is read from the event's JSON, as an event a remote client
-- sends or a test makes carries it: @button@ must be there and be 0, and
-- @ctrlKey@, @metaKey@, @shiftKey@, @altKey@ and @defaultPrevented@, when
-- there, false; of the link, the event's @currentTarget@, a @target@ other
-- than @_self@ (in any case) or a @download@ that is @true@ (the link has
-- the attribute) make it no plain click. An event that says nothing of its
-- button, such as the bare one an in-memory test fires, is not one; one
-- that says nothing of its link is taken to be on a link that opens in its
-- own tab.
isPlainLeftClick :: Event -> Bool
isPlainLeftClick event
  | inBrowser = js_plain_left_click event
  | otherwise = nativePlainLeftClick event

nativePlainLeftClick :: Event -> Bool
nativePlainLeftClick (Event value) =
  readProp "button" (Just . foreignToInt) value
    == Just 0
    && not (any flag ["ctrlKey", "metaKey", "shiftKey", "altKey", "defaultPrevented"])
    && not elsewhere
    && not download
  where
    flag name = readProp name (Just . foreignToBool) value == Just True
    anchor = readProp "currentTarget" Just value
    elsewhere = case T.toLower <$> (readProp "target" (Just . foreignToString) =<< anchor) of
      Just target -> not (T.null target) && target /= "_self"
      Nothing -> False
    download = (readProp "download" (Just . foreignToBool) =<< anchor) == Just True
