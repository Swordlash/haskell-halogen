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

import Data.Foreign (foreignToBool, foreignToInt, foreignToString, readElements, readProp)
import Data.Text qualified as T
import HPrelude
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, inBrowser, isNull, jsNull, toJSText)
import Web.Event.Internal.Types (Event (..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent (..))
import Web.UIEvent.KeyboardEvent qualified as KE

$( browserJS
     ["jsbits/events.js"]
     [ ("js_current_target", "halogen_current_target", Unsafe, [t|Event -> JSVal|])
     , ("js_plain_left_click_on", "halogen_plain_left_click_on", Unsafe, [t|Event -> JSVal -> Bool|])
     , ("js_within", "halogen_within", Unsafe, [t|Event -> JSVal -> JSText -> JSVal|])
     , ("js_carrying", "halogen_carrying", Unsafe, [t|Event -> JSText -> Bool|])
     ]
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
  | -- | The event's target is, or is inside, an element the CSS selector
    -- matches, which is itself the listening element or inside it; and the
    -- inner match holds, judged with that element in the place of the
    -- listening one. One handler on a container hears the clicks on the
    -- links inside it, and judges each on its link:
    --
    -- > Within "a[href]" PlainLeftClick
    --
    -- is a plain click on a link inside, one that opens in its own tab and
    -- is no download ('isPlainLeftClick' of the link, not of the
    -- container). The element is the nearest one around the target that the
    -- selector matches (@target.closest(selector)@, the target's parent
    -- element for a target that is not an element); one outside the
    -- listening element does not count, even when it is the only one. A
    -- selector that does not parse matches nothing.
    --
    -- A 'Within' inside another looks for its element inside the outer
    -- one's: @Within ".article" (Within "a[href]" PlainLeftClick)@ is a
    -- plain click on a link inside an article inside the listening element,
    -- the link judged.
    Within Text EventMatch
  | -- | A drag or a paste that carries data of one of these types: its
    -- @dataTransfer@ (a drag event's) or @clipboardData@ (a clipboard
    -- event's) lists it among its @types@, as @"Files"@ for files,
    -- @"text/plain"@, @"text/uri-list"@, @"text/html"@. An event that
    -- carries neither matches none. A file's own media type is not one of
    -- the types: a drop of an image carries @"Files"@.
    Carrying [Text]
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
-- an event a remote client sends or a test makes carries it: a 'Within'
-- finds its element under the event's @target@'s @closest@, by its
-- selector (@{"target": {"closest": {"a[href]": {...}}}}@), and takes the
-- sender's word that it is where the match says; a 'Carrying' reads the
-- @types@ of the event's @dataTransfer@ or @clipboardData@.
-- "Halogen.Remote.Protocol" spells what a client sends.
matchesEvent :: EventMatch -> Event -> Bool
matchesEvent match event = matchesOn (listening event) match event

-- The element whose listener is asking: the event's current target.
listening :: Event -> JSVal
listening event@(Event value)
  | inBrowser = js_current_target event
  | otherwise = fromMaybe jsNull (readProp "currentTarget" Just value)

-- Whether the event matches, judged against an element: the listening one,
-- or the one a 'Within' found.
matchesOn :: JSVal -> EventMatch -> Event -> Bool
matchesOn element match event = case match of
  AnyEvent -> True
  PlainLeftClick -> plainLeftClickOn element event
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
  Within selector inner -> case within element selector event of
    Just found -> matchesOn found inner event
    Nothing -> False
  Carrying types -> any (carrying event) types

-- The element around the event's target that a selector matches, inside
-- the element given. Natively it is the one the event's JSON says the
-- client found for the selector.
within :: JSVal -> Text -> Event -> Maybe JSVal
within element selector event@(Event value)
  | inBrowser =
      let found = js_within event element (toJSText selector)
       in if isNull found then Nothing else Just found
  | otherwise = readProp "target" (readProp "closest" (readProp selector Just)) value

-- Whether the event's transfer lists a type.
carrying :: Event -> Text -> Bool
carrying event@(Event value) wanted
  | inBrowser = js_carrying event (toJSText wanted)
  | otherwise = case readProp "dataTransfer" Just value <|> readProp "clipboardData" Just value of
      Just transfer -> wanted `elem` maybe [] (map foreignToString . readElements) (readProp "types" Just transfer)
      Nothing -> False

-- | A click with the main button and no modifier key, that nothing has
-- prevented already, on a link that opens in its own tab and is not a
-- download: the one a page follows itself. The link is the event's current
-- target, the element whose listener is asking (under a 'Within', the
-- element it found).
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
isPlainLeftClick event = plainLeftClickOn (listening event) event

plainLeftClickOn :: JSVal -> Event -> Bool
plainLeftClickOn anchor event
  | inBrowser = js_plain_left_click_on event anchor
  | otherwise = nativePlainLeftClick anchor event

nativePlainLeftClick :: JSVal -> Event -> Bool
nativePlainLeftClick anchor (Event value) =
  readProp "button" (Just . foreignToInt) value
    == Just 0
    && not (any flag ["ctrlKey", "metaKey", "shiftKey", "altKey", "defaultPrevented"])
    && not elsewhere
    && not download
  where
    flag name = readProp name (Just . foreignToBool) value == Just True
    elsewhere = case T.toLower <$> readProp "target" (Just . foreignToString) anchor of
      Just target -> not (T.null target) && target /= "_self"
      Nothing -> False
    download = readProp "download" (Just . foreignToBool) anchor == Just True
