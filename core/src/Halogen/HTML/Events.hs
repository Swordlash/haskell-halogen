{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The HasType constraints here are what type-checks a property against the
-- element it is put on; none is needed to build the property, so GHC calls
-- every one of them redundant.
module Halogen.HTML.Events where

import Data.Foreign
import Data.Row
import HPrelude
import Halogen.HTML.Core qualified as Core
import Halogen.HTML.Properties
import Halogen.Query.Input
import Halogen.VDom.DOM.Prop (EventMatch (..), HandlerOptions (..), KeyMatch (..), keyMatch)
import Web.Clipboard.ClipboardEvent
import Web.Clipboard.ClipboardEvent.EventTypes qualified as CET
import Web.Event.Event hiding (defaultPrevented, target, type_)
import Web.Event.Event qualified as EE
import Web.HTML.Event.DragEvent
import Web.HTML.Event.DragEvent.EventTypes qualified as DET
import Web.HTML.Event.EventTypes qualified as ET
import Web.UIEvent.FocusEvent
import Web.UIEvent.FocusEvent.EventTypes qualified as FET
import Web.UIEvent.KeyboardEvent (KeyboardEvent (..))
import Web.UIEvent.KeyboardEvent.EventTypes qualified as KET
import Web.UIEvent.MouseEvent (MouseEvent (..))
import Web.UIEvent.MouseEvent.EventTypes qualified as MET
import Web.UIEvent.PointerEvent
import Web.UIEvent.TouchEvent
import Web.UIEvent.WheelEvent
import Web.UIEvent.WheelEvent.EventTypes qualified as WET

handler :: forall r i. EventType -> (Event -> i) -> IProp r i
handler et f = IProp $ Core.handler et (Just . Action . f)

handler' :: forall r i. EventType -> (Event -> Maybe i) -> IProp r i
handler' et f = IProp $ Core.handler et (fmap Action . f)

-- | The handler, keeping the browser from taking the event's default action
-- (following a link, submitting a form, typing a key) whenever it makes an
-- action: a handler that makes none (say, 'handler'' for a key it does not
-- handle) leaves the default alone. With a match declared ('matching'), it
-- is every event that matches whose default is prevented.
--
-- > HH.form [HE.preventingDefault (HE.onSubmit (const Save))] […]
--
-- Calling 'Web.Event.Event.preventDefault' from the component's action
-- handler, in 'Halogen.Query.HalogenM.liftEffect', is not the same: that runs
-- when the tree's loop gets to the action, which is during the event's
-- dispatch only if the loop is free when the event arrives. An event that
-- arrives while the loop is busy elsewhere has its action queued, and the
-- browser has followed the link by the time it runs. This prevents the
-- default in the event listener itself, while the event is dispatched,
-- before the action is handed to the loop. Anything else on a property
-- leaves it as it is.
preventingDefault :: forall r i. IProp r i -> IProp r i
preventingDefault (IProp p) = IProp $ Core.withHandlerOptions (\options -> options {preventDefault = True}) p

-- | The handler, stopping the event from reaching the listeners of the
-- elements around whenever it makes an action, in the event listener itself,
-- as 'preventingDefault' prevents the default (and, with a match declared,
-- whenever an event matches).
stoppingPropagation :: forall r i. IProp r i -> IProp r i
stoppingPropagation (IProp p) = IProp $ Core.withHandlerOptions (\options -> options {stopPropagation = True}) p

-- | The handler, asked only about the events that match: the others are
-- not handed to its function at all, and 'preventingDefault' and
-- 'stoppingPropagation' apply to every event that matches, whether or not
-- the function then makes an action for it.
--
-- > HH.div [HE.preventingDefault (HE.matching (HE.keys ["/"]) (HE.onKeyDown (const FocusSearch)))] […]
--
-- In a page this only says ahead of time what the function would decide.
-- It matters where the page is not the program's: under
-- haskell-halogen-remote's @RemoteDOM@ the handler runs in the program,
-- after the page has finished with the event, and a match is what the page
-- reads instead, while it dispatches the event, to prevent the default or
-- stop it in time; an event that does not match does not cross the wire.
-- A link of "Halogen.Router" declares 'plainLeftClick'.
matching :: forall r i. EventMatch -> IProp r i -> IProp r i
matching match (IProp p) = IProp $ Core.withHandlerOptions (\options -> options {matching = Just match}) p

-- | Every event of the handler's type: the default prevented of each, with
-- 'preventingDefault', whatever the function makes of it.
anyEvent :: EventMatch
anyEvent = AnyEvent

-- | A click the browser would follow as it is, in this tab: the main
-- button, no modifier key, nothing that prevented it already, a link with
-- no other target that is no download. The click a page follows itself.
plainLeftClick :: EventMatch
plainLeftClick = PlainLeftClick

-- | A keyboard event for one of these keys, as the event's @key@ names them
-- (@"\/"@, @"ArrowLeft"@, @"Escape"@), with any modifiers or none. For a
-- key with a modifier, 'keysWith'.
keys :: [Text] -> EventMatch
keys = Keys . map keyMatch

-- | A keyboard event for one of these keys and modifiers, which
-- "Halogen.VDom.DOM.HandlerOptions" spells:
--
-- > import Halogen.VDom.DOM.HandlerOptions (KeyMatch (..), keyMatch)
-- >
-- > HE.keysWith [(keyMatch "s") {ctrlKey = Just True}]
keysWith :: [KeyMatch] -> EventMatch
keysWith = Keys

-- | An event on an element the CSS selector matches, inside the listening
-- element (the target itself or its nearest ancestor that matches), for
-- which the inner match holds judged on that element: one handler on a
-- container for the links in it, each click judged on its link.
--
-- > HH.div
-- >   [HE.preventingDefault (HE.matching (HE.within "a[href]" HE.plainLeftClick) (HE.handler' (EventType "click") followLink))]
-- >   [rendered]
--
-- The handler's function finds the link itself, purely, in the listener
-- ("Web.DOM.Element"'s 'Web.DOM.Element.closestNow' from the event's
-- target, then 'Web.DOM.Element.getAttributeNow'); over
-- haskell-halogen-remote's wire those two read what the page found for the
-- selector, which the page sends with the event because the match names
-- it. A selector that does not parse matches nothing. See
-- 'Halogen.VDom.DOM.HandlerOptions.Within'.
within :: Text -> EventMatch -> EventMatch
within = Within

-- | A drag or a paste carrying data of one of these types (@"Files"@,
-- @"text/plain"@, @"text/uri-list"@): a paste of files is the handler's,
-- one of text left to the browser.
--
-- > HH.div [HE.preventingDefault (HE.matching (HE.carrying ["Files"]) (HE.onPaste pastedFiles))] [editor]
--
-- See 'Halogen.VDom.DOM.HandlerOptions.Carrying'.
carrying :: [Text] -> EventMatch
carrying = Carrying

onAbort :: forall r i. (HasType "onAbort" Event r) => (Event -> i) -> IProp r i
onAbort = handler (EventType "abort")

onError :: forall r i. (HasType "onError" Event r) => (Event -> i) -> IProp r i
onError = handler ET.error

onLoad :: forall r i. (HasType "onLoad" Event r) => (Event -> i) -> IProp r i
onLoad = handler ET.load

onScroll :: forall r i. (HasType "onScroll" Event r) => (Event -> i) -> IProp r i
onScroll = handler (EventType "scroll")

onChange :: forall r i. (HasType "onChange" Event r) => (Event -> i) -> IProp r i
onChange = handler ET.change

{-
onFileUpload
  :: forall r i t
   . HasType "onChange" Event r
  => Unfoldable t
  => (t File -> i)
  -> IProp r i
onFileUpload f = handler ET.change $
  ( Event.target
      >=> HTMLInputElement.fromEventTarget
      >=>
        HTMLInputElement.files >>> unsafePerformEffect
  )
    >>> maybe none items
    >>> f
-}

onInput :: forall r i. (HasType "onInput" Event r) => (Event -> i) -> IProp r i
onInput = handler ET.input

onInvalid :: forall r i. (HasType "onInvalid" Event r) => (Event -> i) -> IProp r i
onInvalid = handler ET.invalid

onReset :: forall r i. (HasType "onReset" Event r) => (Event -> i) -> IProp r i
onReset = handler (EventType "reset")

onSelect :: forall r i. (HasType "onSelect" Event r) => (Event -> i) -> IProp r i
onSelect = handler ET.select

onSubmit :: forall r i. (HasType "onSubmit" Event r) => (Event -> i) -> IProp r i
onSubmit = handler (EventType "submit")

onTransitionEnd :: forall r i. (HasType "onTransitionEnd" Event r) => (Event -> i) -> IProp r i
onTransitionEnd = handler (EventType "transitionend")

onCopy :: forall r i. (HasType "onCopy" ClipboardEvent r) => (ClipboardEvent -> i) -> IProp r i
onCopy = handler CET.copy . clipboardHandler

onPaste :: forall r i. (HasType "onPaste" ClipboardEvent r) => (ClipboardEvent -> i) -> IProp r i
onPaste = handler CET.paste . clipboardHandler

onCut :: forall r i. (HasType "onCut" ClipboardEvent r) => (ClipboardEvent -> i) -> IProp r i
onCut = handler CET.cut . clipboardHandler

onAuxClick :: forall r i. (HasType "onAuxClick" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onAuxClick = handler MET.auxclick . mouseHandler

onClick :: forall r i. (HasType "onClick" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onClick = handler MET.click . mouseHandler

-- onContextMenu :: forall r i. HasType "onContextMenu" MouseEvent r => (MouseEvent -> i) -> IProp r i
-- onContextMenu = handler ET.contextmenu . mouseHandler

onDoubleClick :: forall r i. (HasType "onDoubleClick" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onDoubleClick = handler MET.dblclick . mouseHandler

onMouseDown :: forall r i. (HasType "onMouseDown" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseDown = handler MET.mousedown . mouseHandler

onMouseEnter :: forall r i. (HasType "onMouseEnter" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseEnter = handler MET.mouseenter . mouseHandler

onMouseLeave :: forall r i. (HasType "onMouseLeave" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseLeave = handler MET.mouseleave . mouseHandler

onMouseMove :: forall r i. (HasType "onMouseMove" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseMove = handler MET.mousemove . mouseHandler

onMouseOver :: forall r i. (HasType "onMouseOver" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseOver = handler MET.mouseover . mouseHandler

onMouseOut :: forall r i. (HasType "onMouseOut" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseOut = handler MET.mouseout . mouseHandler

onMouseUp :: forall r i. (HasType "onMouseUp" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onMouseUp = handler MET.mouseup . mouseHandler

onWheel :: forall r i. (HasType "onWheel" WheelEvent r) => (WheelEvent -> i) -> IProp r i
onWheel = handler WET.wheel . wheelHandler

onKeyDown :: forall r i. (HasType "onKeyDown" KeyboardEvent r) => (KeyboardEvent -> i) -> IProp r i
onKeyDown = handler KET.keydown . keyHandler

-- onKeyPress :: forall r i. HasType "onKeyPress" KeyboardEvent r => (KeyboardEvent -> i) -> IProp r i
-- onKeyPress = handler KET.keypress . keyHandler

onKeyUp :: forall r i. (HasType "onKeyUp" KeyboardEvent r) => (KeyboardEvent -> i) -> IProp r i
onKeyUp = handler KET.keyup . keyHandler

onBlur :: forall r i. (HasType "onBlur" FocusEvent r) => (FocusEvent -> i) -> IProp r i
onBlur = handler ET.blur . focusHandler

onFocus :: forall r i. (HasType "onFocus" FocusEvent r) => (FocusEvent -> i) -> IProp r i
onFocus = handler FET.focus . focusHandler

onFocusIn :: forall r i. (HasType "onFocusIn" FocusEvent r) => (FocusEvent -> i) -> IProp r i
onFocusIn = handler FET.focusin . focusHandler

onFocusOut :: forall r i. (HasType "onFocusOut" FocusEvent r) => (FocusEvent -> i) -> IProp r i
onFocusOut = handler FET.focusout . focusHandler

onDrag :: forall r i. (HasType "onDrag" DragEvent r) => (DragEvent -> i) -> IProp r i
onDrag = handler DET.drag . dragHandler

onDragEnd :: forall r i. (HasType "onDragEnd" DragEvent r) => (DragEvent -> i) -> IProp r i
onDragEnd = handler DET.dragend . dragHandler

onDragExit :: forall r i. (HasType "onDragExit" DragEvent r) => (DragEvent -> i) -> IProp r i
onDragExit = handler DET.dragexit . dragHandler

onDragEnter :: forall r i. (HasType "onDragEnter" DragEvent r) => (DragEvent -> i) -> IProp r i
onDragEnter = handler DET.dragenter . dragHandler

onDragLeave :: forall r i. (HasType "onDragLeave" DragEvent r) => (DragEvent -> i) -> IProp r i
onDragLeave = handler DET.dragleave . dragHandler

onDragOver :: forall r i. (HasType "onDragOver" DragEvent r) => (DragEvent -> i) -> IProp r i
onDragOver = handler DET.dragover . dragHandler

onDragStart :: forall r i. (HasType "onDragStart" DragEvent r) => (DragEvent -> i) -> IProp r i
onDragStart = handler DET.dragstart . dragHandler

onDrop :: forall r i. (HasType "onDrop" DragEvent r) => (DragEvent -> i) -> IProp r i
onDrop = handler DET.drop . dragHandler

onTouchCancel :: forall r i. (HasType "onTouchCancel" TouchEvent r) => (TouchEvent -> i) -> IProp r i
onTouchCancel = handler (EventType "touchcancel") . touchHandler

onTouchEnd :: forall r i. (HasType "onTouchEnd" TouchEvent r) => (TouchEvent -> i) -> IProp r i
onTouchEnd = handler (EventType "touchend") . touchHandler

onTouchEnter :: forall r i. (HasType "onTouchEnter" TouchEvent r) => (TouchEvent -> i) -> IProp r i
onTouchEnter = handler (EventType "touchenter") . touchHandler

onTouchLeave :: forall r i. (HasType "onTouchLeave" TouchEvent r) => (TouchEvent -> i) -> IProp r i
onTouchLeave = handler (EventType "touchleave") . touchHandler

onTouchMove :: forall r i. (HasType "onTouchMove" TouchEvent r) => (TouchEvent -> i) -> IProp r i
onTouchMove = handler (EventType "touchmove") . touchHandler

onTouchStart :: forall r i. (HasType "onTouchStart" TouchEvent r) => (TouchEvent -> i) -> IProp r i
onTouchStart = handler (EventType "touchstart") . touchHandler

onPointerOver :: forall r i. (HasType "onPointerOver" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerOver = handler (EventType "pointerover") . pointerHandler

onPointerEnter :: forall r i. (HasType "onPointerEnter" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerEnter = handler (EventType "pointerenter") . pointerHandler

onPointerDown :: forall r i. (HasType "onPointerDown" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerDown = handler (EventType "pointerdown") . pointerHandler

onPointerMove :: forall r i. (HasType "onPointerMove" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerMove = handler (EventType "pointermove") . pointerHandler

onPointerUp :: forall r i. (HasType "onPointerUp" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerUp = handler (EventType "pointerup") . pointerHandler

onPointerCancel :: forall r i. (HasType "onPointerCancel" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerCancel = handler (EventType "pointercancel") . pointerHandler

onPointerOut :: forall r i. (HasType "onPointerOut" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerOut = handler (EventType "pointerout") . pointerHandler

onPointerLeave :: forall r i. (HasType "onPointerLeave" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onPointerLeave = handler (EventType "pointerleave") . pointerHandler

onGotPointerCapture :: forall r i. (HasType "onGotPointerCapture" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onGotPointerCapture = handler (EventType "gotpointercapture") . pointerHandler

onLostPointerCapture :: forall r i. (HasType "onLostPointerCapture" PointerEvent r) => (PointerEvent -> i) -> IProp r i
onLostPointerCapture = handler (EventType "lostpointercapture") . pointerHandler

onResize :: forall r i. (HasType "onResize" Event r) => (Event -> i) -> IProp r i
onResize = handler (EventType "resize")

keyHandler :: forall i. (KeyboardEvent -> i) -> Event -> i
keyHandler = coerce

mouseHandler :: forall i. (MouseEvent -> i) -> Event -> i
mouseHandler = coerce

pointerHandler :: forall i. (PointerEvent -> i) -> Event -> i
pointerHandler = coerce

wheelHandler :: forall i. (WheelEvent -> i) -> Event -> i
wheelHandler = coerce

focusHandler :: forall i. (FocusEvent -> i) -> Event -> i
focusHandler = coerce

dragHandler :: forall i. (DragEvent -> i) -> Event -> i
dragHandler = coerce

clipboardHandler :: forall i. (ClipboardEvent -> i) -> Event -> i
clipboardHandler = coerce

touchHandler :: forall i. (TouchEvent -> i) -> Event -> i
touchHandler = coerce

-- | Attaches event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall value tag r i. EventType -> Text -> (Foreign tag -> Maybe value) -> (value -> i) -> IProp r i
addForeignPropHandler key prop_ reader_ f =
  handler' key $ EE.currentTarget >=> go
  where
    go a = f <$> readProp prop_ reader_ (coerce a)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r i. (HasType "value" Text r, HasType "onChange" Event r) => (Text -> i) -> IProp r i
onValueChange = addForeignPropHandler ET.change "value" (Just . foreignToString)

--  | Reacts to the intermediate value change of the input element by checking the input value of event
onInputValueChange :: forall r i. (HasType "value" Text r, HasType "onInput" Event r) => (Text -> Maybe i) -> IProp r i
onInputValueChange f = handler' ET.input $ \ev -> do
  trg <- coerce <$> currentTarget ev
  readProp "value" (f . foreignToString) trg

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r i. (HasType "selectedIndex" Int r, HasType "onChange" Event r) => (Int -> i) -> IProp r i
onSelectedIndexChange = addForeignPropHandler ET.change "selectedIndex" (Just . foreignToInt)

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r i. (HasType "value" Text r, HasType "onInput" Event r) => (Text -> i) -> IProp r i
onValueInput = addForeignPropHandler ET.input "value" (Just . foreignToString)

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r i. (HasType "checked" Bool r, HasType "onChange" Event r) => (Bool -> i) -> IProp r i
onChecked = addForeignPropHandler ET.change "checked" (Just . foreignToBool)
