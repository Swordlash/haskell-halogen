module Halogen.HTML.Events where

import Data.Foreign
import Data.Row
import HPrelude
import Halogen.HTML.Core qualified as Core
import Halogen.HTML.Properties
import Halogen.Query.Input
import Web.Clipboard.ClipboardEvent
import Web.Clipboard.ClipboardEvent.EventTypes qualified as CET
import Web.Event.Event
import Web.Event.Event qualified as EE
import Web.HTML.Event.DragEvent
import Web.HTML.Event.DragEvent.EventTypes qualified as DET
import Web.HTML.Event.EventTypes qualified as ET
import Web.UIEvent.FocusEvent
import Web.UIEvent.FocusEvent.EventTypes qualified as FET
import Web.UIEvent.KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes qualified as KET
import Web.UIEvent.MouseEvent
import Web.UIEvent.MouseEvent.EventTypes qualified as MET
import Web.UIEvent.TouchEvent
import Web.UIEvent.WheelEvent
import Web.UIEvent.WheelEvent.EventTypes qualified as WET

handler :: forall r i. EventType -> (Event -> i) -> IProp r i
handler et f = IProp $ Core.handler et (Just . Action . f)

handler' :: forall r i. EventType -> (Event -> Maybe i) -> IProp r i
handler' et f = IProp $ Core.handler et (fmap Action . f)

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

onResize :: forall r i. (HasType "onResize" Event r) => (Event -> i) -> IProp r i
onResize = handler (EventType "resize")

keyHandler :: forall i. (KeyboardEvent -> i) -> Event -> i
keyHandler = coerce

mouseHandler :: forall i. (MouseEvent -> i) -> Event -> i
mouseHandler = coerce

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
addForeignPropHandler :: EventType -> Text -> (Foreign tag -> Maybe value) -> (value -> i) -> IProp r i
addForeignPropHandler key prop_ reader_ f =
  handler' key $ EE.currentTarget >=> go
  where
    go a = f <$> readProp prop_ reader_ (coerce a)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r i. (HasType "value" Text r, HasType "onChange" Event r) => (Text -> i) -> IProp r i
onValueChange = addForeignPropHandler ET.change "value" (Just . foreignToString)

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
