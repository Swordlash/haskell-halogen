module Halogen.HTML.Properties.ARIA where

import HPrelude
import Halogen.HTML.Core (AttrName (..))
import Halogen.HTML.Properties (IProp, attr)

activeDescendant :: forall r i. Text -> IProp r i
activeDescendant = attr (AttrName "aria-activedescendant")

atomic :: forall r i. Text -> IProp r i
atomic = attr (AttrName "aria-atomic")

autoComplete :: forall r i. Text -> IProp r i
autoComplete = attr (AttrName "aria-autocomplete")

busy :: forall r i. Text -> IProp r i
busy = attr (AttrName "aria-busy")

checked :: forall r i. Text -> IProp r i
checked = attr (AttrName "aria-checked")

controls :: forall r i. Text -> IProp r i
controls = attr (AttrName "aria-controls")

describedBy :: forall r i. Text -> IProp r i
describedBy = attr (AttrName "aria-describedby")

disabled :: forall r i. Text -> IProp r i
disabled = attr (AttrName "aria-disabled")

dropEffect :: forall r i. Text -> IProp r i
dropEffect = attr (AttrName "aria-dropeffect")

expanded :: forall r i. Text -> IProp r i
expanded = attr (AttrName "aria-expanded")

flowTo :: forall r i. Text -> IProp r i
flowTo = attr (AttrName "aria-flowto")

grabbed :: forall r i. Text -> IProp r i
grabbed = attr (AttrName "aria-grabbed")

hasPopup :: forall r i. Text -> IProp r i
hasPopup = attr (AttrName "aria-haspopup")

hidden :: forall r i. Text -> IProp r i
hidden = attr (AttrName "aria-hidden")

invalid :: forall r i. Text -> IProp r i
invalid = attr (AttrName "aria-invalid")

label :: forall r i. Text -> IProp r i
label = attr (AttrName "aria-label")

labelledBy :: forall r i. Text -> IProp r i
labelledBy = attr (AttrName "aria-labelledby")

level :: forall r i. Text -> IProp r i
level = attr (AttrName "aria-level")

live :: forall r i. Text -> IProp r i
live = attr (AttrName "aria-live")

modal :: forall r i. Text -> IProp r i
modal = attr (AttrName "aria-modal")

multiLine :: forall r i. Text -> IProp r i
multiLine = attr (AttrName "aria-multiline")

multiSelectable :: forall r i. Text -> IProp r i
multiSelectable = attr (AttrName "aria-multiselectable")

orientation :: forall r i. Text -> IProp r i
orientation = attr (AttrName "aria-orientation")

owns :: forall r i. Text -> IProp r i
owns = attr (AttrName "aria-owns")

posInSet :: forall r i. Text -> IProp r i
posInSet = attr (AttrName "aria-posinset")

pressed :: forall r i. Text -> IProp r i
pressed = attr (AttrName "aria-pressed")

readOnly :: forall r i. Text -> IProp r i
readOnly = attr (AttrName "aria-readonly")

relevant :: forall r i. Text -> IProp r i
relevant = attr (AttrName "aria-relevant")

required :: forall r i. Text -> IProp r i
required = attr (AttrName "aria-required")

selected :: forall r i. Text -> IProp r i
selected = attr (AttrName "aria-selected")

setSize :: forall r i. Text -> IProp r i
setSize = attr (AttrName "aria-setsize")

sort :: forall r i. Text -> IProp r i
sort = attr (AttrName "aria-sort")

valueMax :: forall r i. Text -> IProp r i
valueMax = attr (AttrName "aria-valuemax")

valueMin :: forall r i. Text -> IProp r i
valueMin = attr (AttrName "aria-valuemin")

valueNow :: forall r i. Text -> IProp r i
valueNow = attr (AttrName "aria-valuenow")

valueText :: forall r i. Text -> IProp r i
valueText = attr (AttrName "aria-valuetext")

role :: forall r i. Text -> IProp r i
role = attr (AttrName "role")
