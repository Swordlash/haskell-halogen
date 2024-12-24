module Halogen.HTML.Properties where

import DOM.HTML.Indexed qualified as I
import Data.Coerce
import Data.MediaType
import Data.Row
import Data.Text qualified as T
import HPrelude
import Halogen.HTML.Core (IsProp (..), Namespace)
import Halogen.HTML.Core qualified as Core
import Halogen.Query.Input
import Halogen.VDom.DOM.Prop
import Web.DOM.Element
import Web.HTML.Common

newtype IProp (r :: Row Type) msg = IProp (Prop (Input msg))

prop :: (IsProp value) => PropName value -> value -> IProp r msg
prop _name val = IProp $ Property _name (toPropValue val)

attr :: AttrName -> Text -> IProp r msg
attr _name val = IProp $ Attribute Nothing _name val

attrNS :: Namespace -> AttrName -> Text -> IProp r i
attrNS ns _name val = IProp $ Attribute (Just ns) _name val

rows :: (HasType "rows" Int r) => Int -> IProp r msg
rows = prop "rows"

-- cast property from smaller row to greater row
castProp :: (Subset r1 r2) => IProp r1 msg -> IProp r2 msg
castProp = coerce

-- | The `ref` property allows an input to be raised once a `HTMLElement` has
-- | been created or destroyed in the DOM for the element that the property is
-- | attached to.
ref :: forall r i. RefLabel -> IProp r i
ref = IProp . Core.ref . go
  where
    go :: RefLabel -> Maybe Element -> Maybe (Input i)
    go p mel = Just (RefUpdate p mel)

alt :: (HasType "alt" Text r) => Text -> IProp r i
alt = prop "alt"

charset :: (HasType "charset" Text r) => Text -> IProp r i
charset = prop "charset"

class_ :: (HasType "className" Text r) => ClassName -> IProp r i
class_ (ClassName txt) = prop "className" txt

classes :: (HasType "className" Text r) => [ClassName] -> IProp r i
classes = prop "className" . T.intercalate " " . coerce

cols :: (HasType "cols" Int r) => Int -> IProp r i
cols = prop "cols"

colSpan :: (HasType "colSpan" Int r) => Int -> IProp r i
colSpan = prop "colSpan"

rowSpan :: (HasType "rowSpan" Int r) => Int -> IProp r i
rowSpan = prop "rowSpan"

for :: (HasType "htmlFor" Text r) => Text -> IProp r i
for = prop "htmlFor"

height :: (HasType "height" I.CSSPixel r) => I.CSSPixel -> IProp r i
height = prop "height"

width :: (HasType "width" I.CSSPixel r) => I.CSSPixel -> IProp r i
width = prop "width"

href :: (HasType "href" Text r) => Text -> IProp r i
href = prop "href"

id :: (HasType "id" Text r) => Text -> IProp r i
id = prop "id"

name :: (HasType "name" Text r) => Text -> IProp r i
name = prop "name"

rel :: (HasType "rel" Text r) => Text -> IProp r i
rel = prop "rel"

src :: (HasType "src" Text r) => Text -> IProp r i
src = prop "src"

srcDoc :: (HasType "srcDoc" Text r) => Text -> IProp r i
srcDoc = prop "srcdoc"

-- | Sets the `style` attribute to the specified Text.
-- |
-- | ```purs
-- | ... [ style "height: 50px;" ]
-- | ```
-- |
-- | If you prefer to use typed CSS for this attribute, you can use the purescript-halogen-css library:
-- | https://github.com/purescript-halogen/purescript-halogen-css
style :: (HasType "style" Text r) => Text -> IProp r i
style = attr (AttrName "style")

scope :: (HasType "scope" I.ScopeValue r) => I.ScopeValue -> IProp r i
scope = prop (PropName "scope")

target :: (HasType "target" Text r) => Text -> IProp r i
target = prop (PropName "target")

title :: (HasType "title" Text r) => Text -> IProp r i
title = prop (PropName "title")

download :: (HasType "download" Text r) => Text -> IProp r i
download = prop (PropName "download")

method :: (HasType "method" I.FormMethod r) => I.FormMethod -> IProp r i
method = prop (PropName "method")

action :: (HasType "action" Text r) => Text -> IProp r i
action = prop (PropName "action")

enctype :: (HasType "enctype" MediaType r) => MediaType -> IProp r i
enctype = prop (PropName "enctype")

noValidate :: (HasType "noValidate" Bool r) => Bool -> IProp r i
noValidate = prop (PropName "noValidate")

type_ :: (HasType "type" value r, IsProp value) => value -> IProp r i
type_ = prop (PropName "type")

value :: (HasType "value" value r, IsProp value) => value -> IProp r i
value = prop (PropName "value")

min :: (HasType "min" Double r) => Double -> IProp r i
min = prop (PropName "min")

max :: (HasType "max" Double r) => Double -> IProp r i
max = prop (PropName "max")

step :: (HasType "step" I.StepValue r) => I.StepValue -> IProp r i
step = prop (PropName "step")

enabled :: (HasType "disabled" Bool r) => Bool -> IProp r i
enabled = disabled . not

disabled :: (HasType "disabled" Bool r) => Bool -> IProp r i
disabled = prop (PropName "disabled")

required :: (HasType "required" Bool r) => Bool -> IProp r i
required = prop (PropName "required")

readOnly :: (HasType "readOnly" Bool r) => Bool -> IProp r i
readOnly = prop (PropName "readOnly")

spellcheck :: (HasType "spellcheck" Bool r) => Bool -> IProp r i
spellcheck = prop (PropName "spellcheck")

checked :: (HasType "checked" Bool r) => Bool -> IProp r i
checked = prop (PropName "checked")

selected :: (HasType "selected" Bool r) => Bool -> IProp r i
selected = prop (PropName "selected")

selectedIndex :: (HasType "selectedIndex" Int r) => Int -> IProp r i
selectedIndex = prop (PropName "selectedIndex")

placeholder :: (HasType "placeholder" Text r) => Text -> IProp r i
placeholder = prop (PropName "placeholder")

autocomplete :: (HasType "autocomplete" I.AutocompleteType r) => I.AutocompleteType -> IProp r i
autocomplete = prop (PropName "autocomplete")

list :: (HasType "list" Text r) => Text -> IProp r i
list = attr (AttrName "list")

autofocus :: (HasType "autofocus" Bool r) => Bool -> IProp r i
autofocus = prop (PropName "autofocus")

multiple :: (HasType "multiple" Bool r) => Bool -> IProp r i
multiple = prop (PropName "multiple")

accept :: (HasType "accept" I.InputAcceptType r) => I.InputAcceptType -> IProp r i
accept = prop (PropName "accept")

pattern :: (HasType "pattern" Text r) => Text -> IProp r i
pattern = prop (PropName "pattern")

autoplay :: (HasType "autoplay" Bool r) => Bool -> IProp r i
autoplay = prop (PropName "autoplay")

controls :: (HasType "controls" Bool r) => Bool -> IProp r i
controls = prop (PropName "controls")

loop :: (HasType "loop" Bool r) => Bool -> IProp r i
loop = prop (PropName "loop")

muted :: (HasType "muted" Bool r) => Bool -> IProp r i
muted = prop (PropName "muted")

poster :: (HasType "poster" Text r) => Text -> IProp r i
poster = prop (PropName "poster")

preload :: (HasType "preload" I.PreloadValue r) => I.PreloadValue -> IProp r i
preload = prop (PropName "preload")

draggable :: (HasType "draggable" Bool r) => Bool -> IProp r i
draggable = prop (PropName "draggable")

tabIndex :: (HasType "tabIndex" Int r) => Int -> IProp r i
tabIndex = prop (PropName "tabIndex")
