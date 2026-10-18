{-# LANGUAGE TemplateHaskellQuotes #-}

-- | The names an HTML template compiles to: the element, property and event
-- functions of "Halogen.HTML", by the name written in HTML. The tables are
-- the export lists of those modules; a name missing here becomes a plain
-- element or attribute.
module Halogen.TH.Html.Names
  ( ElementKind (..)
  , element
  , PropKind (..)
  , property
  , event
  , isVoid
  )
where

import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.HTML.Elements qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Language.Haskell.TH (Name)

data ElementKind
  = WithChildren Name
  -- ^ Takes properties and children.
  | NoChildren Name
  -- ^ Takes properties only.

element :: Text -> Maybe ElementKind
element = (`Map.lookup` elements) . T.map toLower

elements :: Map Text ElementKind
elements =
  Map.fromList $
    [(n, WithChildren x) | (n, x) <- nodes] <> [(n, NoChildren x) | (n, x) <- leaves]
  where
    nodes =
      [ ("a", 'HH.a), ("abbr", 'HH.abbr), ("address", 'HH.address), ("article", 'HH.article), ("aside", 'HH.aside), ("audio", 'HH.audio)
      , ("b", 'HH.b), ("bdi", 'HH.bdi), ("bdo", 'HH.bdo), ("blockquote", 'HH.blockquote), ("body", 'HH.body), ("button", 'HH.button)
      , ("caption", 'HH.caption), ("cite", 'HH.cite), ("code", 'HH.code), ("colgroup", 'HH.colgroup)
      , ("datalist", 'HH.datalist), ("dd", 'HH.dd), ("del", 'HH.del), ("details", 'HH.details), ("dfn", 'HH.dfn), ("dialog", 'HH.dialog), ("div", 'HH.div), ("dl", 'HH.dl), ("dt", 'HH.dt)
      , ("em", 'HH.em), ("embed", 'HH.embed)
      , ("fieldset", 'HH.fieldset), ("figcaption", 'HH.figcaption), ("figure", 'HH.figure), ("footer", 'HH.footer), ("form", 'HH.form)
      , ("h1", 'HH.h1), ("h2", 'HH.h2), ("h3", 'HH.h3), ("h4", 'HH.h4), ("h5", 'HH.h5), ("h6", 'HH.h6), ("head", 'HH.head), ("header", 'HH.header), ("html", 'HH.html)
      , ("i", 'HH.i), ("ins", 'HH.ins), ("kbd", 'HH.kbd), ("label", 'HH.label), ("legend", 'HH.legend), ("li", 'HH.li)
      , ("main", 'HH.main), ("map", 'HH.map), ("mark", 'HH.mark), ("menu", 'HH.menu), ("menuitem", 'HH.menuitem), ("meter", 'HH.meter)
      , ("nav", 'HH.nav), ("noscript", 'HH.noscript), ("object", 'HH.object), ("ol", 'HH.ol), ("optgroup", 'HH.optgroup), ("option", 'HH.option), ("output", 'HH.output)
      , ("p", 'HH.p), ("pre", 'HH.pre), ("progress", 'HH.progress), ("q", 'HH.q), ("rp", 'HH.rp), ("rt", 'HH.rt), ("ruby", 'HH.ruby)
      , ("samp", 'HH.samp), ("script", 'HH.script), ("section", 'HH.section), ("select", 'HH.select), ("small", 'HH.small), ("span", 'HH.span), ("strong", 'HH.strong), ("style", 'HH.style), ("sub", 'HH.sub), ("summary", 'HH.summary), ("sup", 'HH.sup)
      , ("table", 'HH.table), ("tbody", 'HH.tbody), ("td", 'HH.td), ("tfoot", 'HH.tfoot), ("th", 'HH.th), ("thead", 'HH.thead), ("time", 'HH.time), ("title", 'HH.title), ("tr", 'HH.tr)
      , ("u", 'HH.u), ("ul", 'HH.ul), ("var", 'HH.var), ("video", 'HH.video)
      ]
    leaves =
      [ ("area", 'HH.area), ("base", 'HH.base), ("br", 'HH.br), ("canvas", 'HH.canvas), ("col", 'HH.col), ("command", 'HH.command)
      , ("hr", 'HH.hr), ("iframe", 'HH.iframe), ("img", 'HH.img), ("input", 'HH.input), ("link", 'HH.link), ("meta", 'HH.meta)
      , ("param", 'HH.param), ("source", 'HH.source), ("textarea", 'HH.textarea), ("track", 'HH.track), ("wbr", 'HH.wbr)
      ]

-- | The elements HTML gives no end tag: their start tag is the whole
-- element, whatever the next tag is.
isVoid :: Text -> Bool
isVoid n = T.map toLower n `elem` ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

-- | What a property takes.
data PropKind
  = TextProp Name
  | ClassProp
  -- ^ @class@: 'HP.class_' over a 'ClassName'.
  | BoolProp Name
  -- ^ Present without a value, or with a Haskell 'Bool'.
  | IntProp Name
  | DoubleProp Name
  | TypedProp Name
  -- ^ A type of its own ('HP.type_', 'HP.value', 'HP.step', …): with
  -- Haskell it is the function; written as text it is the attribute.
  | StyleProp
  -- ^ @style@: Clay's 'Css' or a 'Text' from Haskell (the 'Styled' class),
  -- 'HP.styleText' over text written.
  | RefProp
  -- ^ @ref@: 'HP.ref' over a 'RefLabel'.

-- | By the attribute's name, in any case.
property :: Text -> Maybe PropKind
property = (`Map.lookup` properties) . T.map toLower

properties :: Map Text PropKind
properties =
  Map.fromList
    [ ("accept", TypedProp 'HP.accept)
    , ("action", TextProp 'HP.action)
    , ("alt", TextProp 'HP.alt)
    , ("autocomplete", TypedProp 'HP.autocomplete)
    , ("autofocus", BoolProp 'HP.autofocus)
    , ("autoplay", BoolProp 'HP.autoplay)
    , ("charset", TextProp 'HP.charset)
    , ("checked", BoolProp 'HP.checked)
    , ("class", ClassProp)
    , ("cols", IntProp 'HP.cols)
    , ("colspan", IntProp 'HP.colSpan)
    , ("controls", BoolProp 'HP.controls)
    , ("disabled", BoolProp 'HP.disabled)
    , ("download", TextProp 'HP.download)
    , ("draggable", BoolProp 'HP.draggable)
    , ("enctype", TypedProp 'HP.enctype)
    , ("for", TextProp 'HP.for)
    , ("height", TypedProp 'HP.height)
    , ("hidden", BoolProp 'HP.hidden)
    , ("href", TextProp 'HP.href)
    , ("id", TextProp 'HP.id)
    , ("list", TextProp 'HP.list)
    , ("loop", BoolProp 'HP.loop)
    , ("max", DoubleProp 'HP.max)
    , ("maxlength", IntProp 'HP.maxLength)
    , ("method", TypedProp 'HP.method)
    , ("min", DoubleProp 'HP.min)
    , ("minlength", IntProp 'HP.minLength)
    , ("multiple", BoolProp 'HP.multiple)
    , ("muted", BoolProp 'HP.muted)
    , ("name", TextProp 'HP.name)
    , ("novalidate", BoolProp 'HP.noValidate)
    , ("pattern", TextProp 'HP.pattern_)
    , ("placeholder", TextProp 'HP.placeholder)
    , ("poster", TextProp 'HP.poster)
    , ("preload", TypedProp 'HP.preload)
    , ("readonly", BoolProp 'HP.readOnly)
    , ("ref", RefProp)
    , ("rel", TextProp 'HP.rel)
    , ("required", BoolProp 'HP.required)
    , ("rows", IntProp 'HP.rows)
    , ("rowspan", IntProp 'HP.rowSpan)
    , ("scope", TypedProp 'HP.scope)
    , ("selected", BoolProp 'HP.selected)
    , ("selectedindex", IntProp 'HP.selectedIndex)
    , ("spellcheck", BoolProp 'HP.spellcheck)
    , ("src", TextProp 'HP.src)
    , ("srcdoc", TextProp 'HP.srcDoc)
    , ("step", TypedProp 'HP.step)
    , ("style", StyleProp)
    , ("tabindex", IntProp 'HP.tabIndex)
    , ("target", TextProp 'HP.target)
    , ("title", TextProp 'HP.title)
    , ("type", TypedProp 'HP.type_)
    , ("value", TypedProp 'HP.value)
    , ("width", TypedProp 'HP.width)
    ]

-- | An event handler by the attribute's name, in any case: @onclick@ and
-- @onClick@ alike.
event :: Text -> Maybe Name
event = (`Map.lookup` events) . T.map toLower

events :: Map Text Name
events =
  Map.fromList
    [ ("onabort", 'HE.onAbort)
    , ("onauxclick", 'HE.onAuxClick)
    , ("onblur", 'HE.onBlur)
    , ("onchange", 'HE.onChange)
    , ("onchecked", 'HE.onChecked)
    , ("onclick", 'HE.onClick)
    , ("oncopy", 'HE.onCopy)
    , ("oncut", 'HE.onCut)
    , ("ondoubleclick", 'HE.onDoubleClick)
    , ("ondblclick", 'HE.onDoubleClick)
    , ("ondrag", 'HE.onDrag)
    , ("ondragend", 'HE.onDragEnd)
    , ("ondragenter", 'HE.onDragEnter)
    , ("ondragexit", 'HE.onDragExit)
    , ("ondragleave", 'HE.onDragLeave)
    , ("ondragover", 'HE.onDragOver)
    , ("ondragstart", 'HE.onDragStart)
    , ("ondrop", 'HE.onDrop)
    , ("onerror", 'HE.onError)
    , ("onfocus", 'HE.onFocus)
    , ("onfocusin", 'HE.onFocusIn)
    , ("onfocusout", 'HE.onFocusOut)
    , ("ongotpointercapture", 'HE.onGotPointerCapture)
    , ("oninput", 'HE.onInput)
    , ("oninputvaluechange", 'HE.onInputValueChange)
    , ("oninvalid", 'HE.onInvalid)
    , ("onkeydown", 'HE.onKeyDown)
    , ("onkeyup", 'HE.onKeyUp)
    , ("onload", 'HE.onLoad)
    , ("onlostpointercapture", 'HE.onLostPointerCapture)
    , ("onmousedown", 'HE.onMouseDown)
    , ("onmouseenter", 'HE.onMouseEnter)
    , ("onmouseleave", 'HE.onMouseLeave)
    , ("onmousemove", 'HE.onMouseMove)
    , ("onmouseout", 'HE.onMouseOut)
    , ("onmouseover", 'HE.onMouseOver)
    , ("onmouseup", 'HE.onMouseUp)
    , ("onpaste", 'HE.onPaste)
    , ("onpointercancel", 'HE.onPointerCancel)
    , ("onpointerdown", 'HE.onPointerDown)
    , ("onpointerenter", 'HE.onPointerEnter)
    , ("onpointerleave", 'HE.onPointerLeave)
    , ("onpointermove", 'HE.onPointerMove)
    , ("onpointerout", 'HE.onPointerOut)
    , ("onpointerover", 'HE.onPointerOver)
    , ("onpointerup", 'HE.onPointerUp)
    , ("onreset", 'HE.onReset)
    , ("onresize", 'HE.onResize)
    , ("onscroll", 'HE.onScroll)
    , ("onselect", 'HE.onSelect)
    , ("onselectedindexchange", 'HE.onSelectedIndexChange)
    , ("onsubmit", 'HE.onSubmit)
    , ("ontouchcancel", 'HE.onTouchCancel)
    , ("ontouchend", 'HE.onTouchEnd)
    , ("ontouchenter", 'HE.onTouchEnter)
    , ("ontouchleave", 'HE.onTouchLeave)
    , ("ontouchmove", 'HE.onTouchMove)
    , ("ontouchstart", 'HE.onTouchStart)
    , ("ontransitionend", 'HE.onTransitionEnd)
    , ("onvaluechange", 'HE.onValueChange)
    , ("onvalueinput", 'HE.onValueInput)
    , ("onwheel", 'HE.onWheel)
    ]
