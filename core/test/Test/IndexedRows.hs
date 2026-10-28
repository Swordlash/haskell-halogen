{-# LANGUAGE TemplateHaskell #-}

-- | Test.IndexedRows checks the element rows of "DOM.HTML.Indexed" and
-- "Halogen.Svg.Indexed" against the definitions the rows had before they
-- were written as sorted lists.
--
-- Each element row is now the sorted list of labels that 'flatRow'
-- (Halogen.Row.TH) writes at compile time. The bottom of this module keeps
-- the old definitions, renamed with the prefix Old (OldSvg for the SVG
-- groups). They build each row with row-types' type families .+ and .==
-- from chains of groups. Each check states that a row and its old
-- definition are the same type; a group is checked with the empty row. A
-- label that is missing, added, misspelt, given another type or put out of
-- order makes this module fail to compile.
--
-- The checks under Halogen.Row.TH are for 'flatRow' on rows a user may
-- write: one built on an element row that is already a sorted list, and
-- one whose type synonym uses its parameter inside a field type.
module Test.IndexedRows (spec) where

import DOM.HTML.Indexed
import Data.Kind (Type)
import Data.MediaType (MediaType)
import Data.Row (Empty, Rec, Row, type (.+), type (.==))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Type.Equality ((:~:) (..))
import Halogen.Row.TH (flatRow)
import Halogen.Svg.Indexed qualified as SI
import Prelude
import Test.Hspec
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Event.Event (Event)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.PointerEvent (PointerEvent)
import Web.UIEvent.TouchEvent (TouchEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

-- | Nested is a row whose parameter is both joined to the row and part of
-- the type of the label "nested".
type Nested r = "nested" .== Rec r .+ r

-- The splice below can look up Nested only from a later declaration group.
$(pure [])

type FlatNested = $(flatRow [t|Nested ("enabled" .== Bool)|])

type FlatCustomDiv = $(flatRow [t|HTMLdiv .+ "custom" .== Text|])

spec :: Spec
spec = describe "element rows" $ do
  describe "DOM.HTML.Indexed" html
  describe "Halogen.Svg.Indexed" svg
  describe "Halogen.Row.TH" $ do
    same "an element row with a label added" (Refl :: FlatCustomDiv :~: (HTMLdiv .+ "custom" .== Text))
    same "a synonym with its parameter in a field type" (Refl :: FlatNested :~: Nested ("enabled" .== Bool))

-- | same is one check: the proof that two rows are the same type. The
-- compiler does the check; the test only names it in the report.
same :: String -> a :~: b -> Spec
same name proof = it (name <> " is the row of its old definition") $ case proof of Refl -> (pure () :: Expectation)

html :: Spec
html = do
  same "GlobalAttributes" (Refl :: GlobalAttributes Empty :~: OldGlobalAttributes Empty)
  same "GlobalEvents" (Refl :: GlobalEvents Empty :~: OldGlobalEvents Empty)
  same "MouseEvents" (Refl :: MouseEvents Empty :~: OldMouseEvents Empty)
  same "DragEvents" (Refl :: DragEvents Empty :~: OldDragEvents Empty)
  same "TouchEvents" (Refl :: TouchEvents Empty :~: OldTouchEvents Empty)
  same "PointerEvents" (Refl :: PointerEvents Empty :~: OldPointerEvents Empty)
  same "KeyEvents" (Refl :: KeyEvents Empty :~: OldKeyEvents Empty)
  same "TransitionEvents" (Refl :: TransitionEvents Empty :~: OldTransitionEvents Empty)
  same "FocusEvents" (Refl :: FocusEvents Empty :~: OldFocusEvents Empty)
  same "ClipboardEvents" (Refl :: ClipboardEvents Empty :~: OldClipboardEvents Empty)
  same "InteractiveEvents" (Refl :: InteractiveEvents Empty :~: OldInteractiveEvents Empty)
  same "GlobalProperties" (Refl :: GlobalProperties Empty :~: OldGlobalProperties Empty)
  same "Interactive" (Refl :: Interactive Empty :~: OldInteractive Empty)
  same "Noninteractive" (Refl :: Noninteractive Empty :~: OldNoninteractive Empty)
  same "HTMLa" (Refl :: HTMLa :~: OldHTMLa)
  same "HTMLabbr" (Refl :: HTMLabbr :~: OldHTMLabbr)
  same "HTMLaddress" (Refl :: HTMLaddress :~: OldHTMLaddress)
  same "HTMLarea" (Refl :: HTMLarea :~: OldHTMLarea)
  same "HTMLarticle" (Refl :: HTMLarticle :~: OldHTMLarticle)
  same "HTMLaside" (Refl :: HTMLaside :~: OldHTMLaside)
  same "HTMLaudio" (Refl :: HTMLaudio :~: OldHTMLaudio)
  same "HTMLb" (Refl :: HTMLb :~: OldHTMLb)
  same "HTMLbase" (Refl :: HTMLbase :~: OldHTMLbase)
  same "HTMLbdi" (Refl :: HTMLbdi :~: OldHTMLbdi)
  same "HTMLbdo" (Refl :: HTMLbdo :~: OldHTMLbdo)
  same "HTMLblockquote" (Refl :: HTMLblockquote :~: OldHTMLblockquote)
  same "HTMLbody" (Refl :: HTMLbody :~: OldHTMLbody)
  same "HTMLbr" (Refl :: HTMLbr :~: OldHTMLbr)
  same "HTMLbutton" (Refl :: HTMLbutton :~: OldHTMLbutton)
  same "HTMLcanvas" (Refl :: HTMLcanvas :~: OldHTMLcanvas)
  same "HTMLcaption" (Refl :: HTMLcaption :~: OldHTMLcaption)
  same "HTMLcite" (Refl :: HTMLcite :~: OldHTMLcite)
  same "HTMLcode" (Refl :: HTMLcode :~: OldHTMLcode)
  same "HTMLcol" (Refl :: HTMLcol :~: OldHTMLcol)
  same "HTMLcolgroup" (Refl :: HTMLcolgroup :~: OldHTMLcolgroup)
  same "HTMLcommand" (Refl :: HTMLcommand :~: OldHTMLcommand)
  same "HTMLdatalist" (Refl :: HTMLdatalist :~: OldHTMLdatalist)
  same "HTMLdd" (Refl :: HTMLdd :~: OldHTMLdd)
  same "HTMLdel" (Refl :: HTMLdel :~: OldHTMLdel)
  same "HTMLdetails" (Refl :: HTMLdetails :~: OldHTMLdetails)
  same "HTMLdfn" (Refl :: HTMLdfn :~: OldHTMLdfn)
  same "HTMLdialog" (Refl :: HTMLdialog :~: OldHTMLdialog)
  same "HTMLdiv" (Refl :: HTMLdiv :~: OldHTMLdiv)
  same "HTMLdl" (Refl :: HTMLdl :~: OldHTMLdl)
  same "HTMLdt" (Refl :: HTMLdt :~: OldHTMLdt)
  same "HTMLem" (Refl :: HTMLem :~: OldHTMLem)
  same "HTMLembed" (Refl :: HTMLembed :~: OldHTMLembed)
  same "HTMLfieldset" (Refl :: HTMLfieldset :~: OldHTMLfieldset)
  same "HTMLfigcaption" (Refl :: HTMLfigcaption :~: OldHTMLfigcaption)
  same "HTMLfigure" (Refl :: HTMLfigure :~: OldHTMLfigure)
  same "HTMLfooter" (Refl :: HTMLfooter :~: OldHTMLfooter)
  same "HTMLform" (Refl :: HTMLform :~: OldHTMLform)
  same "HTMLh1" (Refl :: HTMLh1 :~: OldHTMLh1)
  same "HTMLh2" (Refl :: HTMLh2 :~: OldHTMLh2)
  same "HTMLh3" (Refl :: HTMLh3 :~: OldHTMLh3)
  same "HTMLh4" (Refl :: HTMLh4 :~: OldHTMLh4)
  same "HTMLh5" (Refl :: HTMLh5 :~: OldHTMLh5)
  same "HTMLh6" (Refl :: HTMLh6 :~: OldHTMLh6)
  same "HTMLhead" (Refl :: HTMLhead :~: OldHTMLhead)
  same "HTMLheader" (Refl :: HTMLheader :~: OldHTMLheader)
  same "HTMLhr" (Refl :: HTMLhr :~: OldHTMLhr)
  same "HTMLhtml" (Refl :: HTMLhtml :~: OldHTMLhtml)
  same "HTMLi" (Refl :: HTMLi :~: OldHTMLi)
  same "HTMLiframe" (Refl :: HTMLiframe :~: OldHTMLiframe)
  same "HTMLimg" (Refl :: HTMLimg :~: OldHTMLimg)
  same "HTMLinput" (Refl :: HTMLinput :~: OldHTMLinput)
  same "HTMLins" (Refl :: HTMLins :~: OldHTMLins)
  same "HTMLkbd" (Refl :: HTMLkbd :~: OldHTMLkbd)
  same "HTMLlabel" (Refl :: HTMLlabel :~: OldHTMLlabel)
  same "HTMLlegend" (Refl :: HTMLlegend :~: OldHTMLlegend)
  same "HTMLli" (Refl :: HTMLli :~: OldHTMLli)
  same "HTMLlink" (Refl :: HTMLlink :~: OldHTMLlink)
  same "HTMLmain" (Refl :: HTMLmain :~: OldHTMLmain)
  same "HTMLmap" (Refl :: HTMLmap :~: OldHTMLmap)
  same "HTMLmark" (Refl :: HTMLmark :~: OldHTMLmark)
  same "HTMLmenu" (Refl :: HTMLmenu :~: OldHTMLmenu)
  same "HTMLmenuitem" (Refl :: HTMLmenuitem :~: OldHTMLmenuitem)
  same "HTMLmeta" (Refl :: HTMLmeta :~: OldHTMLmeta)
  same "HTMLmeter" (Refl :: HTMLmeter :~: OldHTMLmeter)
  same "HTMLnav" (Refl :: HTMLnav :~: OldHTMLnav)
  same "HTMLnoscript" (Refl :: HTMLnoscript :~: OldHTMLnoscript)
  same "HTMLobject" (Refl :: HTMLobject :~: OldHTMLobject)
  same "HTMLol" (Refl :: HTMLol :~: OldHTMLol)
  same "HTMLoptgroup" (Refl :: HTMLoptgroup :~: OldHTMLoptgroup)
  same "HTMLoption" (Refl :: HTMLoption :~: OldHTMLoption)
  same "HTMLoutput" (Refl :: HTMLoutput :~: OldHTMLoutput)
  same "HTMLp" (Refl :: HTMLp :~: OldHTMLp)
  same "HTMLparam" (Refl :: HTMLparam :~: OldHTMLparam)
  same "HTMLpre" (Refl :: HTMLpre :~: OldHTMLpre)
  same "HTMLprogress" (Refl :: HTMLprogress :~: OldHTMLprogress)
  same "HTMLq" (Refl :: HTMLq :~: OldHTMLq)
  same "HTMLrp" (Refl :: HTMLrp :~: OldHTMLrp)
  same "HTMLrt" (Refl :: HTMLrt :~: OldHTMLrt)
  same "HTMLruby" (Refl :: HTMLruby :~: OldHTMLruby)
  same "HTMLsamp" (Refl :: HTMLsamp :~: OldHTMLsamp)
  same "HTMLscript" (Refl :: HTMLscript :~: OldHTMLscript)
  same "HTMLsection" (Refl :: HTMLsection :~: OldHTMLsection)
  same "HTMLselect" (Refl :: HTMLselect :~: OldHTMLselect)
  same "HTMLsmall" (Refl :: HTMLsmall :~: OldHTMLsmall)
  same "HTMLsource" (Refl :: HTMLsource :~: OldHTMLsource)
  same "HTMLspan" (Refl :: HTMLspan :~: OldHTMLspan)
  same "HTMLstrong" (Refl :: HTMLstrong :~: OldHTMLstrong)
  same "HTMLstyle" (Refl :: HTMLstyle :~: OldHTMLstyle)
  same "HTMLsub" (Refl :: HTMLsub :~: OldHTMLsub)
  same "HTMLsummary" (Refl :: HTMLsummary :~: OldHTMLsummary)
  same "HTMLsup" (Refl :: HTMLsup :~: OldHTMLsup)
  same "HTMLtable" (Refl :: HTMLtable :~: OldHTMLtable)
  same "HTMLtbody" (Refl :: HTMLtbody :~: OldHTMLtbody)
  same "HTMLtd" (Refl :: HTMLtd :~: OldHTMLtd)
  same "HTMLtextarea" (Refl :: HTMLtextarea :~: OldHTMLtextarea)
  same "HTMLtfoot" (Refl :: HTMLtfoot :~: OldHTMLtfoot)
  same "HTMLth" (Refl :: HTMLth :~: OldHTMLth)
  same "HTMLthead" (Refl :: HTMLthead :~: OldHTMLthead)
  same "HTMLtime" (Refl :: HTMLtime :~: OldHTMLtime)
  same "HTMLtitle" (Refl :: HTMLtitle :~: OldHTMLtitle)
  same "HTMLtr" (Refl :: HTMLtr :~: OldHTMLtr)
  same "HTMLtrack" (Refl :: HTMLtrack :~: OldHTMLtrack)
  same "HTMLu" (Refl :: HTMLu :~: OldHTMLu)
  same "HTMLul" (Refl :: HTMLul :~: OldHTMLul)
  same "HTMLvar" (Refl :: HTMLvar :~: OldHTMLvar)
  same "HTMLvideo" (Refl :: HTMLvideo :~: OldHTMLvideo)
  same "HTMLwbr" (Refl :: HTMLwbr :~: OldHTMLwbr)

svg :: Spec
svg = do
  same "CoreAttributes" (Refl :: SI.CoreAttributes Empty :~: OldSvgCoreAttributes Empty)
  same "GlobalEventAttributes" (Refl :: SI.GlobalEventAttributes Empty :~: OldSvgGlobalEventAttributes Empty)
  same "GlobalAttributes" (Refl :: SI.GlobalAttributes Empty :~: OldSvgGlobalAttributes Empty)
  same "StrokeAttributes" (Refl :: SI.StrokeAttributes Empty :~: OldSvgStrokeAttributes Empty)
  same "StokeEndAttributes" (Refl :: SI.StokeEndAttributes Empty :~: OldSvgStokeEndAttributes Empty)
  same "StrokeJoinAttributes" (Refl :: SI.StrokeJoinAttributes Empty :~: OldSvgStrokeJoinAttributes Empty)
  same "FillAttributes" (Refl :: SI.FillAttributes Empty :~: OldSvgFillAttributes Empty)
  same "MarkerAttributes" (Refl :: SI.MarkerAttributes Empty :~: OldSvgMarkerAttributes Empty)
  same "FontAttributes" (Refl :: SI.FontAttributes Empty :~: OldSvgFontAttributes Empty)
  same "CanBeMaskedAttributes" (Refl :: SI.CanBeMaskedAttributes Empty :~: OldSvgCanBeMaskedAttributes Empty)
  same "AllPresentationAttributes" (Refl :: SI.AllPresentationAttributes Empty :~: OldSvgAllPresentationAttributes Empty)
  same "SVGsvg" (Refl :: SI.SVGsvg :~: OldSVGsvg)
  same "SVGg" (Refl :: SI.SVGg :~: OldSVGg)
  same "SVGforeignObject" (Refl :: SI.SVGforeignObject :~: OldSVGforeignObject)
  same "SVGmarker" (Refl :: SI.SVGmarker :~: OldSVGmarker)
  same "SVGmask" (Refl :: SI.SVGmask :~: OldSVGmask)
  same "SVGcircle" (Refl :: SI.SVGcircle :~: OldSVGcircle)
  same "SVGellipse" (Refl :: SI.SVGellipse :~: OldSVGellipse)
  same "SVGline" (Refl :: SI.SVGline :~: OldSVGline)
  same "SVGpolyline" (Refl :: SI.SVGpolyline :~: OldSVGpolyline)
  same "SVGpolygon" (Refl :: SI.SVGpolygon :~: OldSVGpolygon)
  same "SVGpath" (Refl :: SI.SVGpath :~: OldSVGpath)
  same "SVGpattern" (Refl :: SI.SVGpattern :~: OldSVGpattern)
  same "SVGrect" (Refl :: SI.SVGrect :~: OldSVGrect)
  same "SVGtext" (Refl :: SI.SVGtext :~: OldSVGtext)
  same "SVGuse" (Refl :: SI.SVGuse :~: OldSVGuse)
  same "AnimationAttributes" (Refl :: SI.AnimationAttributes Empty :~: OldSvgAnimationAttributes Empty)
  same "SVGanimate" (Refl :: SI.SVGanimate :~: OldSVGanimate)
  same "SVGanimateMotion" (Refl :: SI.SVGanimateMotion :~: OldSVGanimateMotion)
  same "SVGanimateTransform" (Refl :: SI.SVGanimateTransform :~: OldSVGanimateTransform)
  same "SVGset" (Refl :: SI.SVGset :~: OldSVGset)
  same "SVGimage" (Refl :: SI.SVGimage :~: OldSVGimage)
  same "SVGmpath" (Refl :: SI.SVGmpath :~: OldSVGmpath)
  same "SVGtitle" (Refl :: SI.SVGtitle :~: OldSVGtitle)

-- The old definitions of "DOM.HTML.Indexed".

type OldGlobalAttributes (r :: Row Type) = ("id" .== Text .+ "title" .== Text .+ "class" .== Text .+ "style" .== Text .+ "spellcheck" .== Bool .+ "draggable" .== Bool .+ "lang" .== Text .+ "dir" .== DirValue .+ "hidden" .== Bool .+ "tabIndex" .== Int .+ "accessKey" .== Text .+ "contentEditable" .== Bool .+ r)

type OldGlobalEvents r = ("onContextMenu" .== Event .+ "onInput" .== Event .+ "onBeforeInput" .== Event .+ r)

type OldMouseEvents r = ("onDoubleClick" .== MouseEvent .+ "onClick" .== MouseEvent .+ "onAuxClick" .== MouseEvent .+ "onMouseDown" .== MouseEvent .+ "onMouseEnter" .== MouseEvent .+ "onMouseLeave" .== MouseEvent .+ "onMouseMove" .== MouseEvent .+ "onMouseOver" .== MouseEvent .+ "onMouseOut" .== MouseEvent .+ "onMouseUp" .== MouseEvent .+ r)

type OldDragEvents r = ("onDrag" .== DragEvent .+ "onDragEnd" .== DragEvent .+ "onDragExit" .== DragEvent .+ "onDragEnter" .== DragEvent .+ "onDragLeave" .== DragEvent .+ "onDragOver" .== DragEvent .+ "onDragStart" .== DragEvent .+ "onDrop" .== DragEvent .+ r)

type OldTouchEvents r = ("onTouchCancel" .== TouchEvent .+ "onTouchEnd" .== TouchEvent .+ "onTouchEnter" .== TouchEvent .+ "onTouchLeave" .== TouchEvent .+ "onTouchMove" .== TouchEvent .+ "onTouchStart" .== TouchEvent .+ r)

type OldPointerEvents r = ("onPointerOver" .== PointerEvent .+ "onPointerEnter" .== PointerEvent .+ "onPointerDown" .== PointerEvent .+ "onPointerMove" .== PointerEvent .+ "onPointerUp" .== PointerEvent .+ "onPointerCancel" .== PointerEvent .+ "onPointerOut" .== PointerEvent .+ "onPointerLeave" .== PointerEvent .+ "onGotPointerCapture" .== PointerEvent .+ "onLostPointerCapture" .== PointerEvent .+ r)

type OldKeyEvents r = ("onKeyDown" .== KeyboardEvent .+ "onKeyUp" .== KeyboardEvent .+ "onKeyPress" .== KeyboardEvent .+ r)

type OldTransitionEvents r = ("onTransitionEnd" .== Event .+ r)

type OldFocusEvents r = ("onBlur" .== FocusEvent .+ "onFocus" .== FocusEvent .+ "onFocusIn" .== FocusEvent .+ "onFocusOut" .== FocusEvent .+ r)

type OldClipboardEvents r = ("onCopy" .== ClipboardEvent .+ "onCut" .== ClipboardEvent .+ "onPaste" .== ClipboardEvent .+ r)

type OldInteractiveEvents r = OldClipboardEvents (OldFocusEvents (OldTransitionEvents (OldKeyEvents (OldPointerEvents (OldTouchEvents (OldDragEvents (OldMouseEvents ("onWheel" .== WheelEvent .+ r))))))))

type OldGlobalProperties r = OldGlobalAttributes (OldGlobalEvents r)

type OldInteractive r = OldInteractiveEvents (OldGlobalProperties r)

type OldNoninteractive r = OldGlobalProperties r

type OldHTMLa = OldInteractive ("download" .== Text .+ "href" .== Text .+ "hrefLang" .== Text .+ "rel" .== Text .+ "target" .== Text .+ "type" .== MediaType)

type OldHTMLabbr = OldInteractive Empty

type OldHTMLaddress = OldInteractive ("onScroll" .== Event)

type OldHTMLarea = OldInteractive ("coords" .== Text .+ "download" .== Text .+ "href" .== Text .+ "hrefLang" .== Text .+ "media" .== Text .+ "rel" .== Text .+ "shape" .== Text .+ "target" .== Text .+ "type" .== MediaType)

type OldHTMLarticle = OldInteractive Empty

type OldHTMLaside = OldInteractive Empty

type OldHTMLaudio = OldInteractive ("autoplay" .== Bool .+ "controls" .== Bool .+ "loop" .== Bool .+ "muted" .== Bool .+ "preload" .== PreloadValue .+ "src" .== Text .+ "onError" .== Event)

type OldHTMLb = OldInteractive Empty

type OldHTMLbase = OldNoninteractive ("href" .== Text .+ "target" .== Text)

type OldHTMLbdi = OldInteractive Empty

type OldHTMLbdo = OldNoninteractive Empty

type OldHTMLblockquote = OldInteractive ("cite" .== Text .+ "onScroll" .== Event)

type OldHTMLbody = OldInteractive ("onBeforeUnload" .== Event .+ "onHashChange" .== Event .+ "onLoad" .== Event .+ "onPageShow" .== Event .+ "onPageHide" .== Event .+ "onResize" .== Event .+ "onScroll" .== Event .+ "onUnload" .== Event)

type OldHTMLbr = OldNoninteractive Empty

type OldHTMLbutton = OldInteractive ("autofocus" .== Bool .+ "disabled" .== Bool .+ "form" .== Text .+ "formAction" .== Text .+ "formEncType" .== MediaType .+ "formMethod" .== FormMethod .+ "formNoValidate" .== Bool .+ "formTarget" .== Text .+ "name" .== Text .+ "type" .== ButtonType .+ "value" .== Text)

type OldHTMLcanvas = OldInteractive ("width" .== CSSPixel .+ "height" .== CSSPixel)

type OldHTMLcaption = OldInteractive ("onScroll" .== Event)

type OldHTMLcite = OldInteractive Empty

type OldHTMLcode = OldInteractive Empty

type OldHTMLcol = OldInteractive Empty

type OldHTMLcolgroup = OldInteractive ("span" .== Int)

type OldHTMLcommand = OldInteractive Empty

type OldHTMLdatalist = OldInteractive Empty

type OldHTMLdd = OldInteractive ("onScroll" .== Event)

type OldHTMLdel = OldInteractive ("cite" .== Text .+ "UTCTime" .== Text)

type OldHTMLdetails = OldInteractive ("open" .== Bool)

type OldHTMLdfn = OldInteractive Empty

type OldHTMLdialog = OldInteractive ("open" .== Bool)

type OldHTMLdiv = OldInteractive ("onScroll" .== Event)

type OldHTMLdl = OldInteractive ("onScroll" .== Event)

type OldHTMLdt = OldInteractive ("onScroll" .== Event)

type OldHTMLem = OldInteractive Empty

type OldHTMLembed = OldInteractive ("height" .== CSSPixel .+ "src" .== Text .+ "type" .== MediaType .+ "width" .== CSSPixel)

type OldHTMLfieldset = OldInteractive ("disabled" .== Bool .+ "form" .== Text .+ "name" .== Text .+ "onScroll" .== Event)

type OldHTMLfigcaption = OldInteractive Empty

type OldHTMLfigure = OldInteractive Empty

type OldHTMLfooter = OldInteractive Empty

type OldHTMLform = OldInteractive ("acceptCharset" .== Text .+ "action" .== Text .+ "autocomplete" .== AutocompleteType .+ "enctype" .== MediaType .+ "method" .== FormMethod .+ "name" .== Text .+ "noValidate" .== Bool .+ "onReset" .== Event .+ "onScroll" .== Event .+ "onSubmit" .== Event .+ "target" .== Text)

type OldHTMLh1 = OldInteractive ("onScroll" .== Event)

type OldHTMLh2 = OldInteractive ("onScroll" .== Event)

type OldHTMLh3 = OldInteractive ("onScroll" .== Event)

type OldHTMLh4 = OldInteractive ("onScroll" .== Event)

type OldHTMLh5 = OldInteractive ("onScroll" .== Event)

type OldHTMLh6 = OldInteractive ("onScroll" .== Event)

type OldHTMLhead = OldNoninteractive Empty

type OldHTMLheader = OldInteractive Empty

type OldHTMLhr = OldInteractive Empty

type OldHTMLhtml = OldInteractive ("manifest" .== Text .+ "onScroll" .== Event .+ "xmlns" .== Text)

type OldHTMLi = OldInteractive Empty

type OldHTMLiframe = OldNoninteractive ("height" .== CSSPixel .+ "name" .== Text .+ "onLoad" .== Event .+ "sandbox" .== Text .+ "src" .== Text .+ "srcDoc" .== Text .+ "width" .== CSSPixel)

type OldHTMLimg = OldInteractive ("alt" .== Text .+ "crossOrigin" .== CrossOriginValue .+ "height" .== CSSPixel .+ "isMap" .== Bool .+ "longDesc" .== Text .+ "onAbort" .== Event .+ "onError" .== Event .+ "onLoad" .== Event .+ "src" .== Text .+ "useMap" .== Text .+ "width" .== CSSPixel)

type OldHTMLinput = OldInteractive ("accept" .== InputAcceptType .+ "autocomplete" .== AutocompleteType .+ "autofocus" .== Bool .+ "checked" .== Bool .+ "disabled" .== Bool .+ "form" .== Text .+ "formAction" .== Text .+ "formEncType" .== MediaType .+ "formMethod" .== FormMethod .+ "formNoValidate" .== Bool .+ "formTarget" .== Text .+ "height" .== CSSPixel .+ "list" .== Text .+ "max" .== Double .+ "min" .== Double .+ "maxLength" .== Int .+ "minLength" .== Int .+ "multiple" .== Bool .+ "name" .== Text .+ "onAbort" .== Event .+ "onChange" .== Event .+ "onError" .== Event .+ "onInvalid" .== Event .+ "onLoad" .== Event .+ "onSearch" .== Event .+ "onSelect" .== Event .+ "pattern" .== Text .+ "placeholder" .== Text .+ "readOnly" .== Bool .+ "required" .== Bool .+ "size" .== Int .+ "src" .== Text .+ "step" .== StepValue .+ "type" .== InputType .+ "value" .== Text .+ "width" .== CSSPixel)

type OldHTMLins = OldInteractive ("cite" .== Text .+ "UTCTime" .== UTCTime)

type OldHTMLkbd = OldInteractive Empty

type OldHTMLlabel = OldInteractive ("for" .== Text .+ "form" .== Text)

type OldHTMLlegend = OldInteractive Empty

type OldHTMLli = OldInteractive ("value" .== Int .+ "onScroll" .== Event)

type OldHTMLlink = OldNoninteractive ("crossOrigin" .== CrossOriginValue .+ "href" .== Text .+ "hreflang" .== Text .+ "media" .== Text .+ "onLoad" .== Event .+ "rel" .== Text .+ "sizes" .== Text .+ "type" .== MediaType)

type OldHTMLmain = OldInteractive Empty

type OldHTMLmap = OldInteractive ("name" .== Text)

type OldHTMLmark = OldInteractive Empty

type OldHTMLmenu = OldInteractive ("label" .== Text .+ "onScroll" .== Event .+ "type" .== MenuType)

type OldHTMLmenuitem = OldInteractive ("checked" .== Bool .+ "command" .== Text .+ "default" .== Bool .+ "disabled" .== Bool .+ "icon" .== Text .+ "label" .== Text .+ "radioGroup" .== Text .+ "type" .== MenuitemType)

type OldHTMLmeta = OldNoninteractive ("charset" .== Text .+ "content" .== Text .+ "httpEquiv" .== Text .+ "name" .== Text)

type OldHTMLmeter = OldInteractive ("form" .== Text .+ "high" .== Double .+ "low" .== Double .+ "max" .== Double .+ "min" .== Double .+ "optimum" .== Double .+ "value" .== Double)

type OldHTMLnav = OldInteractive Empty

type OldHTMLnoscript = OldInteractive Empty

type OldHTMLobject = OldInteractive ("data" .== Text .+ "form" .== Text .+ "height" .== CSSPixel .+ "name" .== Text .+ "onError" .== Event .+ "onScroll" .== Event .+ "type" .== MediaType .+ "useMap" .== Text .+ "width" .== CSSPixel)

type OldHTMLol = OldInteractive ("onScroll" .== Event .+ "reversed" .== Bool .+ "start" .== Int .+ "type" .== OrderedListType)

type OldHTMLoptgroup = OldInteractive ("disabled" .== Bool .+ "label" .== Text)

type OldHTMLoption = OldInteractive ("disabled" .== Bool .+ "label" .== Text .+ "selected" .== Bool .+ "value" .== Text)

type OldHTMLoutput = OldInteractive ("for" .== Text .+ "form" .== Text .+ "name" .== Text)

type OldHTMLp = OldInteractive ("onScroll" .== Event)

type OldHTMLparam = OldNoninteractive ("name" .== Text .+ "value" .== Text)

type OldHTMLpre = OldInteractive ("onScroll" .== Event)

type OldHTMLprogress = OldInteractive ("max" .== Double .+ "value" .== Double)

type OldHTMLq = OldInteractive ("cite" .== Text)

type OldHTMLrp = OldInteractive Empty

type OldHTMLrt = OldInteractive Empty

type OldHTMLruby = OldInteractive Empty

type OldHTMLsamp = OldInteractive Empty

type OldHTMLscript = OldNoninteractive ("async" .== Bool .+ "charset" .== Text .+ "defer" .== Bool .+ "onError" .== Event .+ "onLoad" .== Event .+ "src" .== Text .+ "type" .== MediaType)

type OldHTMLsection = OldInteractive Empty

type OldHTMLselect = OldInteractive ("autofocus" .== Bool .+ "disabled" .== Bool .+ "form" .== Text .+ "multiple" .== Bool .+ "name" .== Text .+ "onChange" .== Event .+ "onScroll" .== Event .+ "required" .== Bool .+ "selectedIndex" .== Int .+ "size" .== Int .+ "value" .== Text)

type OldHTMLsmall = OldInteractive Empty

type OldHTMLsource = OldInteractive ("media" .== Text .+ "src" .== Text .+ "type" .== MediaType)

type OldHTMLspan = OldInteractive Empty

type OldHTMLstrong = OldInteractive Empty

type OldHTMLstyle = OldNoninteractive ("media" .== Text .+ "onError" .== Event .+ "onLoad" .== Event .+ "scoped" .== Bool .+ "type" .== MediaType)

type OldHTMLsub = OldInteractive Empty

type OldHTMLsummary = OldInteractive Empty

type OldHTMLsup = OldInteractive Empty

type OldHTMLtable = OldInteractive ("sortable" .== Bool)

type OldHTMLtbody = OldInteractive ("onScroll" .== Event)

type OldHTMLtd = OldInteractive ("colSpan" .== Int .+ "headers" .== Text .+ "rowSpan" .== Int)

type OldHTMLtextarea = OldInteractive ("autofocus" .== Bool .+ "cols" .== Int .+ "disabled" .== Bool .+ "form" .== Text .+ "maxLength" .== Int .+ "name" .== Text .+ "onChange" .== Event .+ "onScroll" .== Event .+ "onSelect" .== Event .+ "placeholder" .== Text .+ "readOnly" .== Bool .+ "required" .== Bool .+ "rows" .== Int .+ "value" .== Text .+ "wrap" .== WrapValue)

type OldHTMLtfoot = OldInteractive ("onScroll" .== Event)

type OldHTMLth = OldInteractive ("abbr" .== Text .+ "colSpan" .== Int .+ "headers" .== Text .+ "rowSpan" .== Int .+ "scope" .== ScopeValue .+ "sorted" .== Bool)

type OldHTMLthead = OldInteractive Empty

type OldHTMLtime = OldInteractive ("UTCTime" .== UTCTime)

type OldHTMLtitle = OldNoninteractive Empty

type OldHTMLtr = OldInteractive Empty

type OldHTMLtrack = OldInteractive ("default" .== Bool .+ "kind" .== KindValue .+ "label" .== Text .+ "src" .== Text .+ "srcLang" .== Text)

type OldHTMLu = OldInteractive Empty

type OldHTMLul = OldInteractive ("onScroll" .== Event)

type OldHTMLvar = OldInteractive Empty

type OldHTMLvideo = OldInteractive ("autoplay" .== Bool .+ "controls" .== Bool .+ "height" .== CSSPixel .+ "loop" .== Bool .+ "muted" .== Bool .+ "poster" .== Text .+ "preload" .== PreloadValue .+ "src" .== Text .+ "width" .== CSSPixel .+ "type" .== MediaType .+ "onError" .== Event)

type OldHTMLwbr = OldInteractive Empty

-- The old definitions of "Halogen.Svg.Indexed".

type OldSvgCoreAttributes r = ("id" .== Text .+ "class" .== Text .+ "style" .== Text .+ "tabIndex" .== Int .+ "lang" .== Text .+ r)

type OldSvgGlobalEventAttributes r = ("onClick" .== MouseEvent .+ "onFocus" .== FocusEvent .+ "onBlur" .== FocusEvent .+ "onFocusIn" .== FocusEvent .+ "onFocusOut" .== FocusEvent .+ "onTouchStart" .== TouchEvent .+ "onTouchEnd" .== TouchEvent .+ "onTouchMove" .== TouchEvent .+ "onTouchCancel" .== TouchEvent .+ "onTransitionEnd" .== Event .+ "onDoubleClick" .== MouseEvent .+ "onContextMenu" .== MouseEvent .+ "onKeyDown" .== KeyboardEvent .+ "onKeyPress" .== KeyboardEvent .+ "onKeyUp" .== KeyboardEvent .+ "onMouseDown" .== MouseEvent .+ "onMouseEnter" .== MouseEvent .+ "onMouseLeave" .== MouseEvent .+ "onMouseMove" .== MouseEvent .+ "onMouseOut" .== MouseEvent .+ "onMouseOver" .== MouseEvent .+ "onMouseUp" .== MouseEvent .+ "onWheel" .== WheelEvent .+ "onPointerOver" .== PointerEvent .+ "onPointerEnter" .== PointerEvent .+ "onPointerDown" .== PointerEvent .+ "onPointerMove" .== PointerEvent .+ "onPointerUp" .== PointerEvent .+ "onPointerCancel" .== PointerEvent .+ "onPointerOut" .== PointerEvent .+ "onPointerLeave" .== PointerEvent .+ "onGotPointerCapture" .== PointerEvent .+ "onLostPointerCapture" .== PointerEvent .+ "onDrag" .== DragEvent .+ "onDragEnd" .== DragEvent .+ "onDragEnter" .== DragEvent .+ "onDragExit" .== DragEvent .+ "onDragLeave" .== DragEvent .+ "onDragOver" .== DragEvent .+ "onDragStart" .== DragEvent .+ "onDrop" .== DragEvent .+ r)

type OldSvgGlobalAttributes r = OldSvgCoreAttributes (OldSvgGlobalEventAttributes r)

type OldSvgStrokeAttributes r = ("stroke" .== Text .+ "strokeDashArray" .== Text .+ "strokeDashOffset" .== Double .+ "strokeOpacity" .== Double .+ "strokeWidth" .== Double .+ r)

type OldSvgStokeEndAttributes r = ("strokeLineCap" .== Text .+ r)

type OldSvgStrokeJoinAttributes r = ("strokeLineJoin" .== Text .+ "strokeMiterLimit" .== Text .+ r)

type OldSvgFillAttributes r = ("fill" .== Text .+ "fillOpacity" .== Double .+ r)

type OldSvgMarkerAttributes r = ("markerStart" .== Text .+ "markerMid" .== Text .+ "markerEnd" .== Text .+ r)

type OldSvgFontAttributes r = ("fontFamily" .== Text .+ "fontSize" .== Text .+ "fontSizeAdjust" .== Double .+ "fontStretch" .== Text .+ "fontStyle" .== Text .+ "fontVariant" .== Text .+ "fontWeight" .== Text .+ r)

type OldSvgCanBeMaskedAttributes r = ("mask" .== Text .+ r)

type OldSvgAllPresentationAttributes r = OldSvgStrokeAttributes (OldSvgStrokeJoinAttributes (OldSvgStokeEndAttributes (OldSvgFillAttributes (OldSvgFontAttributes (OldSvgMarkerAttributes (OldSvgCanBeMaskedAttributes r))))))

type OldSVGsvg = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("width" .== Double .+ "height" .== Double .+ "viewBox" .== Text .+ "preserveAspectRatio" .== Text .+ "transform" .== Text .+ "onLoad" .== Event .+ "onError" .== Event))

type OldSVGg = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("transform" .== Text))

type OldSVGforeignObject = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("x" .== Double .+ "y" .== Double .+ "height" .== Double .+ "width" .== Double .+ "transform" .== Text))

type OldSVGmarker = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("markerWidth" .== Double .+ "markerHeight" .== Double .+ "refX" .== Double .+ "refY" .== Double .+ "orient" .== Text .+ "markerUnits" .== Text .+ "viewBox" .== Text .+ "preserveAspectRatio" .== Text))

type OldSVGmask = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("transform" .== Text .+ "x" .== Double .+ "y" .== Double .+ "width" .== Double .+ "height" .== Double .+ "maskUnits" .== Text .+ "maskContentUnits" .== Text))

type OldSVGcircle = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("cx" .== Double .+ "cy" .== Double .+ "r" .== Double .+ "transform" .== Text .+ "pathLength" .== Double))

type OldSVGellipse = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("cx" .== Double .+ "cy" .== Double .+ "rx" .== Double .+ "ry" .== Double .+ "transform" .== Text .+ "pathLength" .== Double))

type OldSVGline = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("x1" .== Double .+ "y1" .== Double .+ "x2" .== Double .+ "y2" .== Double .+ "transform" .== Text .+ "pathLength" .== Double))

type OldSVGpolyline = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("points" .== Text .+ "pathLength" .== Double .+ "transform" .== Text))

type OldSVGpolygon = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("points" .== Text .+ "pathLength" .== Double .+ "transform" .== Text))

type OldSVGpath = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("d" .== Text .+ "transform" .== Text .+ "pathLength" .== Double))

type OldSVGpattern = OldSvgGlobalAttributes ("height" .== Double .+ "href" .== Text .+ "patternContentUnits" .== Text .+ "patternTransform" .== Text .+ "patternUnits" .== Text .+ "preserveAspectRatio" .== Text .+ "viewBox" .== Text .+ "width" .== Double .+ "x" .== Double .+ "xlinkHref" .== Text .+ "y" .== Double)

type OldSVGrect = OldSvgGlobalAttributes (OldSvgAllPresentationAttributes ("x" .== Double .+ "y" .== Double .+ "rx" .== Double .+ "ry" .== Double .+ "width" .== Double .+ "height" .== Double .+ "transform" .== Text .+ "pathLength" .== Double))

type OldSVGtext = OldSvgGlobalAttributes (OldSvgCanBeMaskedAttributes (OldSvgStrokeAttributes (OldSvgStokeEndAttributes (OldSvgStrokeJoinAttributes (OldSvgFillAttributes (OldSvgFontAttributes ("x" .== Double .+ "y" .== Double .+ "textAnchor" .== Text .+ "dominantBaseline" .== Text .+ "transform" .== Text)))))))

type OldSVGuse = OldSvgGlobalAttributes (OldSvgCanBeMaskedAttributes (OldSvgStrokeAttributes (OldSvgStokeEndAttributes (OldSvgStrokeJoinAttributes (OldSvgFillAttributes (OldSvgFontAttributes ("x" .== Double .+ "y" .== Double .+ "width" .== Double .+ "height" .== Double .+ "transform" .== Text .+ "href" .== Text .+ "xlinkHref" .== Text)))))))

type OldSvgAnimationAttributes r = OldSvgGlobalAttributes ("from" .== Text .+ "to" .== Text .+ "begin" .== Text .+ "dur" .== Text .+ "repeatCount" .== Text .+ "fill" .== Text .+ "href" .== Text .+ "xlinkHref" .== Text .+ r)

type OldSVGanimate = OldSvgAnimationAttributes ("attributeName" .== Text)

type OldSVGanimateMotion = OldSvgAnimationAttributes ("path" .== Text)

type OldSVGanimateTransform = OldSvgAnimationAttributes ("attributeName" .== Text .+ "type" .== Text .+ "by" .== Text)

type OldSVGset = OldSvgAnimationAttributes ("attributeName" .== Text)

type OldSVGimage = OldSvgGlobalAttributes ("x" .== Double .+ "y" .== Double .+ "width" .== Double .+ "height" .== Double .+ "href" .== Text .+ "xlinkHref" .== Text .+ "preserveAspectRatio" .== Text .+ "transform" .== Text .+ "onLoad" .== Event .+ "onError" .== Event)

type OldSVGmpath = OldSvgGlobalAttributes ("href" .== Text .+ "xlinkHref" .== Text)

type OldSVGtitle = OldSvgGlobalAttributes Empty
