module DOM.HTML.Indexed where

import Data.MediaType
import Data.Row
import Data.Text qualified as T
import Data.Time
import HPrelude hiding (Any)
import Web.Event.Event
import Web.UIEvent.ClipboardEvent
import Web.UIEvent.DragEvent
import Web.UIEvent.FocusEvent
import Web.UIEvent.KeyboardEvent
import Web.UIEvent.MouseEvent
import Web.UIEvent.PointerEvent
import Web.UIEvent.TouchEvent
import Web.UIEvent.WheelEvent

--------------------------------------

data DirValue
  = DirLTR
  | DirRTL
  | DirAuto
  deriving (Eq, Ord)

renderDirValue :: DirValue -> Text
renderDirValue = \case
  DirLTR -> "ltr"
  DirRTL -> "rtl"
  DirAuto -> "auto"

--------------------------------------

data PreloadValue
  = PreloadNone
  | PreloadAuto
  | PreloadMetadata
  deriving stock (Eq, Ord)

renderPreloadValue :: PreloadValue -> Text
renderPreloadValue = \case
  PreloadNone -> "none"
  PreloadAuto -> "auto"
  PreloadMetadata -> "metadata"

--------------------------------------

data ButtonType
  = ButtonButton
  | ButtonSubmit
  | ButtonReset

renderButtonType :: ButtonType -> Text
renderButtonType = \case
  ButtonButton -> "button"
  ButtonSubmit -> "submit"
  ButtonReset -> "reset"

--------------------------------------

data AutocompleteType
  = AutocompleteOff
  | AutocompleteOn
  | AutocompleteName
  | AutocompleteHonorificPrefix
  | AutocompleteGivenName
  | AutocompleteAdditionalName
  | AutocompleteFamilyName
  | AutocompleteHonorificSuffix
  | AutocompleteNickname
  | AutocompleteEmail
  | AutocompleteUsername
  | AutocompleteNewPassword
  | AutocompleteCurrentPassword
  | AutocompleteOneTimeCode
  | AutocompleteOrganizationTitle
  | AutocompleteOrganization
  | AutocompleteStreetAddress
  | AutocompleteAddressLine1
  | AutocompleteAddressLine2
  | AutocompleteAddressLine3
  | AutocompleteAddressLevel1
  | AutocompleteAddressLevel2
  | AutocompleteAddressLevel3
  | AutocompleteAddressLevel4
  | AutocompleteCountry
  | AutocompleteCountryName
  | AutocompletePostalCode
  | AutocompleteCreditCardName
  | AutocompleteCreditCardGivenName
  | AutocompleteCreditCardAdditionalName
  | AutocompleteCreditCardFamilyName
  | AutocompleteCreditCardNumber
  | AutocompleteCreditCardExpiration
  | AutocompleteCreditCardExpirationMonth
  | AutocompleteCreditCardExpirationYear
  | AutocompleteCreditCardSecurityCode
  | AutocompleteCreditCardType
  | AutocompleteTransactionCurrency
  | AutocompleteTransactionAmount
  | AutocompleteLanguage
  | AutocompleteBirthday
  | AutocompleteBirthdayDay
  | AutocompleteBirthdayMonth
  | AutocompleteBirthdayYear
  | AutocompleteSex
  | AutocompleteTelephone
  | AutocompleteTelephoneCountryCode
  | AutocompleteTelephoneNational
  | AutocompleteTelephoneAreaCode
  | AutocompleteTelephoneLocal
  | AutocompleteTelephoneLocalPrefix
  | AutocompleteTelephoneLocalSuffix
  | AutocompleteTelephoneExtension
  | AutocompleteIMPP
  | AutocompleteURL
  | AutocompletePhoto
  deriving stock (Eq, Ord)

renderAutocompleteType :: AutocompleteType -> Text
renderAutocompleteType = \case
  AutocompleteOff -> "off"
  AutocompleteOn -> "on"
  AutocompleteName -> "name"
  AutocompleteHonorificPrefix -> "honorific-prefix"
  AutocompleteGivenName -> "given-name"
  AutocompleteAdditionalName -> "additional-name"
  AutocompleteFamilyName -> "family-name"
  AutocompleteHonorificSuffix -> "honorific-suffix"
  AutocompleteNickname -> "nickname"
  AutocompleteEmail -> "email"
  AutocompleteUsername -> "username"
  AutocompleteNewPassword -> "new-password"
  AutocompleteCurrentPassword -> "current-password"
  AutocompleteOneTimeCode -> "one-time-code"
  AutocompleteOrganizationTitle -> "organization-title"
  AutocompleteOrganization -> "organization"
  AutocompleteStreetAddress -> "street-address"
  AutocompleteAddressLine1 -> "address-line1"
  AutocompleteAddressLine2 -> "address-line2"
  AutocompleteAddressLine3 -> "address-line3"
  AutocompleteAddressLevel1 -> "address-level1"
  AutocompleteAddressLevel2 -> "address-level2"
  AutocompleteAddressLevel3 -> "address-level3"
  AutocompleteAddressLevel4 -> "address-level4"
  AutocompleteCountry -> "country"
  AutocompleteCountryName -> "country-name"
  AutocompletePostalCode -> "postal-code"
  AutocompleteCreditCardName -> "cc-name"
  AutocompleteCreditCardGivenName -> "cc-given-name"
  AutocompleteCreditCardAdditionalName -> "cc-additional-name"
  AutocompleteCreditCardFamilyName -> "cc-family-name"
  AutocompleteCreditCardNumber -> "cc-number"
  AutocompleteCreditCardExpiration -> "cc-exp"
  AutocompleteCreditCardExpirationMonth -> "cc-exp-month"
  AutocompleteCreditCardExpirationYear -> "cc-exp-year"
  AutocompleteCreditCardSecurityCode -> "cc-csc"
  AutocompleteCreditCardType -> "cc-type"
  AutocompleteTransactionCurrency -> "transaction-currency"
  AutocompleteTransactionAmount -> "transaction-amount"
  AutocompleteLanguage -> "language"
  AutocompleteBirthday -> "bday"
  AutocompleteBirthdayDay -> "bday-day"
  AutocompleteBirthdayMonth -> "bday-month"
  AutocompleteBirthdayYear -> "bday-year"
  AutocompleteSex -> "sex"
  AutocompleteTelephone -> "tel"
  AutocompleteTelephoneCountryCode -> "tel-country-code"
  AutocompleteTelephoneNational -> "tel-national"
  AutocompleteTelephoneAreaCode -> "telarea-code"
  AutocompleteTelephoneLocal -> "tel-local"
  AutocompleteTelephoneLocalPrefix -> "tel-local-prefix"
  AutocompleteTelephoneLocalSuffix -> "tel-local-suffix"
  AutocompleteTelephoneExtension -> "tel-extension"
  AutocompleteIMPP -> "impp"
  AutocompleteURL -> "url"
  AutocompletePhoto -> "photo"

--------------------------------------

data CrossOriginValue
  = Anonymous
  | UseCredentials
  deriving stock (Eq, Ord)

renderCrossOriginValue :: CrossOriginValue -> Text
renderCrossOriginValue = \case
  Anonymous -> "anonymous"
  UseCredentials -> "use-credentials"

--------------------------------------

newtype InputAcceptType = InputAcceptType [InputAcceptTypeAtom]
  deriving newtype (Eq, Ord, Semigroup, Monoid)

mediaType :: MediaType -> InputAcceptType
mediaType mt = InputAcceptType [AcceptMediaType mt]

extension :: Text -> InputAcceptType
extension ext = InputAcceptType [AcceptFileExtension ext]

data InputAcceptTypeAtom
  = AcceptMediaType MediaType
  | AcceptFileExtension Text
  deriving stock (Eq, Ord)

renderInputAcceptType :: InputAcceptType -> Text
renderInputAcceptType (InputAcceptType atoms) =
  T.intercalate "," (map renderInputAcceptTypeAtom atoms)

renderInputAcceptTypeAtom :: InputAcceptTypeAtom -> Text
renderInputAcceptTypeAtom = \case
  AcceptMediaType (MediaType mt) -> mt
  AcceptFileExtension ext -> ext

--------------------------------------

data MenuType
  = MenuList
  | MenuContext
  | MenuToolbar
  deriving stock (Eq, Ord)

renderMenuType :: MenuType -> Text
renderMenuType = \case
  MenuList -> "list"
  MenuContext -> "context"
  MenuToolbar -> "toolbar"

--------------------------------------

data MenuitemType
  = MenuitemCommand
  | MenuitemCheckbox
  | MenuitemRadio
  deriving stock (Eq, Ord)

renderMenuitemType :: MenuitemType -> Text
renderMenuitemType = \case
  MenuitemCommand -> "command"
  MenuitemCheckbox -> "checkbox"
  MenuitemRadio -> "radio"

--------------------------------------

data FormMethod
  = POST
  | GET
  deriving stock (Eq, Ord)

renderFormMethod :: FormMethod -> Text
renderFormMethod = \case
  POST -> "post"
  GET -> "get"

--------------------------------------

data InputType
  = InputButton
  | InputCheckbox
  | InputColor
  | InputDate
  | InputDatetimeLocal
  | InputEmail
  | InputFile
  | InputHidden
  | InputImage
  | InputMonth
  | InputNumber
  | InputPassword
  | InputRadio
  | InputRange
  | InputReset
  | InputSearch
  | InputSubmit
  | InputTel
  | InputText
  | InputTime
  | InputUrl
  | InputWeek
  deriving stock (Eq, Ord)

renderInputType :: InputType -> Text
renderInputType = \case
  InputButton -> "button"
  InputCheckbox -> "checkbox"
  InputColor -> "color"
  InputDate -> "date"
  InputDatetimeLocal -> "datetime-local"
  InputEmail -> "email"
  InputFile -> "file"
  InputHidden -> "hidden"
  InputImage -> "image"
  InputMonth -> "month"
  InputNumber -> "number"
  InputPassword -> "password"
  InputRadio -> "radio"
  InputRange -> "range"
  InputReset -> "reset"
  InputSearch -> "search"
  InputSubmit -> "submit"
  InputTel -> "tel"
  InputText -> "text"
  InputTime -> "time"
  InputUrl -> "url"
  InputWeek -> "week"

--------------------------------------

data StepValue
  = Any
  | Step Double
  deriving stock (Eq, Ord)

renderStepValue :: StepValue -> Text
renderStepValue = \case
  Any -> "any"
  Step n -> show n

--------------------------------------

data CaseType
  = Uppercase
  | Lowercase
  deriving stock (Eq, Ord)

data NumeralType
  = NumeralDecimal
  | NumeralRoman CaseType
  deriving stock (Eq, Ord)

data OrderedListType
  = OrderedListNumeric NumeralType
  | OrderedListAlphabetic CaseType
  deriving stock (Eq, Ord)

renderOrderedListType :: OrderedListType -> Text
renderOrderedListType = \case
  OrderedListNumeric NumeralDecimal -> "1"
  OrderedListNumeric (NumeralRoman Lowercase) -> "i"
  OrderedListNumeric (NumeralRoman Uppercase) -> "I"
  OrderedListAlphabetic Lowercase -> "a"
  OrderedListAlphabetic Uppercase -> "A"

--------------------------------------

data WrapValue
  = Hard
  | Soft
  deriving stock (Eq, Ord)

renderWrapValue :: WrapValue -> Text
renderWrapValue = \case
  Hard -> "hard"
  Soft -> "soft"

--------------------------------------

data ScopeValue
  = ScopeRow
  | ScopeCol
  | ScopeRowGroup
  | ScopeColGroup
  | ScopeAuto
  deriving stock (Eq, Ord)

renderScopeValue :: ScopeValue -> Text
renderScopeValue = \case
  ScopeRow -> "row"
  ScopeCol -> "col"
  ScopeRowGroup -> "rowgroup"
  ScopeColGroup -> "colgroup"
  ScopeAuto -> "auto"

--------------------------------------

data KindValue
  = KindSubtitles
  | KindCaptions
  | KindDescriptions
  | KindChapters
  | KindMetadata
  deriving stock (Eq, Ord)

renderKindValue :: KindValue -> Text
renderKindValue = \case
  KindSubtitles -> "subtitles"
  KindCaptions -> "captions"
  KindDescriptions -> "descriptions"
  KindChapters -> "chapters"
  KindMetadata -> "metadata"

--------------------------------------

type CSSPixel = Int

type GlobalAttributes (r :: Row Type) =
  ( "id" .== Text
      .+ "title" .== Text
      .+ "class" .== Text
      .+ "style" .== Text
      .+ "spellcheck" .== Bool
      .+ "draggable" .== Bool
      .+ "lang" .== Text
      .+ "dir" .== DirValue
      .+ "hidden" .== Bool
      .+ "tabIndex" .== Int
      .+ "accessKey" .== Text
      .+ "contentEditable" .== Bool
      .+ r
  )

type GlobalEvents r =
  ( "onContextMenu" .== Event
      .+ "onInput" .== Event
      .+ "onBeforeInput" .== Event
      .+ r
  )

type MouseEvents r =
  ( "onDoubleClick" .== MouseEvent
      .+ "onClick" .== MouseEvent
      .+ "onAuxClick" .== MouseEvent
      .+ "onMouseDown" .== MouseEvent
      .+ "onMouseEnter" .== MouseEvent
      .+ "onMouseLeave" .== MouseEvent
      .+ "onMouseMove" .== MouseEvent
      .+ "onMouseOver" .== MouseEvent
      .+ "onMouseOut" .== MouseEvent
      .+ "onMouseUp" .== MouseEvent
      .+ r
  )

type DragEvents r =
  ( "onDrag" .== DragEvent
      .+ "onDragEnd" .== DragEvent
      .+ "onDragExit" .== DragEvent
      .+ "onDragEnter" .== DragEvent
      .+ "onDragLeave" .== DragEvent
      .+ "onDragOver" .== DragEvent
      .+ "onDragStart" .== DragEvent
      .+ "onDrop" .== DragEvent
      .+ r
  )

type TouchEvents r =
  ( "onTouchCancel" .== TouchEvent
      .+ "onTouchEnd" .== TouchEvent
      .+ "onTouchEnter" .== TouchEvent
      .+ "onTouchLeave" .== TouchEvent
      .+ "onTouchMove" .== TouchEvent
      .+ "onTouchStart" .== TouchEvent
      .+ r
  )

type PointerEvents r =
  ( "onPointerOver" .== PointerEvent
      .+ "onPointerEnter" .== PointerEvent
      .+ "onPointerDown" .== PointerEvent
      .+ "onPointerMove" .== PointerEvent
      .+ "onPointerUp" .== PointerEvent
      .+ "onPointerCancel" .== PointerEvent
      .+ "onPointerOut" .== PointerEvent
      .+ "onPointerLeave" .== PointerEvent
      .+ "onGotPointerCapture" .== PointerEvent
      .+ "onLostPointerCapture" .== PointerEvent
      .+ r
  )

type KeyEvents r =
  ( "onKeyDown" .== KeyboardEvent
      .+ "onKeyUp" .== KeyboardEvent
      .+ "onKeyPress" .== KeyboardEvent
      .+ r
  )

type TransitionEvents r =
  ( "onTransitionEnd" .== Event
      .+ r
  )

type FocusEvents r =
  ( "onBlur" .== FocusEvent
      .+ "onFocus" .== FocusEvent
      .+ "onFocusIn" .== FocusEvent
      .+ "onFocusOut" .== FocusEvent
      .+ r
  )

type ClipboardEvents r =
  ( "onCopy" .== ClipboardEvent
      .+ "onCut" .== ClipboardEvent
      .+ "onPaste" .== ClipboardEvent
      .+ r
  )

type InteractiveEvents r = ClipboardEvents (FocusEvents (TransitionEvents (KeyEvents (PointerEvents (TouchEvents (DragEvents (MouseEvents ("onWheel" .== WheelEvent .+ r))))))))

type GlobalProperties r = GlobalAttributes (GlobalEvents r)

type Interactive r = InteractiveEvents (GlobalProperties r)

type Noninteractive r = GlobalProperties r

type HTMLa =
  Interactive
    ( "download" .== Text
        .+ "href" .== Text
        .+ "hrefLang" .== Text
        .+ "rel" .== Text
        .+ "target" .== Text
        .+ "type" .== MediaType
    )

type HTMLabbr = Interactive Empty

type HTMLaddress = Interactive ("onScroll" .== Event)

type HTMLarea =
  Interactive
    ( "coords" .== Text
        .+ "download" .== Text
        .+ "href" .== Text
        .+ "hrefLang" .== Text
        .+ "media" .== Text
        .+ "rel" .== Text
        .+ "shape" .== Text
        .+ "target" .== Text
        .+ "type" .== MediaType
    )

type HTMLarticle = Interactive Empty

type HTMLaside = Interactive Empty

type HTMLaudio =
  Interactive
    ( "autoplay" .== Bool
        .+ "controls" .== Bool
        .+ "loop" .== Bool
        .+ "muted" .== Bool
        .+ "preload" .== PreloadValue
        .+ "src" .== Text
        .+ "onError" .== Event
    )

type HTMLb = Interactive Empty

type HTMLbase =
  Noninteractive
    ( "href" .== Text
        .+ "target" .== Text
    )

type HTMLbdi = Interactive Empty

type HTMLbdo = Noninteractive Empty

type HTMLblockquote =
  Interactive
    ( "cite" .== Text
        .+ "onScroll" .== Event
    )

type HTMLbody =
  Interactive
    ( "onBeforeUnload" .== Event
        .+ "onHashChange" .== Event
        .+ "onLoad" .== Event
        .+ "onPageShow" .== Event
        .+ "onPageHide" .== Event
        .+ "onResize" .== Event
        .+ "onScroll" .== Event
        .+ "onUnload" .== Event
    )

type HTMLbr = Noninteractive Empty

type HTMLbutton =
  Interactive
    ( "autofocus" .== Bool
        .+ "disabled" .== Bool
        .+ "form" .== Text
        .+ "formAction" .== Text
        .+ "formEncType" .== MediaType
        .+ "formMethod" .== FormMethod
        .+ "formNoValidate" .== Bool
        .+ "formTarget" .== Text
        .+ "name" .== Text
        .+ "type" .== ButtonType
        .+ "value" .== Text
    )

type HTMLcanvas =
  Interactive
    ( "width" .== CSSPixel
        .+ "height" .== CSSPixel
    )

type HTMLcaption = Interactive ("onScroll" .== Event)

type HTMLcite = Interactive Empty

type HTMLcode = Interactive Empty

type HTMLcol = Interactive Empty

type HTMLcolgroup = Interactive ("span" .== Int)

type HTMLcommand = Interactive Empty

type HTMLdatalist = Interactive Empty

type HTMLdd = Interactive ("onScroll" .== Event)

type HTMLdel =
  Interactive
    ( "cite" .== Text
        .+ "UTCTime" .== Text
    )

type HTMLdetails = Interactive ("open" .== Bool)

type HTMLdfn = Interactive Empty

type HTMLdialog = Interactive ("open" .== Bool)

type HTMLdiv = Interactive ("onScroll" .== Event)

type HTMLdl = Interactive ("onScroll" .== Event)

type HTMLdt = Interactive ("onScroll" .== Event)

type HTMLem = Interactive Empty

type HTMLembed =
  Interactive
    ( "height" .== CSSPixel
        .+ "src" .== Text
        .+ "type" .== MediaType
        .+ "width" .== CSSPixel
    )

type HTMLfieldset =
  Interactive
    ( "disabled" .== Bool
        .+ "form" .== Text
        .+ "name" .== Text
        .+ "onScroll" .== Event
    )

type HTMLfigcaption = Interactive Empty

type HTMLfigure = Interactive Empty

type HTMLfooter = Interactive Empty

type HTMLform =
  Interactive
    ( "acceptCharset" .== Text
        .+ "action" .== Text
        .+ "autocomplete" .== AutocompleteType
        .+ "enctype" .== MediaType
        .+ "method" .== FormMethod
        .+ "name" .== Text
        .+ "noValidate" .== Bool
        .+ "onReset" .== Event
        .+ "onScroll" .== Event
        .+ "onSubmit" .== Event
        .+ "target" .== Text
    )

type HTMLh1 = Interactive ("onScroll" .== Event)

type HTMLh2 = Interactive ("onScroll" .== Event)

type HTMLh3 = Interactive ("onScroll" .== Event)

type HTMLh4 = Interactive ("onScroll" .== Event)

type HTMLh5 = Interactive ("onScroll" .== Event)

type HTMLh6 = Interactive ("onScroll" .== Event)

type HTMLhead = Noninteractive Empty

type HTMLheader = Interactive Empty

type HTMLhr = Interactive Empty

type HTMLhtml =
  Interactive
    ( "manifest" .== Text
        .+ "onScroll" .== Event
        .+ "xmlns" .== Text
    )

type HTMLi = Interactive Empty

type HTMLiframe =
  Noninteractive
    ( "height" .== CSSPixel
        .+ "name" .== Text
        .+ "onLoad" .== Event
        .+ "sandbox" .== Text
        .+ "src" .== Text
        .+ "srcDoc" .== Text
        .+ "width" .== CSSPixel
    )

type HTMLimg =
  Interactive
    ( "alt" .== Text
        .+ "crossOrigin" .== CrossOriginValue
        .+ "height" .== CSSPixel
        .+ "isMap" .== Bool
        .+ "longDesc" .== Text
        .+ "onAbort" .== Event
        .+ "onError" .== Event
        .+ "onLoad" .== Event
        .+ "src" .== Text
        .+ "useMap" .== Text
        .+ "width" .== CSSPixel
    )

type HTMLinput =
  Interactive
    ( "accept" .== InputAcceptType
        .+ "autocomplete" .== AutocompleteType
        .+ "autofocus" .== Bool
        .+ "checked" .== Bool
        .+ "disabled" .== Bool
        .+ "form" .== Text
        .+ "formAction" .== Text
        .+ "formEncType" .== MediaType
        .+ "formMethod" .== FormMethod
        .+ "formNoValidate" .== Bool
        .+ "formTarget" .== Text
        .+ "height" .== CSSPixel
        .+ "list" .== Text
        .+ "max" .== Double
        .+ "min" .== Double
        .+ "maxLength" .== Int
        .+ "minLength" .== Int
        .+ "multiple" .== Bool
        .+ "name" .== Text
        .+ "onAbort" .== Event
        .+ "onChange" .== Event
        .+ "onError" .== Event
        .+ "onInvalid" .== Event
        .+ "onLoad" .== Event
        .+ "onSearch" .== Event
        .+ "onSelect" .== Event
        .+ "pattern" .== Text
        .+ "placeholder" .== Text
        .+ "readOnly" .== Bool
        .+ "required" .== Bool
        .+ "size" .== Int
        .+ "src" .== Text
        .+ "step" .== StepValue
        .+ "type" .== InputType
        .+ "value" .== Text
        .+ "width" .== CSSPixel
    )

type HTMLins =
  Interactive
    ( "cite" .== Text
        .+ "UTCTime" .== UTCTime
    )

type HTMLkbd = Interactive Empty

type HTMLlabel =
  Interactive
    ( "for" .== Text
        .+ "form" .== Text
    )

type HTMLlegend = Interactive Empty

type HTMLli =
  Interactive
    ( "value" .== Int
        .+ "onScroll" .== Event
    )

type HTMLlink =
  Noninteractive
    ( "crossOrigin" .== CrossOriginValue
        .+ "href" .== Text
        .+ "hreflang" .== Text
        .+ "media" .== Text
        .+ "onLoad" .== Event
        .+ "rel" .== Text
        .+ "sizes" .== Text
        .+ "type" .== MediaType
    )

type HTMLmain = Interactive Empty

type HTMLmap = Interactive ("name" .== Text)

type HTMLmark = Interactive Empty

type HTMLmenu =
  Interactive
    ( "label" .== Text
        .+ "onScroll" .== Event
        .+ "type" .== MenuType
    )

type HTMLmenuitem =
  Interactive
    ( "checked" .== Bool
        .+ "command" .== Text
        .+ "default" .== Bool
        .+ "disabled" .== Bool
        .+ "icon" .== Text
        .+ "label" .== Text
        .+ "radioGroup" .== Text
        .+ "type" .== MenuitemType
    )

type HTMLmeta =
  Noninteractive
    ( "charset" .== Text
        .+ "content" .== Text
        .+ "httpEquiv" .== Text
        .+ "name" .== Text
    )

type HTMLmeter =
  Interactive
    ( "form" .== Text
        .+ "high" .== Double
        .+ "low" .== Double
        .+ "max" .== Double
        .+ "min" .== Double
        .+ "optimum" .== Double
        .+ "value" .== Double
    )

type HTMLnav = Interactive Empty

type HTMLnoscript = Interactive Empty

type HTMLobject =
  Interactive
    ( "data" .== Text
        .+ "form" .== Text
        .+ "height" .== CSSPixel
        .+ "name" .== Text
        .+ "onError" .== Event
        .+ "onScroll" .== Event
        .+ "type" .== MediaType
        .+ "useMap" .== Text
        .+ "width" .== CSSPixel
    )

type HTMLol =
  Interactive
    ( "onScroll" .== Event
        .+ "reversed" .== Bool
        .+ "start" .== Int
        .+ "type" .== OrderedListType
    )

type HTMLoptgroup =
  Interactive
    ( "disabled" .== Bool
        .+ "label" .== Text
    )

type HTMLoption =
  Interactive
    ( "disabled" .== Bool
        .+ "label" .== Text
        .+ "selected" .== Bool
        .+ "value" .== Text
    )

type HTMLoutput =
  Interactive
    ( "for" .== Text
        .+ "form" .== Text
        .+ "name" .== Text
    )

type HTMLp = Interactive ("onScroll" .== Event)

type HTMLparam =
  Noninteractive
    ( "name" .== Text
        .+ "value" .== Text
    )

type HTMLpre = Interactive ("onScroll" .== Event)

type HTMLprogress =
  Interactive
    ( "max" .== Double
        .+ "value" .== Double
    )

type HTMLq = Interactive ("cite" .== Text)

type HTMLrp = Interactive Empty

type HTMLrt = Interactive Empty

type HTMLruby = Interactive Empty

type HTMLsamp = Interactive Empty

type HTMLscript =
  Noninteractive
    ( "async" .== Bool
        .+ "charset" .== Text
        .+ "defer" .== Bool
        .+ "onError" .== Event
        .+ "onLoad" .== Event
        .+ "src" .== Text
        .+ "type" .== MediaType
    )

type HTMLsection = Interactive Empty

type HTMLselect =
  Interactive
    ( "autofocus" .== Bool
        .+ "disabled" .== Bool
        .+ "form" .== Text
        .+ "multiple" .== Bool
        .+ "name" .== Text
        .+ "onChange" .== Event
        .+ "onScroll" .== Event
        .+ "required" .== Bool
        .+ "selectedIndex" .== Int
        .+ "size" .== Int
        .+ "value" .== Text
    )

type HTMLsmall = Interactive Empty

type HTMLsource =
  Interactive
    ( "media" .== Text
        .+ "src" .== Text
        .+ "type" .== MediaType
    )

type HTMLspan = Interactive Empty

type HTMLstrong = Interactive Empty

type HTMLstyle =
  Noninteractive
    ( "media" .== Text
        .+ "onError" .== Event
        .+ "onLoad" .== Event
        .+ "scoped" .== Bool
        .+ "type" .== MediaType
    )

type HTMLsub = Interactive Empty

type HTMLsummary = Interactive Empty

type HTMLsup = Interactive Empty

type HTMLtable = Interactive ("sortable" .== Bool)

type HTMLtbody = Interactive ("onScroll" .== Event)

type HTMLtd =
  Interactive
    ( "colSpan" .== Int
        .+ "headers" .== Text
        .+ "rowSpan" .== Int
    )

type HTMLtextarea =
  Interactive
    ( "autofocus" .== Bool
        .+ "cols" .== Int
        .+ "disabled" .== Bool
        .+ "form" .== Text
        .+ "maxLength" .== Int
        .+ "name" .== Text
        .+ "onChange" .== Event
        .+ "onScroll" .== Event
        .+ "onSelect" .== Event
        .+ "placeholder" .== Text
        .+ "readOnly" .== Bool
        .+ "required" .== Bool
        .+ "rows" .== Int
        .+ "value" .== Text
        .+ "wrap" .== WrapValue
    )

type HTMLtfoot = Interactive ("onScroll" .== Event)

type HTMLth =
  Interactive
    ( "abbr" .== Text
        .+ "colSpan" .== Int
        .+ "headers" .== Text
        .+ "rowSpan" .== Int
        .+ "scope" .== ScopeValue
        .+ "sorted" .== Bool
    )

type HTMLthead = Interactive Empty

type HTMLtime = Interactive ("UTCTime" .== UTCTime)

type HTMLtitle = Noninteractive Empty

type HTMLtr = Interactive Empty

type HTMLtrack =
  Interactive
    ( "default" .== Bool
        .+ "kind" .== KindValue
        .+ "label" .== Text
        .+ "src" .== Text
        .+ "srcLang" .== Text
    )

type HTMLu = Interactive Empty

type HTMLul = Interactive ("onScroll" .== Event)

type HTMLvar = Interactive Empty

type HTMLvideo =
  Interactive
    ( "autoplay" .== Bool
        .+ "controls" .== Bool
        .+ "height" .== CSSPixel
        .+ "loop" .== Bool
        .+ "muted" .== Bool
        .+ "poster" .== Text
        .+ "preload" .== PreloadValue
        .+ "src" .== Text
        .+ "width" .== CSSPixel
        .+ "type" .== MediaType
        .+ "onError" .== Event
    )

type HTMLwbr = Interactive Empty
