{-# LANGUAGE TemplateHaskell #-}

-- | The DOM's types, and the fields of the window, the document and
-- elements ("Halogen.JSBits.Fields"): here, beside the newtypes, so that
-- the instances are not orphans. The same fields as functions are in the
-- module of each type, meant to be imported qualified: "Web.HTML.Window",
-- "Web.HTML.HTMLDocument", "Web.DOM.Element", "Web.HTML.HTMLElement".
module Web.DOM.Internal.Types
  ( Node (..)
  , NodeList (..)
  , Element (..)
  , HTMLElement (..)
  , HTMLCollection (..)
  , EventListener (..)
  , Document (..)
  , HTMLDocument (..)
  , Window (..)
  , fromElement
  , toDocument
  , toNode
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (Location)
import Halogen.JSBits (Callback, Inert)
import Halogen.JSBits.Fields (Member (..), jsFields)
import Unsafe.Coerce (unsafeCoerce)
import Web.Console (Console (..))
import Web.Crypto (Crypto (..))
import Web.HTML.History (History (..))
import Web.HTML.Location (Location (..))
import Web.HTML.Navigator (Navigator (..))

newtype Node = Node (Foreign Node)
  deriving newtype (Inert)

newtype NodeList = NodeList (Foreign NodeList)
  deriving newtype (Inert)

newtype Element = Element (Foreign Element)
  deriving newtype (Inert)

newtype HTMLElement = HTMLElement (Foreign HTMLElement)
  deriving newtype (Inert)

newtype HTMLCollection = HTMLCollection (Foreign HTMLCollection)
  deriving newtype (Inert)

newtype EventListener = EventListener Callback

newtype Document = Document (Foreign Document)
  deriving newtype (Inert)

newtype HTMLDocument = HTMLDocument (Foreign HTMLDocument)
  deriving newtype (Inert)

newtype Window = Window (Foreign Window)
  deriving newtype (Inert)

fromElement :: Element -> Maybe HTMLElement
fromElement = Just . coerce

toDocument :: a -> Document
toDocument = unsafeCoerce

toNode :: a -> Node
toNode = unsafeCoerce

$( jsFields
     ''Window
     [ -- The objects a window keeps for as long as the page lives.
       Fixed "document" [t|HTMLDocument|]
     , Fixed "navigator" [t|Navigator|]
     , Fixed "history" [t|History|]
     , Fixed "location" [t|Location|]
     , Fixed "crypto" [t|Crypto|]
     , Fixed "console" [t|Console|]
     , -- Whether the page is a secure context (HTTPS, or a page on
       -- localhost), which some APIs ask for: the clipboard,
       -- 'Web.Crypto.randomUUID'.
       Fixed "isSecureContext" [t|Bool|]
     , -- The size of the viewport, in CSS pixels.
       Live "innerWidth" [t|IO Int|]
     , Live "innerHeight" [t|IO Int|]
     , -- The browser's print dialog, from which a page can also be saved
       -- as a PDF. It returns once the dialog has closed.
       Method "print" [t|IO ()|]
     ]
 )

$( jsFields
     ''HTMLDocument
     [ -- The title the browser shows in its tab and history.
       Writable "title" [t|IO Text|]
     , -- Every cookie the document can see, as pairs separated by @"; "@;
       -- setting it writes one cookie ("Web.HTML.Cookie").
       Writable "cookie" [t|IO Text|]
     , -- @"visible"@ or @"hidden"@ ("Web.HTML.HTMLDocument.visibilityState"
       -- reads it as a 'Web.HTML.HTMLDocument.VisibilityState.VisibilityState').
       Live "visibilityState" [t|IO Text|]
     , -- The address relative links are read against: the page's, or its
       -- @\<base href>@.
       Live "baseURI" [t|IO Text|]
     , -- The element shown full screen, if one is.
       Live "fullscreenElement" [t|IO (Maybe Element)|]
     , -- The first element a CSS selector matches. A selector that does not
       -- parse throws a 'Halogen.JSBits.Promise.JSError' (a @SyntaxError@).
       Method "querySelector" [t|Text -> IO (Maybe Element)|]
     , Method "getElementById" [t|Text -> IO (Maybe Element)|]
     , -- Leave full screen. Rejected when nothing is full screen.
       Awaited "exitFullscreen" [t|IO ()|]
     ]
 )

$( jsFields
     ''Element
     [ -- The text of the element and everything in it; setting it replaces
       -- everything in it with that text.
       Writable "textContent" [t|IO Text|]
     , Method "getAttribute" [t|Text -> IO (Maybe Text)|]
     , Method "hasAttribute" [t|Text -> IO Bool|]
     , -- The element itself or its nearest ancestor that a CSS selector
       -- matches. A selector that does not parse throws a
       -- 'Halogen.JSBits.Promise.JSError' (a @SyntaxError@), as for 'querySelector'.
       Method "closest" [t|Text -> IO (Maybe Element)|]
     , Method "querySelector" [t|Text -> IO (Maybe Element)|]
     , -- Scroll the page (and whatever scrolls around the element) until
       -- the element is in view.
       Method "scrollIntoView" [t|IO ()|]
     ]
 )

$( jsFields
     ''HTMLElement
     [ Method "focus" [t|IO ()|]
     , Method "blur" [t|IO ()|]
     , -- A click, as if the reader had clicked: its handlers run before
       -- this returns.
       Method "click" [t|IO ()|]
     ]
 )
