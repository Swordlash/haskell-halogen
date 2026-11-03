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
  , Range (..)
  , Selection (..)
  , fromElement
  , toDocument
  , toNode
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (Location)
import Halogen.JSBits (Callback, Inert)
import Halogen.JSBits.Fields (Member (..), jsFields)
import Halogen.JSBits.Promise (Promise (..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Cache (CacheStorage (..))
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

-- | A @Range@: a stretch of the document, between two places in it
-- ("Web.DOM.Range").
newtype Range = Range (Foreign Range)
  deriving newtype (Inert)

-- | The reader's selection in the page, @window.getSelection()@
-- ("Web.Selection").
newtype Selection = Selection (Foreign Selection)
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
       Fixed "document" ''HTMLDocument
     , Fixed "navigator" ''Navigator
     , Fixed "history" ''History
     , Fixed "location" ''Location
     , Fixed "crypto" ''Crypto
     , Fixed "console" ''Console
     , -- Whether the page is a secure context (HTTPS, or a page on
       -- localhost), which some APIs ask for: the clipboard,
       -- 'Web.Crypto.randomUUID'.
       Fixed "isSecureContext" ''Bool
     , -- The size of the viewport, in CSS pixels.
       Live "innerWidth" ''Int
     , Live "innerHeight" ''Int
     , -- The browser's print dialog, from which a page can also be saved
       -- as a PDF. It returns once the dialog has closed.
       Method "print" [t|IO ()|]
     , -- What the reader has selected in the page, or 'Nothing' in a
       -- document that has no selection (one not shown).
       Method "getSelection" [t|IO (Maybe Selection)|]
     , -- The caches a service worker keeps ("Web.Cache"), which only a
       -- secure context (HTTPS, or a page on localhost) has.
       Live "caches" [t|Maybe CacheStorage|]
     ]
 )

$( jsFields
     ''HTMLDocument
     [ -- The title the browser shows in its tab and history.
       Writable "title" ''Text
     , -- Every cookie the document can see, as pairs separated by @"; "@;
       -- setting it writes one cookie ("Web.HTML.Cookie").
       Writable "cookie" ''Text
     , -- @"visible"@ or @"hidden"@ ("Web.HTML.HTMLDocument.visibilityState"
       -- reads it as a 'Web.HTML.HTMLDocument.VisibilityState.VisibilityState').
       Live "visibilityState" ''Text
     , -- The address relative links are read against: the page's, or its
       -- @\<base href>@.
       Live "baseURI" ''Text
     , -- The element shown full screen, if one is.
       Live "fullscreenElement" [t|Maybe Element|]
     , -- The first element a CSS selector matches. A selector that does not
       -- parse throws a 'Halogen.JSBits.Promise.JSError' (a @SyntaxError@).
       Method "querySelector" [t|Text -> IO (Maybe Element)|]
     , Method "getElementById" [t|Text -> IO (Maybe Element)|]
     , -- Leave full screen. Rejected when nothing is full screen.
       Method "exitFullscreen" [t|IO (Promise ())|]
     , -- The @\<head>@, which a page has from the moment its document is
       -- parsed.
       Fixed "head" ''HTMLElement
     , -- The @\<body>@, or 'Nothing' while the document is still being
       -- parsed.
       Live "body" [t|Maybe HTMLElement|]
     , -- The element that has the focus, or 'Nothing' when none has.
       Live "activeElement" [t|Maybe Element|]
     , -- A new element of a tag name, not yet in the document.
       Method "createElement" [t|Text -> IO Element|]
     , -- A new, collapsed 'Range'.
       Method "createRange" [t|IO Range|]
     , -- An editing command on the document, such as @"copy"@, which copies
       -- the selection; whether the browser carried it out. Deprecated by
       -- the browsers, but the one way to copy where
       -- 'Web.Clipboard.writeText' is not there.
       Method "execCommand" [t|Text -> IO Bool|]
     ]
 )

$( jsFields
     ''Element
     [ -- The text of the element and everything in it; setting it replaces
       -- everything in it with that text.
       Writable "textContent" ''Text
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
     , -- Whether the element is in a document.
       Live "isConnected" ''Bool
     , -- The next element among its siblings, or 'Nothing' for the last.
       Live "nextElementSibling" [t|Maybe Element|]
     , -- Set an attribute, adding it or replacing its value.
       Method "setAttribute" [t|Text -> Text -> IO ()|]
     , -- Whether a node is the element itself or inside it.
       Method "contains" [t|Node -> IO Bool|]
     , -- Put a node inside the element, after its last child; the node
       -- moves if it was elsewhere. The node, as the browser returns it.
       Method "appendChild" [t|Node -> IO Node|]
     , -- Show the element full screen. The browser rejects it outside a
       -- user gesture, or where a frame does not allow it.
       Method "requestFullscreen" [t|IO (Promise ())|]
     , -- Take the element out of the document.
       Method "remove" [t|IO ()|]
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

$( jsFields
     ''Range
     [ -- Make the range cover everything inside a node.
       Method "selectNodeContents" [t|Node -> IO ()|]
     ]
 )

$( jsFields
     ''Selection
     [ -- Deselect everything.
       Method "removeAllRanges" [t|IO ()|]
     , -- Select a range, besides what is selected.
       Method "addRange" [t|Range -> IO ()|]
     ]
 )
