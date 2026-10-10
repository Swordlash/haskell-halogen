module Web.DOM.Internal.Types where

import Data.Foreign (Foreign)
import Halogen.JSBits (Callback)
import HPrelude
import Unsafe.Coerce (unsafeCoerce)

newtype Node = Node (Foreign Node)

newtype NodeList = NodeList (Foreign NodeList)

newtype Element = Element (Foreign Element)

newtype HTMLElement = HTMLElement (Foreign HTMLElement)

newtype HTMLCollection = HTMLCollection (Foreign HTMLCollection)

newtype EventListener = EventListener Callback

newtype Document = Document (Foreign Document)

newtype HTMLDocument = HTMLDocument (Foreign HTMLDocument)

newtype Window = Window (Foreign Window)

fromElement :: Element -> Maybe HTMLElement
fromElement = Just . coerce

toDocument :: a -> Document
toDocument = unsafeCoerce

toNode :: a -> Node
toNode = unsafeCoerce
