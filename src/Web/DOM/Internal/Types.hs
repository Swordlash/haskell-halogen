module Web.DOM.Internal.Types where

import Data.Foreign (Foreign)
import Protolude
import Unsafe.Coerce (unsafeCoerce)

newtype Node = Node (Foreign Node)

newtype NodeList = NodeList (Foreign NodeList)

newtype Element = Element (Foreign Element)

newtype HTMLElement = HTMLElement (Foreign HTMLElement)

newtype HTMLCollection = HTMLCollection (Foreign HTMLCollection)

newtype EventListener = EventListener (Foreign EventListener)

newtype Document = Document (Foreign Document)

newtype HTMLDocument = HMTLDocument (Foreign Document)

newtype Window = Window (Foreign Window)

fromElement :: Element -> Maybe HTMLElement
fromElement = Just . unsafeCoerce

toDocument :: a -> Document
toDocument = unsafeCoerce

toNode :: a -> Node
toNode = unsafeCoerce
