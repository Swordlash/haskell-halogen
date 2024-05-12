module Web.DOM.Internal.Types where

import Data.Foreign (Foreign)

newtype Node = Node (Foreign Node)

newtype NodeList = NodeList (Foreign NodeList)

newtype Element = Element (Foreign Element)

newtype HTMLCollection = HTMLCollection (Foreign HTMLCollection)
