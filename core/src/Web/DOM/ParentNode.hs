module Web.DOM.ParentNode where

import Data.Foreign
import HPrelude
import Web.DOM.Internal.Types

newtype ParentNode = ParentNode (Foreign ParentNode)

toParentNode :: Node -> ParentNode
toParentNode = coerce

newtype QuerySelector = QuerySelector Text
