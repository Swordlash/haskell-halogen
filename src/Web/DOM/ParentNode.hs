module Web.DOM.ParentNode where

import Data.Foreign
import HPrelude
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types

newtype ParentNode = ParentNode (Foreign ParentNode)

toParentNode :: Node -> ParentNode
toParentNode = unsafeCoerce

newtype QuerySelector = QuerySelector Text
