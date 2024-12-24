module Web.HTML.HTMLDocument where

import Data.Coerce
import Web.DOM.Internal.Types
import Web.DOM.ParentNode

toParentNode :: HTMLDocument -> ParentNode
toParentNode = coerce
