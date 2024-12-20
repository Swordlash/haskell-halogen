module Web.HTML.HTMLDocument where

import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types
import Web.DOM.ParentNode

toParentNode :: HTMLDocument -> ParentNode
toParentNode = unsafeCoerce
