{-# LANGUAGE CPP #-}

module Web.DOM.Internal.Types where

import Data.Foreign (Foreign)
import HPrelude
import Unsafe.Coerce (unsafeCoerce)

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim
import GHC.JS.Foreign.Callback
#endif

newtype Node = Node (Foreign Node)

newtype NodeList = NodeList (Foreign NodeList)

newtype Element = Element (Foreign Element)

newtype HTMLElement = HTMLElement (Foreign HTMLElement)

newtype HTMLCollection = HTMLCollection (Foreign HTMLCollection)

#if defined(javascript_HOST_ARCH)
newtype EventListener = EventListener (Callback (JSVal -> IO ()))
#else
newtype EventListener = EventListener (Foreign EventListener)
#endif

newtype Document = Document (Foreign Document)

newtype HTMLDocument = HTMLDocument (Foreign HTMLDocument)

newtype Window = Window (Foreign Window)

fromElement :: Element -> Maybe HTMLElement
fromElement = Just . unsafeCoerce

toDocument :: a -> Document
toDocument = unsafeCoerce

toNode :: a -> Node
toNode = unsafeCoerce
