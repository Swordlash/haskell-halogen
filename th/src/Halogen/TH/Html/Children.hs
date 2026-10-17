{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What a @{{ … }}@ among the children of an element, or among its
-- attributes, may be. The quoter splices one in through these classes, so
-- a hole takes one node, a list of them, a text, or nothing.
module Halogen.TH.Html.Children
  ( Children (..)
  , Props (..)
  )
where

import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Core qualified as HH
import Halogen.HTML.Properties (IProp)

-- | A value among the children of an element.
class Children w i a where
  children :: a -> [HTML w i]

instance (w ~ w', i ~ i') => Children w i (HTML w' i') where
  children h = [h]

instance (Children w i a) => Children w i [a] where
  children = concatMap children

instance (Children w i a) => Children w i (Maybe a) where
  children = maybe [] children

instance Children w i Text where
  children t = [HH.text t]

instance (TypeError ('Text "A String among the children of an element: use Text, or pack it." ':$$: 'Text "A character is not a child.")) => Children w i Char where
  children = error "unreachable"

-- | A value among the attributes of an element.
class Props r i a where
  props :: a -> [IProp r i]

instance (r ~ r', i ~ i') => Props r i (IProp r' i') where
  props p = [p]

instance (Props r i a) => Props r i [a] where
  props = concatMap props

instance (Props r i a) => Props r i (Maybe a) where
  props = maybe [] props
