module Web.HTML.Common where

import HPrelude

newtype PropName value = PropName Text
  deriving (Eq, Ord, Show, IsString)

newtype AttrName = AttrName Text
  deriving (Eq, Ord, Show, IsString)

newtype ClassName = ClassName Text
  deriving (Eq, Ord, Show, IsString)
