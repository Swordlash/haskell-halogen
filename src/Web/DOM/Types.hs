module Web.DOM.Types where

import Protolude


newtype ElemName = ElemName Text
  deriving (Eq, Ord, Show, IsString)

newtype Namespace = Namespace Text
  deriving (Eq, Ord, Show, IsString)

newtype EventType = EventType Text
  deriving (Eq, Ord, Show, IsString)

newtype PropName = PropName Text
  deriving (Eq, Ord, Show, IsString)

newtype AttrName = AttrName Text
  deriving (Eq, Ord, Show, IsString)