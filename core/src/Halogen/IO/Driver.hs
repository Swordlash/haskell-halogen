-- | Mounting a component: the driver that runs a tree of components on a
-- loop of its own ("Halogen.IO.Driver.Runtime").
module Halogen.IO.Driver
  ( RenderSpec (..)
  , ChildSlots (..)
  , runUI
  , runUIListening
  , HalogenSocket (..)
  )
where

import Halogen.IO.Driver.Mount
