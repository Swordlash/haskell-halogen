module Halogen.Aff.Driver 
  ( RenderSpec (..)
  --, runUI
  --, module Halogen
  ) where

import Protolude
import Halogen.Query.Input
import Halogen.Component (ComponentSlot)
import qualified Halogen.HTML.Core as HC
import Halogen.Aff.Driver.State


data RenderSpec m r = RenderSpec
  { render ::
      forall s act ps o
       . (Input act -> m ())
      -> (ComponentSlot ps m act -> m (RenderStateX r))
      -> HC.HTML (ComponentSlot ps m act) act
      -> Maybe (r s act ps o)
      -> m (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> m ()
  , dispose :: forall s act ps o. r s act ps o -> m ()
  }