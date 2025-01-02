module Halogen.Material.List
  ( ElemRenderer (..)
  , ElemTextRenderer (..)
  , ListCfg (..)
  , ListElem (..)
  , list
  )
where

import Data.Foreign
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Material.Icons
import Halogen.VDom.DOM.Monad
import Protolude hiding (list, log)
import Web.DOM.Internal.Types (HTMLElement)

newtype MDCList = MDCList (Foreign MDCList)

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "window.Halogen.init_material_list" initList :: HTMLElement -> IO MDCList
#else
initList :: HTMLElement -> IO MDCList
initList _ = panic "can only be run in JS"
#endif

data ElemTextRenderer a
  = Oneline (a -> Text)
  | Twoline (a -> (Text, Text))

data ElemRenderer a w i = ElemRenderer
  { iconRenderer :: Maybe (a -> HH.HTML w i)
  , textRenderer :: ElemTextRenderer a
  , metaRenderer :: Maybe (a -> HH.HTML w i)
  }

data ListElem a
  = ListElem a
  | Separator

data ListCfg a w i = ListCfg
  { items :: [ListElem a]
  , elemRenderer :: ElemRenderer a w i
  }

data ListState a w i = ListState
  { listElement :: Maybe HTMLElement
  , mdcList :: Maybe MDCList
  , items :: [ListElem a]
  , elemRenderer :: ElemRenderer a w i
  }

list :: (MonadIO m, MonadDOM m) => H.Component H.VoidF (ListCfg a w i) () m
list =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \ListCfg {..} -> ListState {listElement = Nothing, mdcList = Nothing, ..}
      , render = \_ -> HH.div_ mempty
      , eval = H.mkEval $ H.defaultEval
      }
