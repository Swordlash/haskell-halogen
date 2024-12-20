module Halogen.Aff.Util where

import Control.Monad.Primitive
import Data.Primitive.MVar
import Halogen.VDom.DOM.Monad
import Protolude hiding (newEmptyMVar, putMVar, takeMVar)
import Web.DOM.Internal.Types
import Web.DOM.ParentNode
import Web.HTML.Event.EventTypes qualified as ET
import Web.HTML.HTMLDocument qualified as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState

-- | Waits for the document to load.
awaitLoad :: (PrimMonad m, MonadDOM m) => m ()
awaitLoad = do
  rs <- readyState =<< document =<< window
  mvar <- newEmptyMVar
  case rs of
    Loading -> do
      et <- toEventTarget <$> window
      listener <- mkEventListener (\_ -> putMVar mvar ())
      addEventListener ET.domcontentloaded listener et
      takeMVar mvar
      removeEventListener ET.domcontentloaded listener et
    _ -> pass

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: (MonadDOM m, PrimMonad m) => m HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (panic "Could not find body") pure body

-- | Tries to find an element in the document.
selectElement :: (MonadDOM m) => QuerySelector -> m (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query . HTMLDocument.toParentNode <=< document) =<< window
  pure $ fromElement =<< mel
