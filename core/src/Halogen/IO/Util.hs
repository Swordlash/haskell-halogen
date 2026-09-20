module Halogen.IO.Util where

import HPrelude
import Halogen.VDom.DOM.Monad
import Web.DOM.Internal.Types
import Web.DOM.ParentNode
import Web.HTML.Event.EventTypes qualified as ET
import Web.HTML.HTMLDocument.ReadyState

-- | Waits for the document to load.
awaitLoad :: (MonadBrowserDOM m, MonadIO m) => m ()
awaitLoad = do
  rs <- readyState =<< document =<< window
  mvar <- liftIO newEmptyMVar
  case rs of
    Loading -> do
      et <- windowToEventTarget =<< window
      listener <- mkEventListener (const $ liftIO $ putMVar mvar ())
      addEventListener ET.domcontentloaded listener et
      liftIO $ takeMVar mvar
      removeEventListener ET.domcontentloaded listener et
    _ -> pass

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: (MonadBrowserDOM m, MonadIO m) => m HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (panic "Could not find body") pure body

-- | Tries to find an element in the document.
selectElement :: (MonadBrowserDOM m) => QuerySelector -> m (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query <=< documentToNode <=< document) =<< window
  pure $ fromElement =<< mel
