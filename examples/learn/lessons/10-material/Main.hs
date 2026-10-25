{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 10. Material's components call functions that
-- the material layer's material.js puts on globalThis, and look as its
-- material.css says (the layer's "assets" in rootfs/rootfs.json). The page
-- does not add a layer's assets itself, so this adds them to the document's
-- head, once, waits for both to load, and only then mounts the component.
module Main (main) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName (..))
import Lesson (component)
import Web.DOM.Internal.Types (toDocument)
import Web.DOM.ParentNode (QuerySelector (..))
import Web.Event.Event (EventType (..))
import Web.HTML.Common (AttrName (..))

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  -- Both assets must have loaded before the component mounts: a component
  -- mounted before material.js has run fails to start. Waiting here, before
  -- main returns, also keeps a second compile or a change of lesson from
  -- finding the mount still to come.
  asset "link" "href" "rootfs/layers/material/material.css"
  asset "script" "src" "rootfs/layers/material/material.js"
  socket <- runUI component () result
  HA.keepForNextRun socket.dispose

-- Wait until the element <tag attr="url"> in the document has loaded,
-- adding it to the head first if it is not there.
asset :: Text -> Text -> Text -> BrowserDOM ()
asset tag attr url = do
  doc <- document =<< window
  root <- documentToNode doc
  let selector = tag <> "[" <> attr <> "=\"" <> url <> "\"]"
  querySelector (QuerySelector (selector <> "[data-loaded]")) root >>= \case
    Just _ -> pure ()
    Nothing -> do
      element <- querySelector (QuerySelector selector) root >>= maybe (create doc root) pure
      done <- liftIO newEmptyMVar
      loaded <- mkEventListener (\_ -> setAttribute Nothing (AttrName "data-loaded") "" element >> liftIO (putMVar done True))
      -- An element whose request failed is taken out again, so that the
      -- next compile makes a new one and a new request, instead of waiting
      -- on one that will never load.
      failed <- mkEventListener $ \_ -> do
        child <- elementToNode element
        parent <- querySelector (QuerySelector "head") root >>= maybe (error "the page has no head") pure
        removeChild child =<< elementToNode parent
        liftIO (putMVar done False)
      target <- elementToEventTarget element
      addEventListener (EventType "load") loaded target
      addEventListener (EventType "error") failed target
      ok <- liftIO (takeMVar done)
      unless ok (error ("could not load " <> T.unpack url))
  where
    create doc root = do
      element <- createElement Nothing (ElemName tag) (toDocument doc)
      when (tag == "link") (setAttribute Nothing (AttrName "rel") "stylesheet" element)
      setAttribute Nothing (AttrName attr) url element
      parent <- querySelector (QuerySelector "head") root >>= maybe (error "the page has no head") pure
      child <- elementToNode element
      appendChild child =<< elementToNode parent
      pure element
