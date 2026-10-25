{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 10. Material's components call functions that
-- the material layer's material.js puts on globalThis, and look as its
-- material.css says (the layer's "assets" in rootfs/rootfs.json). The page
-- does not add a layer's assets itself, so this adds them to the document's
-- head, once, and mounts the component when both have loaded: a component
-- mounted before material.js has run fails to start.
module Main (main) where

import Control.Monad (when)
import Data.Text (Text)
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
  asset "link" "href" "rootfs/layers/material/material.css" $
    asset "script" "src" "rootfs/layers/material/material.js" $ do
      socket <- runUI component () result
      HA.keepForNextRun socket.dispose

-- Run the program once the element <tag attr="url"> in the document has
-- loaded, adding it to the head first if it is not there.
asset :: Text -> Text -> Text -> BrowserDOM () -> BrowserDOM ()
asset tag attr url next = do
  doc <- document =<< window
  root <- documentToNode doc
  let selector = tag <> "[" <> attr <> "=\"" <> url <> "\"]"
  querySelector (QuerySelector (selector <> "[data-loaded]")) root >>= \case
    Just _ -> next
    Nothing -> do
      element <- querySelector (QuerySelector selector) root >>= maybe (create doc root) pure
      loaded <- mkEventListener (\_ -> setAttribute Nothing (AttrName "data-loaded") "" element >> next)
      addEventListener (EventType "load") loaded =<< elementToEventTarget element
  where
    create doc root = do
      element <- createElement Nothing (ElemName tag) (toDocument doc)
      when (tag == "link") (setAttribute Nothing (AttrName "rel") "stylesheet" element)
      setAttribute Nothing (AttrName attr) url element
      parent <- querySelector (QuerySelector "head") root >>= maybe (error "the page has no head") pure
      child <- elementToNode element
      appendChild child =<< elementToNode parent
      pure element
