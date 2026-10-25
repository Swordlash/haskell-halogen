{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 12 stands in for a prerendered page: it puts
-- into #learn-result the HTML that Halogen.Static.renderToHtml writes for
-- Start.hs's component, then hydrates the reader's component over it, as a
-- page's program does. Each element of that HTML has a dashed outline, an
-- attribute the component does not render: hydration leaves such attributes
-- as they are, so an element the component adopted keeps its outline, and
-- one it built afresh has none.
module Main (main) where

import Data.Text (Text)
import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (PropValue (..), nodeToElement, runBrowserDOM, setProperty)
import Halogen.VDom.Driver (hydrateUI)
import Lesson (component)
import Web.DOM.Internal.Types (toNode)
import Web.DOM.ParentNode (QuerySelector (..))
import Web.HTML.Common (PropName (..))

-- renderToHtml's output for Start.hs, with a style attribute on each element.
prerendered :: Text
prerendered =
  "<div style=\"" <> outline <> "\"><p style=\"" <> outline <> "\">Count: <!---->0</p><button style=\"" <> outline <> "\">+</button></div>"
  where
    outline = "outline: 2px dashed #16a34a; outline-offset: 2px"

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  element <- nodeToElement (toNode result) >>= maybe (error "#learn-result is not an element") pure
  setProperty (PropName "innerHTML") (TxtProp prerendered) element
  socket <- hydrateUI component () result
  HA.keepForNextRun socket.dispose
