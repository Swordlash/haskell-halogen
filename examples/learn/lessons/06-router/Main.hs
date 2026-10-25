{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 6: it makes the lesson's router, which keeps the
-- route in the address's fragment, and hands it to the component as its
-- input. The router listens to the window's popstate events, which outlive
-- the component's tree, so it is stopped with the tree, before the next
-- compile's run starts (or the page leaves the lesson).
module Main (main) where

import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Lesson (Route (..), component, route)
import Web.DOM.ParentNode (QuerySelector (..))

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  router <- Router.newHashRouter Home route
  socket <- runUI component router result
  HA.keepForNextRun (socket.dispose >> Router.stop router)
