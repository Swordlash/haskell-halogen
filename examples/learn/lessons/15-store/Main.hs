{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 15: it makes the lesson's store and runs the
-- component's tree in it. The component's monad is any one with the store
-- (MonadStore Action Store m); runStoreT makes the store, holding
-- initialStore and changed by reduce, and hands back the tree in the
-- browser's own monad, which runUI mounts. Each compile makes a new store,
-- so the count starts again from nothing.
module Main (main) where

import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.Store qualified as Store
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Lesson (component, initialStore, reduce)
import Web.DOM.ParentNode (QuerySelector (..))

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  root <- Store.runStoreT initialStore reduce component
  socket <- runUI root () result
  HA.keepForNextRun socket.dispose
