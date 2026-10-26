{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 17 is the one an application with Web Workers
-- ships: Worker.program serves the lesson's jobs in a worker and, on the
-- page, goes on to mount the component, with a pool to run the jobs on. In
-- the tutorial there is no binary for a worker to start (the page runs this
-- program in GHC's interpreter, and says nothing in halogenWorker), so the
-- pool starts none and the jobs run on the page. What it keeps for the next
-- run ends the component's tree and the pool.
module Main (main) where

import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Halogen.Worker qualified as Worker
import Lesson (component, jobs)
import Web.DOM.ParentNode (QuerySelector (..))

main :: IO ()
main = Worker.program jobs $ runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  pool <- Worker.newPool 1
  socket <- runUI (component pool) () result
  HA.keepForNextRun (socket.dispose >> Worker.terminate pool)
