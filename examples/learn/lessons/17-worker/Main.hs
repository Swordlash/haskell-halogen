{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 17 is the one an application with Web Workers
-- ships: Worker.program serves the program's jobs in a worker (it finds each
-- by its static pointer's key, so there is no table to hand it) and, on the
-- page, goes on to mount the component, with a pool to run the jobs on. In
-- the tutorial there is no binary for a worker to start, since the page runs
-- this program in GHC's interpreter: before the compile the page names a
-- script of its own in halogenWorker instead, and the pool's worker starts a
-- second compiler from it, compiles this program and Lesson the same way
-- and runs this main there, where Worker.program serves. What it keeps for
-- the next run ends the component's tree and the pool, and so that worker.
module Main (main) where

import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Halogen.Worker qualified as Worker
import Lesson (component)
import Web.DOM.ParentNode (QuerySelector (..))

main :: IO ()
main = Worker.program $ runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  pool <- Worker.newPool 1
  socket <- runUI (component pool) () result
  HA.keepForNextRun (socket.dispose >> Worker.terminate pool)
