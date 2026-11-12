-- The worker layer's program in the playground's test (../Playground.hs,
-- which says what it checks): static jobs of haskell-halogen-worker, run in
-- a Web Worker that is a second compiler.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

module Main (main) where

import Control.Exception (throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (put)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.StaticPtr (StaticPtr)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.IO.Util qualified as HA
import Halogen.JSBits.Promise (mapConcurrently)
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Halogen.Worker qualified as Worker
import Halogen.Worker.Par (runPar, task)
import Web.DOM.ParentNode (QuerySelector (..))

double :: StaticPtr (Worker.Job Int Int)
double = static (Worker.job (\n -> pure (n * 2)))

failing :: StaticPtr (Worker.Job () Int)
failing = static (Worker.job (\() -> throwIO (userError "no luck")))

whereAmI :: StaticPtr (Worker.Job () (Maybe Int))
whereAmI = static (Worker.job (\() -> Worker.whichWorker))

report :: Worker.Pool -> H.Component q () o BrowserDOM
report pool = H.component (\_ -> pure "") (\t -> HH.p_ [HH.text t]) [H.onInitialize initialize]
  where
    initialize = do
      available <- Worker.available
      ranIn <- liftIO (Worker.run pool whereAmI ())
      doubled <- liftIO (Worker.run pool double 21)
      many <- liftIO (mapConcurrently (Worker.start pool double) [1, 2, 3])
      tasks <- liftIO (runPar pool (traverse (task double) [4, 5, 6]))
      failed <- liftIO (try (Worker.run pool failing ()))
      let failure = either (\(e :: Worker.WorkerError) -> e.job <> ": " <> e.message) (T.pack . show) failed
      put ("available " <> T.pack (show available) <> "; ran in " <> T.pack (show ranIn) <> "; doubled 21 to " <> T.pack (show doubled) <> "; doubled 1, 2, 3 to " <> T.pack (show many) <> "; in tasks 4, 5, 6 to " <> T.pack (show tasks) <> "; " <> failure :: Text)

main :: IO ()
main = Worker.program $ runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  pool <- Worker.newPool 1
  socket <- runUI (report pool) () app
  HA.keepForNextRun (socket.dispose >> Worker.terminate pool)
