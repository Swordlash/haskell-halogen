{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 13 runs the lesson's spec instead of mounting
-- its component: hspec-halogen's runBrowserTestsIn, with every test's page
-- in #learn-result, where the reader watches the component being tested.
-- There is no Playwright behind this page, so the harness clicks and types
-- with synthetic events of the page's own. hspec's report goes to stdout,
-- which the page shows under the editor. Each test takes down what it
-- mounted, so nothing is left for the next compile to end; the last
-- lesson's tree is ended first, as every wrapper does.
module Main (main) where

import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Lesson (spec)
import Test.Hspec.Halogen (runBrowserTestsIn)
import Web.DOM.ParentNode (QuerySelector (..))

main :: IO ()
main = do
  result <- runBrowserDOM $ do
    HA.endPreviousRun
    HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  runBrowserTestsIn result (spec BrowserDOM)
