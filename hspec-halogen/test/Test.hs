{-# LANGUAGE CPP #-}

-- | hspec-halogen's own suite, which is also its examples: each Example
-- module is a component, and the Test module beside it shows how it is
-- tested. They cover the harness and, through it, what haskell-halogen-core
-- does in a real browser: events, properties the page changes, keyed
-- elements, child components, refs, forks and subscriptions.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Protolude
import Test.Agreement qualified
import Test.Async qualified
import Test.Counter qualified
import Test.Digits qualified
import Test.Family qualified
import Test.Hspec.Halogen (describe, runBrowserTests)
import Test.Reconcile qualified
import Test.Todo qualified

main :: IO ()
main = runBrowserTests $ do
  describe "Counter" (Test.Counter.spec BrowserDOM)
  describe "Agreement" (Test.Agreement.spec BrowserDOM)
  describe "Digits" (Test.Digits.spec BrowserDOM)
  describe "Todo" (Test.Todo.spec BrowserDOM)
  describe "Family" (Test.Family.spec BrowserDOM)
  describe "Async" (Test.Async.spec BrowserDOM)
  describe "Reconciliation" (Test.Reconcile.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
