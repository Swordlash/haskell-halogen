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
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Test.Busy qualified
import Test.Islands qualified
import Test.Window qualified
#endif
import Test.Counter qualified
import Test.Digits qualified
import Test.Dispatch qualified
import Test.Drag qualified
import Test.Editing qualified
import Test.Family qualified
import Test.Hspec.Halogen (describe, parallel, runBrowserTests)
import Test.Hydrate qualified
import Test.InPage qualified
import Test.IndexedDB qualified
import Test.Promised qualified
import Test.Reconcile qualified
import Test.Timeline qualified
import Test.Todo qualified

-- Natively BrowserDOM has no MonadBrowserDOM, which this spec asks for.
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Test.LastRun qualified
#endif

main :: IO ()
main = runBrowserTests $ parallel $ do
  describe "Counter" (Test.Counter.spec BrowserDOM)
  describe "Agreement" (Test.Agreement.spec BrowserDOM)
  describe "Digits" (Test.Digits.spec BrowserDOM)
  describe "Todo" (Test.Todo.spec BrowserDOM)
  describe "Family" (Test.Family.spec BrowserDOM)
  describe "Async" (Test.Async.spec BrowserDOM)
  describe "Reconciliation" (Test.Reconcile.spec BrowserDOM)
  describe "Dispatch" (Test.Dispatch.spec BrowserDOM)
  describe "Drag" (Test.Drag.spec BrowserDOM)
  describe "Editing" (Test.Editing.spec BrowserDOM)
  describe "Hydration" (Test.Hydrate.spec BrowserDOM)
  describe "IndexedDB" (Test.IndexedDB.spec BrowserDOM)
  describe "Promised" (Test.Promised.spec BrowserDOM)
  describe "Without the runner" (Test.InPage.spec BrowserDOM)
  describe "A failure's timeline" (Test.Timeline.spec BrowserDOM)
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
  -- These need the browser's DOM ('MonadBrowserDOM'), which 'BrowserDOM'
  -- has only in a browser.
  describe "Busy" (Test.Busy.spec BrowserDOM)
  describe "Islands" (Test.Islands.spec BrowserDOM)
  describe "Embedded state" (Test.Hydrate.embedSpec BrowserDOM)
  describe "Refs across a replacement" (Test.Reconcile.refSpec BrowserDOM)
  describe "Running again" (Test.LastRun.spec BrowserDOM)
  describe "Window" (Test.Window.spec BrowserDOM)
  describe "Dispatch to a subscription" (Test.Dispatch.subscribedSpec BrowserDOM)
#endif

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
