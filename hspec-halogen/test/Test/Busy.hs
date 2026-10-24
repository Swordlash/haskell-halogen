{-# LANGUAGE CPP #-}

module Test.Busy (spec) where

import Example.Busy qualified as Busy
import Halogen qualified as H
import Halogen.Router qualified as Router
import Halogen.Router.Duplex qualified as RD
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  it "keeps the browser from following a router link clicked during a render" $ runPage $ do
    (router, ui) <- mountIt m
    clickDuringRender ui Busy.RouterLink
    find ui ".busy-outcome" >>= (`shouldHaveText` "prevented")
    -- The action still runs, on the loop, once the render is over.
    find ui ".busy-handled" >>= (`shouldHaveText` "RouterLink")
    unsafeIOToPageM (Router.current router) `shouldReturn` Just 1

  it "is too late to prevent the default from the action of a click made during a render" $ runPage $ do
    (_, ui) <- mountIt m
    clickDuringRender ui Busy.ActionPrevents
    find ui ".busy-outcome" >>= (`shouldHaveText` "followed")
    find ui ".busy-handled" >>= (`shouldHaveText` "ActionPrevents")

  it "prevents the default in the listener when the handler declares it" $ runPage $ do
    (_, ui) <- mountIt m
    clickDuringRender ui Busy.ListenerPrevents
    find ui ".busy-outcome" >>= (`shouldHaveText` "prevented")
    find ui ".busy-handled" >>= (`shouldHaveText` "ListenerPrevents")

  it "leaves the default alone when the handler makes no action" $ runPage $ do
    (_, ui) <- mountIt m
    clickDuringRender ui Busy.ListenerIgnores
    find ui ".busy-outcome" >>= (`shouldHaveText` "followed")

  it "stops the propagation in the listener when the handler declares it" $ runPage $ do
    (_, ui) <- mountIt m
    clickDuringRender ui Busy.ListenerStops
    find ui ".busy-outcome" >>= (`shouldHaveText` "stopped")
    find ui ".busy-handled" >>= (`shouldHaveText` "ListenerStops")

  -- Only on WebAssembly: the JavaScript backend cannot run the loop's other
  -- threads inside a dispatch, so a click there finds the loop held by
  -- nothing runnable, and the test would not show anything.
  onWasm "lets the loop's preempted work finish inside the dispatch, so the action prevents the default (WebAssembly only)" $ runPage $ do
    (_, ui) <- mountIt m
    find ui ".busy-preempted" >>= click
    find ui ".busy-preempted-outcome" >>= (`shouldHaveText` "prevented")
    -- The click's action ran after the render it found under way, not
    -- inside it.
    find ui ".busy-handled" >>= (`shouldHaveText` "Filled ActionPrevents")

  it "follows a router link a user clicks, without loading a page" $ runPage $ do
    (router, ui) <- mountIt m
    find ui ("." <> Busy.linkClass Busy.RouterLink) >>= click
    find ui ".busy-handled" >>= (`shouldHaveText` "RouterLink")
    unsafeIOToPageM (Router.current router) `shouldReturn` Just 1

onWasm :: [Char] -> IO () -> Spec
#if defined(javascript_HOST_ARCH)
onWasm = xit
#else
onWasm = it
#endif

-- | The component, with a hash router of its own.
mountIt :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => PageM s (Router.Router Busy.Route, Mounted s H.VoidF Void m)
mountIt m = do
  router <- unsafeIOToPageM (runTest @m (Router.newHashRouter Nothing (RD.root (RD.optional (RD.int RD.segment)))))
  ui <- mount m (Busy.component :: H.Component H.VoidF (Router.Router Busy.Route) Void m) router
  pure (router, ui)

-- | Have the component click a link while it renders.
clickDuringRender :: Mounted s q o m -> Busy.Link -> PageM s ()
clickDuringRender ui l = find ui (".busy-button-" <> show (fromEnum l)) >>= click
