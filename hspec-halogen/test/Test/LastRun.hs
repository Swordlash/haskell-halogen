-- | A program run again on the same page, as browser GHCi's @:main@ runs it:
-- 'Halogen.IO.Util.endPreviousRun' disposes of the tree the last run kept
-- with 'Halogen.IO.Util.keepForNextRun' before the next one mounts.
module Test.LastRun (spec) where

import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Protolude hiding (find)
import Test.Hspec.Halogen
import Web.DOM.ParentNode (QuerySelector (..))

-- | A paragraph that sets the reference it is given once it is finalized,
-- at once ('H.liftEffect'), so that the finalizer has run by the time the
-- disposal returns: what a finalizer does in 'liftIO' goes on after it.
finalizing :: forall m. (MonadIO m) => H.Component H.VoidF (IORef Bool) Void m
finalizing =
  H.mkComponent
    H.ComponentSpec
      { initialState = pure
      , render = \_ -> HH.p_ [HH.text "the last run"]
      , eval =
          H.mkEval
            H.defaultEval
              { H.finalize = Just ()
              , H.handleAction = \() -> get >>= \finalized -> H.liftEffect (liftIO (atomicWriteIORef finalized True))
              }
      }

-- | What a program's @main@ does: end the last run, mount into the page's
-- @.last-run@ and keep the socket for the next run.
run :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => IORef Bool -> IO ()
run m finalized = runTest $ do
  HA.endPreviousRun
  container <- HA.selectElement (QuerySelector ".last-run") >>= maybe (panic "the page has no .last-run") pure
  socket <- runUI finalizing finalized container :: m (H.HalogenSocket H.VoidF Void m)
  HA.keepForNextRun socket

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  it "disposes of the tree the last run kept" $ runPage $ do
    page <- htmlContainer "<div class=\"last-run\"></div>"
    finalized <- unsafeIOToPageM (newIORef False)
    unsafeIOToPageM (run m finalized)
    paragraph <- findIn page "p"
    unsafeIOToPageM HA.endPreviousRun
    isAttached paragraph `shouldReturn` False
    unsafeIOToPageM (readIORef finalized) `shouldReturn` True
    outerHTML page `shouldReturn` "<div><div class=\"last-run\"></div></div>"

  it "does nothing when nothing was kept" $ runPage $ do
    unsafeIOToPageM HA.endPreviousRun
    finalized <- unsafeIOToPageM (newIORef False)
    ui <- mount m finalizing finalized
    paragraph <- find ui "p"
    unsafeIOToPageM HA.endPreviousRun
    isAttached paragraph `shouldReturn` True
    unsafeIOToPageM (readIORef finalized) `shouldReturn` False
