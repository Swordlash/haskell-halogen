{-# LANGUAGE CPP #-}

-- | A listener on a tree's outputs from before any of its code runs
-- ('Halogen.VDom.Driver.runUIListening' and
-- 'Halogen.VDom.Driver.hydrateUIListening'): it hears what the root raises
-- while it is being mounted, which a subscription to the socket's
-- @messages@ cannot, since it can only be made once the mount has
-- returned. And "Halogen.Test.Native"'s 'T.outputs', which hears the
-- component through a parent, has those outputs too.
module Test.Listening (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "a listener from the start of the mount" $ pure ()

#else

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.Activity (quiet)
import Halogen.HTML qualified as HH
import Halogen.Test.Native qualified as T
import Halogen.VDom.DOM.Monad (MemDOM, appendChild, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (hydrateUIListening, runUI, runUIListening)
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it, shouldReturn)

-- | The output, named for the moment it was raised.
data Raised = Initializing | Forked | Told
  deriving stock (Eq, Show)

-- | Raise 'Told'.
newtype Query a = Tell a

-- | A component that raises in its @Initialize@, from a fork its
-- @Initialize@ starts, and when it is told to. The fork waits in 'H.async'
-- for a worker that fills an MVar, and the @Initialize@ waits in place for
-- the MVar, so the fork's answer is on its way before the mount returns.
raising :: forall i m. (MonadIO m) => H.Component Query i Raised m
raising =
  H.component (const (pure ())) render [H.onInitialize initialize, H.onQuery handleQuery]
  where
    render :: () -> H.ComponentHTML () Empty m
    render () = HH.p_ [HH.text "raising"]

    initialize :: H.HalogenM () () Empty Raised m ()
    initialize = do
      H.raise Initializing
      done <- H.sync (liftIO newEmptyMVar)
      void $ H.fork $ do
        H.async (liftIO (putMVar done ()))
        H.raise Forked
      H.sync (liftIO (takeMVar done))

    handleQuery :: Query a -> H.HalogenM () () Empty Raised m (Maybe a)
    handleQuery (Tell a) = Just a <$ H.raise Told

-- | A listener that keeps what it hears, oldest first.
keeping :: IO (IORef [Raised], Raised -> MemDOM ())
keeping = do
  heard <- newIORef []
  pure (heard, \o -> liftIO (atomicModifyIORef' heard (\os -> (os <> [o], ()))))

-- | A container holding what the component rendered there, as a page that
-- arrived with it would: the component is disposed of and its tree put back
-- without the listeners it had.
prerendered :: IO N.NativeNode
prerendered = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI (raising :: H.Component Query () Raised MemDOM) () (N.fromNative container))
  rendered <- N.childNodes container
  runMemDOM dispose
  for_ rendered $ \node -> do
    writeIORef node.listeners []
    runMemDOM (appendChild (N.fromNative node) (N.fromNative container))
  pure container

spec :: Spec
spec = describe "a listener from the start of the mount" $ do
  it "hears what the root raised while it was mounted, and afterwards" $ do
    (heard, listener) <- keeping
    container <- N.newElement Nothing (ElemName "div")
    H.HalogenSocket {H.query = ask, H.waitActivity = waitFor, H.dispose = dispose} <-
      runMemDOM (runUIListening listener raising () (N.fromNative container))
    runMemDOM (waitFor quiet)
    readIORef heard `shouldReturn` [Initializing, Forked]
    void (runMemDOM (ask (H.mkTell Tell)))
    readIORef heard `shouldReturn` [Initializing, Forked, Told]
    runMemDOM dispose

  it "hears what the root raised while it was hydrated" $ do
    (heard, listener) <- keeping
    container <- prerendered
    H.HalogenSocket {H.waitActivity = waitFor, H.dispose = dispose} <-
      runMemDOM (hydrateUIListening listener raising () (N.fromNative container))
    runMemDOM (waitFor quiet)
    readIORef heard `shouldReturn` [Initializing, Forked]
    runMemDOM dispose

  it "is not needed by the native harness, whose outputs have what the component raised while it was mounted" $
    T.withView id raising () $ \view -> do
      T.settle view
      T.outputs view `shouldReturn` [Initializing, Forked]

#endif
