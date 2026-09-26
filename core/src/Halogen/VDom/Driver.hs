module Halogen.VDom.Driver
  ( runUI
  , MonadUI
  , module Halogen.IO.Driver
  )
where

import Control.Exception.Safe
import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Monad.UUID
import Data.Coerce
import Data.Foreign
import HPrelude hiding (onException)
import Halogen.Component
import Halogen.HTML.Core (HTML (..))
import Halogen.IO.Driver (HalogenSocket)
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State
import Halogen.Query.Input
import Halogen.VDom qualified as V
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.DOM.Prop
import Halogen.VDom.DOM.Prop qualified as VP
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk qualified as Thunk
import Web.DOM.Internal.Types
import Web.DOM.Internal.Types qualified as DOM

-- Specialisations live with each backend now that the class is no longer
-- pinned to IO; the unfoldings have to be exported for them to fire.
{-# INLINEABLE runUI #-}

{-# INLINEABLE renderSpec #-}

{-# INLINEABLE mkSpec #-}

type MonadUI m = (DOM.MonadBrowserDOM m, MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)

type VHTML m action slots =
  V.VDom [Prop (Input action)] (ComponentSlot slots m action)

type ChildRenderer m action slots = ComponentSlotBox slots m action -> m (RenderStateX (RenderState m))

data RenderState m state action slots output
  = RenderState
  { node :: DOM.Node
  , machine :: V.Step m (VHTML m action slots) DOM.Node
  , renderChildRef :: IORef (ChildRenderer m action slots)
  , intact :: IORef Bool
  -- ^ Cleared when a patch fails: the machine no longer says what is on the
  -- page (the patch changed some of it), so it is not patched again.
  }

type HTMLThunk m slots action =
  Thunk (HTML (ComponentSlot slots m action)) action

type WidgetState m slots action =
  Maybe (V.Step m (HTMLThunk m slots action) DOM.Node)

mkSpec
  :: forall m action slots
   . (DOM.MonadBrowserDOM m, MonadIO m)
  => (Input action -> m ())
  -> IORef (ChildRenderer m action slots)
  -> DOM.Document
  -> V.VDomSpec m [Prop (Input action)] (ComponentSlot slots m action)
mkSpec handler renderChildRef document =
  V.VDomSpec {buildWidget, buildAttributes, document}
  where
    buildAttributes
      :: DOM.Element
      -> V.Machine m [Prop (Input action)] ()
    buildAttributes = VP.buildProp handler

    buildWidget
      :: V.VDomSpec
           m
           [Prop (Input action)]
           (ComponentSlot slots m action)
      -> V.Machine
           m
           (ComponentSlot slots m action)
           DOM.Node
    buildWidget spec = render
      where
        render :: V.Machine m (ComponentSlot slots m action) DOM.Node
        render = \case
          ComponentSlot cs ->
            renderComponentSlot cs
          ThunkSlot t -> do
            step <- buildThunk t
            pure $ V.Step (V.extract step) (Just step) patch done

        patch
          :: WidgetState m slots action
          -> ComponentSlot slots m action
          -> m (V.Step m (ComponentSlot slots m action) DOM.Node)
        patch st slot =
          case st of
            Just step -> case slot of
              ComponentSlot cs -> do
                V.halt step
                renderComponentSlot cs
              ThunkSlot t -> do
                step' <- V.step step t
                pure $ V.Step (V.extract step') (Just step') patch done
            _ -> render slot

        buildThunk :: V.Machine m (HTMLThunk m slots action) DOM.Node
        buildThunk = Thunk.buildThunk coerce spec

        renderComponentSlot
          :: ComponentSlotBox slots m action
          -> m (V.Step m (ComponentSlot slots m action) DOM.Node)
        renderComponentSlot cs = do
          renderChild <- readIORef renderChildRef
          rsx <- renderChild cs
          let node = getNode rsx
          pure $ V.Step node Nothing patch done

    done :: WidgetState m slots action -> m ()
    done = traverse_ V.halt

    getNode :: RenderStateX (RenderState m) -> DOM.Node
    getNode (RenderStateX (RenderState {node})) = node

-- | Run a component against the DOM its own monad speaks.
--
-- The component monad /is/ the DOM monad. An application with effects of its
-- own stacks them on a backend — @newtype AppM a = AppM (ReaderT Config
-- BrowserDOM a)@ deriving the classes through — rather than handing the
-- driver a pair of natural transformations to get between two of them.
runUI
  :: forall m query input output
   . (DOM.MonadBrowserDOM m, MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)
  => Component query input output m
  -> input
  -> DOM.HTMLElement
  -> m (HalogenSocket query output m)
runUI component i element = do
  document <- toDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component i

renderSpec
  :: forall m
   . (DOM.MonadBrowserDOM m, MonadIO m, MonadMask m)
  => DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec m (RenderState m)
renderSpec document container =
  AD.RenderSpec
    { render
    , renderChild = identity
    , removeChild
    , dispose = removeChild
    }
  where
    render
      :: forall state action slots output
       . (Input action -> m ())
      -> (ComponentSlotBox slots m action -> m (RenderStateX (RenderState m)))
      -> HTML (ComponentSlot slots m action) action
      -> Maybe (RenderState m state action slots output)
      -> m (RenderState m state action slots output)
    render handler child (HTML vdom) =
      \case
        Nothing -> do
          (machine, renderChildRef) <- build
          let node = V.extract machine
          void $ DOM.appendChild node $ toNode container
          intact <- newIORef True
          pure $ RenderState {machine, node, renderChildRef, intact}
        Just (RenderState {machine, node, renderChildRef, intact}) -> do
          parent <- DOM.parentNode node
          nextSib <- DOM.nextSibling node
          whole <- readIORef intact
          if whole
            then do
              atomicWriteIORef renderChildRef child
              -- A patch that fails has changed part of the page already: the
              -- next render starts afresh rather than diff against a machine
              -- that no longer matches the page.
              machine' <- V.step machine vdom `onException` writeIORef intact False
              let newNode = V.extract machine'
              unless (node `unsafeRefEq` newNode)
                $ substInParent newNode nextSib parent
              pure $ RenderState {machine = machine', node = newNode, renderChildRef, intact}
            else do
              -- Whatever the failed patch left is taken off the page (the
              -- refs it held let go first, so that the new ones stay), and
              -- the HTML is built anew where it was.
              V.halt machine
              (machine', renderChildRef') <- build
              let newNode = V.extract machine'
              substInParent newNode nextSib parent
              intact' <- newIORef True
              pure $ RenderState {machine = machine', node = newNode, renderChildRef = renderChildRef', intact = intact'}
      where
        build = do
          renderChildRef <- newIORef child
          let spec = mkSpec handler renderChildRef document
          machine <- V.buildVDom spec vdom
          pure (machine, renderChildRef)

removeChild
  :: forall m state action slots output
   . (DOM.MonadBrowserDOM m)
  => RenderState m state action slots output
  -> m ()
removeChild (RenderState {node}) = do
  npn <- DOM.parentNode node
  traverse_ (DOM.removeChild node) npn

substInParent :: (DOM.MonadBrowserDOM m) => DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> m ()
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pass
