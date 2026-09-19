module Halogen.VDom.Driver
  ( runUI
  , runUIWith
  , module Halogen.IO.Driver
  )
where

import Control.Exception.Safe
import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Monad.UUID
import Data.Coerce
import Data.Foreign
import HPrelude
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

type VHTML m action slots =
  V.VDom [Prop (Input action)] (ComponentSlot slots m action)

type ChildRenderer m action slots = ComponentSlotBox slots m action -> m (RenderStateX (RenderState m))

data RenderState m state action slots output
  = RenderState
  { node :: DOM.Node
  , machine :: V.Step m (VHTML m action slots) DOM.Node
  , renderChildRef :: IORef (ChildRenderer m action slots)
  }

type HTMLThunk m slots action =
  Thunk (HTML (ComponentSlot slots m action)) action

type WidgetState m slots action =
  Maybe (V.Step m (HTMLThunk m slots action) DOM.Node)

mkSpec
  :: forall dom m action slots
   . (MonadIO m, DOM.MonadAttributes dom, DOM.DomElement dom ~ DOM.Element, DOM.DomNode dom ~ DOM.Node, DOM.DomDocument dom ~ DOM.Document)
  => (forall x. dom x -> m x)
  -> (forall x. m x -> dom x)
  -> (Input action -> m ())
  -> IORef (ChildRenderer m action slots)
  -> DOM.Document
  -> V.VDomSpec dom m [Prop (Input action)] (ComponentSlot slots m action)
mkSpec runDom toDom handler renderChildRef document =
  V.VDomSpec {runDom, buildWidget, buildAttributes, document}
  where
    buildAttributes
      :: DOM.Element
      -> V.Machine m [Prop (Input action)] ()
    buildAttributes = VP.buildProp runDom toDom handler

    buildWidget
      :: V.VDomSpec
           dom
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

-- | Run a component against a DOM spoken in a monad of its own.
--
-- @runDom@ sequences a DOM effect from the component monad; @toDom@ runs
-- component code from inside a DOM event callback. See
-- "Halogen.VDom.DOM.Prop" for why only the listener path needs the second.
runUIWith
  :: forall dom m query input output
   . (DOM.MonadBrowserDOM dom, DOM.DomElement dom ~ DOM.Element, DOM.DomNode dom ~ DOM.Node, DOM.DomDocument dom ~ DOM.Document, MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)
  => (forall x. dom x -> m x)
  -> (forall x. m x -> dom x)
  -> Component query input output m
  -> input
  -> DOM.HTMLElement
  -> m (HalogenSocket query output m)
runUIWith runDom toDom component i element = do
  document <- toDocument <$> runDom (DOM.document =<< DOM.window)
  AD.runUI (renderSpec runDom toDom document element) component i

-- | Run a component against this build's default DOM backend.
--
-- Which backend that is follows "Halogen.VDom.DOM.Monad": the browser on the
-- JavaScript and wasm backends, the in-memory document on native. Use
-- 'runUIWith' to name a different one.
runUI
  :: forall m query input output
   . (MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)
  => Component query input output m
  -> input
  -> DOM.HTMLElement
  -> m (HalogenSocket query output m)
runUI component i element =
  withRunInIO $ \runInIO ->
    runInIO
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
      $ runUIWith (liftIO . DOM.runBrowserDOM) (DOM.BrowserDOM . runInIO) component i element
#else
      $ runUIWith (liftIO . DOM.runMemDOM) (DOM.MemDOM . runInIO) component i element
#endif

renderSpec
  :: forall dom m
   . (DOM.MonadAttributes dom, MonadIO m, DOM.DomElement dom ~ DOM.Element, DOM.DomNode dom ~ DOM.Node, DOM.DomDocument dom ~ DOM.Document)
  => (forall x. dom x -> m x)
  -> (forall x. m x -> dom x)
  -> DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec m (RenderState m)
renderSpec runDom toDom document container =
  AD.RenderSpec
    { render
    , renderChild = identity
    , removeChild = removeChild runDom
    , dispose = removeChild runDom
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
          renderChildRef <- newIORef child
          let spec = mkSpec runDom toDom handler renderChildRef document
          machine <- V.buildVDom spec vdom
          let node = V.extract machine
          void $ runDom $ DOM.appendChild node $ toNode container
          pure $ RenderState {machine, node, renderChildRef}
        Just (RenderState {machine, node, renderChildRef}) -> do
          atomicWriteIORef renderChildRef child
          parent <- runDom $ DOM.parentNode node
          nextSib <- runDom $ DOM.nextSibling node
          machine' <- V.step machine vdom
          let newNode = V.extract machine'
          unless (node `unsafeRefEq` newNode)
            $ runDom
            $ substInParent newNode nextSib parent
          pure $ RenderState {machine = machine', node = newNode, renderChildRef}

removeChild
  :: forall dom m state action slots output
   . (DOM.MonadDOM dom, DOM.DomNode dom ~ DOM.Node)
  => (forall x. dom x -> m x)
  -> RenderState m state action slots output
  -> m ()
removeChild runDom (RenderState {node}) = runDom $ do
  npn <- DOM.parentNode node
  traverse_ (DOM.removeChild node) npn

substInParent :: (DOM.MonadDOM dom, DOM.DomNode dom ~ DOM.Node) => DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> dom ()
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pass
