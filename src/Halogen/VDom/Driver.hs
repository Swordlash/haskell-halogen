module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Protolude
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver qualified as AD
import Halogen.VDom qualified as V
import Halogen.VDom.DOM.Prop
import Halogen.Query.Input
import Halogen.Component
import Halogen.Aff.Driver.State
import Data.Foreign
import Halogen.HTML.Core (HTML(..))
import Data.Primitive
import Control.Monad.Primitive
import Halogen.VDom.Thunk (Thunk)
import qualified Web.DOM.Internal.Types as DOM
import qualified Halogen.VDom.DOM.Prop as VP
import qualified Halogen.VDom.Thunk as Thunk
import Data.Coerce
import qualified Halogen.VDom.DOM.Monad as DOM
import Web.DOM.Internal.Types
import Data.MutVarF (atomicWriteMutVar)
import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Exception.Safe
import Control.Monad.UUID

type VHTML m action slots =
  V.VDom [Prop (Input action)] (ComponentSlot slots m action)

type ChildRenderer m action slots = ComponentSlotBox slots m action -> m (RenderStateX (RenderState m))

data RenderState m state action slots output =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step m (VHTML m action slots) DOM.Node
    , renderChildRef :: MutVar (PrimState m) (ChildRenderer m action slots)
    }

type HTMLThunk m slots action =
  Thunk (HTML (ComponentSlot slots m action)) action

type WidgetState m slots action =
  Maybe (V.Step m (HTMLThunk m slots action) DOM.Node)

mkSpec
  :: forall m action slots
   . (PrimMonad m, DOM.MonadDOM m)
  => (Input action -> m ())
  -> MutVar (PrimState m) (ChildRenderer m action slots)
  -> DOM.Document
  -> V.VDomSpec m [Prop (Input action)] (ComponentSlot slots m action)
mkSpec handler renderChildRef document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.Machine m [Prop (Input action)] ()
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec m
         [Prop (Input action)]
         (ComponentSlot slots m action)
    -> V.Machine m
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
      renderChild <- readMutVar renderChildRef
      rsx <- renderChild cs
      let node = getNode rsx
      pure $ V.Step node Nothing patch done

  done :: WidgetState m slots action -> m ()
  done = traverse_ V.halt

  getNode :: RenderStateX (RenderState m) -> DOM.Node
  getNode (RenderStateX(RenderState { node })) = node

runUI
  :: forall m query input output
   . (DOM.MonadDOM m, PrimMonad m, MonadFork Async m, MonadKill Async m, MonadParallel m, MonadMask m, MonadUUID m)
  => Component query input output m
  -> input
  -> DOM.HTMLElement
  -> m (HalogenIO query output m)
runUI component i element = do
  document <- toDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component i

renderSpec
  :: forall m
   . (DOM.MonadDOM m, PrimMonad m)
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
        renderChildRef <- newMutVar child
        let spec = mkSpec handler renderChildRef document
        machine <- V.buildVDom spec vdom
        let node = V.extract machine
        void $ DOM.appendChild node (toNode container)
        pure $ RenderState { machine, node, renderChildRef }
      Just (RenderState { machine, node, renderChildRef }) -> do
        atomicWriteMutVar renderChildRef child
        parent <- DOM.parentNode node
        nextSib <- DOM.nextSibling node
        machine' <- V.step machine vdom
        let newNode = V.extract machine'
        unless (node `unsafeRefEq` newNode) $
          substInParent newNode nextSib parent
        pure $ RenderState { machine = machine', node = newNode, renderChildRef }

removeChild :: DOM.MonadDOM m => RenderState m state action slots output -> m ()
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (DOM.removeChild node) npn

substInParent :: DOM.MonadDOM m => DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> m ()
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pass
