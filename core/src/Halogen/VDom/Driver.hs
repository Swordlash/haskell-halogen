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
  { machine :: V.Step m (VHTML m action slots) DOM.Node
  , renderChildRef :: IORef (ChildRenderer m action slots)
  , shown :: IORef DOM.Node
  -- ^ What stands for the component on the page, and to its parent (which
  -- moves and removes it): the root of its HTML, or, while it is broken, a
  -- placeholder.
  , broken :: IORef Bool
  -- ^ A patch failed part way. What it left was taken off the page, an
  -- empty text node was put in its place, and the next render builds the
  -- HTML afresh there rather than patch a machine that no longer says what
  -- the page holds.
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
          node <- getNode =<< renderChild cs
          pure $ V.Step node Nothing patch done

    done :: WidgetState m slots action -> m ()
    done = traverse_ V.halt

    getNode :: RenderStateX (RenderState m) -> m DOM.Node
    getNode (RenderStateX (RenderState {shown})) = readIORef shown

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
          shown <- newIORef node
          broken <- newIORef False
          pure $ RenderState {machine, renderChildRef, shown, broken}
        Just st@(RenderState {machine, renderChildRef, shown, broken}) -> do
          node <- readIORef shown
          parent <- DOM.parentNode node
          nextSib <- DOM.nextSibling node
          readIORef broken >>= \case
            False -> do
              atomicWriteIORef renderChildRef child
              machine' <- V.step machine vdom `onException` tearDown st node parent nextSib
              let newNode = V.extract machine'
              unless (node `unsafeRefEq` newNode) $ do
                substInParent newNode nextSib parent
                writeIORef shown newNode
              pure st {machine = machine'}
            True -> do
              -- Built where the placeholder is, which the parent may have
              -- moved meanwhile; until a build succeeds it stays.
              (machine', renderChildRef') <- build
              let newNode = V.extract machine'
              for_ parent $ \pn -> do
                DOM.insertBefore newNode node pn
                DOM.removeChild node pn
              writeIORef shown newNode
              writeIORef broken False
              pure $ RenderState {machine = machine', renderChildRef = renderChildRef', shown, broken}
      where
        -- A patch that failed has changed part of the page already. What it
        -- left goes (the old machine is halted, which lets its refs go, and
        -- its root is taken off the page if the patch had not), and an
        -- empty text node holds the place where the root was before it.
        tearDown RenderState {machine, shown, broken} node parent nextSib = do
          placeholder <- DOM.createTextNode "" document
          substInParent placeholder nextSib parent
          void $ tryAny (V.halt machine)
          DOM.parentNode node >>= traverse_ (DOM.removeChild node)
          writeIORef shown placeholder
          writeIORef broken True
        build = do
          renderChildRef <- newIORef child
          let spec = mkSpec handler renderChildRef document
          machine <- V.buildVDom spec vdom
          pure (machine, renderChildRef)

removeChild
  :: forall m state action slots output
   . (DOM.MonadBrowserDOM m, MonadIO m)
  => RenderState m state action slots output
  -> m ()
removeChild (RenderState {shown}) = do
  node <- readIORef shown
  npn <- DOM.parentNode node
  traverse_ (DOM.removeChild node) npn

substInParent :: (DOM.MonadBrowserDOM m) => DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> m ()
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pass
