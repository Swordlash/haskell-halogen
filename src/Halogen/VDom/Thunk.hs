module Halogen.VDom.Thunk where

import Protolude hiding (state)
import Data.Foreign
import qualified Halogen.VDom as V
import Web.DOM.Internal.Types
import Unsafe.Coerce
import Halogen.VDom.DOM.Monad

newtype ThunkId = ThunkId (Foreign ThunkId)
data Thunk f i = forall a. Thunk ThunkId (a -> a -> Bool) (a -> f i) a

unsafeEqThunk :: forall f i. Thunk f i -> Thunk f i -> Bool
unsafeEqThunk (Thunk a1 b1 _ d1) (Thunk a2 b2 _ d2) =
  unsafeRefEq a1 a2 &&
  unsafeRefEqHet b1 b2 &&
  b1 d1 (unsafeCoerce d2)

data ThunkState m f i a w = ThunkState
  { vdom :: V.Step m (V.VDom a w) Node
  , thunk :: Thunk f i
  }

runThunk :: forall f i. Thunk f i -> f i
runThunk (Thunk _ _ render arg) = render arg

buildThunk
  :: forall m f i a w
   . MonadDOM m
  => (f i -> V.VDom a w)
  -> V.VDomSpec m a w
  -> V.Machine m (Thunk f i) Node
buildThunk toVDom = renderThunk
  where
  renderThunk :: V.VDomSpec m a w -> V.Machine m (Thunk f i) Node
  renderThunk spec t = do
    vdom <- V.buildVDom spec (toVDom (runThunk t))
    pure $ V.Step (V.extract vdom) (ThunkState { thunk = t, vdom }) patchThunk haltThunk

  patchThunk :: ThunkState m f i a w -> Thunk f i -> m (V.Step m (Thunk f i) Node)
  patchThunk state t2 = do
    let ThunkState { vdom = prev, thunk = t1 } = state
    if unsafeEqThunk t1 t2
      then pure $ V.Step (V.extract prev) state patchThunk haltThunk
      else do
        vdom <- V.step prev (toVDom (runThunk t2))
        pure $ V.Step (V.extract vdom) (ThunkState { vdom, thunk = t2 }) patchThunk haltThunk

  haltThunk :: ThunkState m f i a w -> m ()
  haltThunk state = V.halt state.vdom