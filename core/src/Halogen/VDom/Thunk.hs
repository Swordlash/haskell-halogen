{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Halogen.VDom.Thunk where

import Data.Foreign
import GHC.Exts qualified as GHC
import HPrelude hiding (state)
import Halogen.VDom qualified as V
import Halogen.VDom.DOM.Monad
import Unsafe.Coerce

newtype ThunkId = ThunkId GHC.Any

unsafeThunkId :: a -> ThunkId
unsafeThunkId = unsafeCoerce

data Thunk f i = forall a. Thunk ThunkId (a -> a -> Bool) (a -> f i) a

deriving instance (Functor f) => Functor (Thunk f)

unsafeEqThunk :: forall f i. Thunk f i -> Thunk f i -> Bool
unsafeEqThunk (Thunk a1 b1 _ d1) (Thunk a2 b2 _ d2) =
  unsafeRefEq a1 a2
    && unsafeRefEq' b1 b2
    && b1 d1 (unsafeCoerce d2)

data ThunkState dom m f i a w = ThunkState
  { vdom :: V.Step m (V.VDom a w) (DomNode dom)
  , thunk :: Thunk f i
  }

hoist :: forall f g a. (forall x. f x -> g x) -> Thunk f a -> Thunk g a
hoist f = mapThunk f

mapThunk :: forall f g i j. (f i -> g j) -> Thunk f i -> Thunk g j
mapThunk k (Thunk a b c d) = Thunk a b (k . c) d

runThunk :: forall f i. Thunk f i -> f i
runThunk (Thunk _ _ render arg) = render arg

{-# INLINEABLE buildThunk #-}
buildThunk
  :: forall dom m f i a w
   . (MonadDOM dom, Monad m)
  => (f i -> V.VDom a w)
  -> V.VDomSpec dom m a w
  -> V.Machine m (Thunk f i) (DomNode dom)
buildThunk toVDom = renderThunk
  where
    renderThunk :: V.VDomSpec dom m a w -> V.Machine m (Thunk f i) (DomNode dom)
    renderThunk spec t = do
      vdom <- V.buildVDom spec (toVDom (runThunk t))
      pure $ V.Step (V.extract vdom) (ThunkState {thunk = t, vdom}) patchThunk haltThunk

    patchThunk :: ThunkState dom m f i a w -> Thunk f i -> m (V.Step m (Thunk f i) (DomNode dom))
    patchThunk state t2 = do
      let ThunkState {vdom = prev, thunk = t1} = state
      if unsafeEqThunk t1 t2
        then pure $ V.Step (V.extract prev) state patchThunk haltThunk
        else do
          vdom <- V.step prev (toVDom (runThunk t2))
          pure $ V.Step (V.extract vdom) (ThunkState {vdom, thunk = t2}) patchThunk haltThunk

    haltThunk :: ThunkState dom m f i a w -> m ()
    haltThunk state = V.halt state.vdom
