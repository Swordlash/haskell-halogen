{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Halogen.VDom.Thunk where

import Data.Foreign
import Data.Functor.Classes (liftEq)
import GHC.Exts qualified as GHC
import HPrelude hiding (state)
import Halogen.VDom qualified as V
import Halogen.VDom.DOM.Monad
import Unsafe.Coerce

newtype ThunkId = ThunkId GHC.Any

unsafeThunkId :: a -> ThunkId
unsafeThunkId = unsafeCoerce

-- | A render put off until its input changes: an identity, an equality on
-- the input, the render and the input. The last field holds the mappings
-- applied to what it renders since ('mapThunk'), outermost first.
--
-- A thunk that compares equal keeps what the last one built, event
-- handlers and all, so a mapping is part of what is compared: two thunks
-- mapped by different functions are different thunks, whatever their
-- input. Mappings are compared by reference, so a thunk mapped anew by a
-- function built in each render (a lambda over a changing value) is built
-- again in each render. Map by a function that stays the same (a
-- constructor, a top-level function) to keep what memoizing saves.
data Thunk f i = forall a. Thunk ThunkId (a -> a -> Bool) (a -> f i) a [GHC.Any]

-- | A thunk of @render@ at @arg@, compared by @eq@ while its identity is
-- the same.
mkThunk :: ThunkId -> (a -> a -> Bool) -> (a -> f i) -> a -> Thunk f i
mkThunk tid eq render arg = Thunk tid eq render arg []

instance (Functor f) => Functor (Thunk f) where
  fmap f = mapThunkBy f (fmap f)

unsafeEqThunk :: forall f i. Thunk f i -> Thunk f i -> Bool
unsafeEqThunk (Thunk a1 b1 _ d1 m1) (Thunk a2 b2 _ d2 m2) =
  unsafeRefEq a1 a2
    && unsafeRefEq' b1 b2
    && liftEq unsafeRefEq m1 m2
    && b1 d1 (unsafeCoerce d2)

data ThunkState m f i a w = ThunkState
  { vdom :: V.Step m (V.VDom a w) (DomNode m)
  , thunk :: Thunk f i
  }

hoist :: forall f g a. (forall x. f x -> g x) -> Thunk f a -> Thunk g a
hoist f = mapThunkBy f f

-- | Map what a thunk renders. The mapping becomes part of the thunk's
-- identity (see 'Thunk').
mapThunk :: forall f g i j. (f i -> g j) -> Thunk f i -> Thunk g j
mapThunk k = mapThunkBy k k

-- | 'mapThunk' by a function built from @key@, which stands for it in the
-- thunk's identity: the function may be built anew each time, as long as
-- the same key means the same mapping.
mapThunkBy :: forall key f g i j. key -> (f i -> g j) -> Thunk f i -> Thunk g j
mapThunkBy key k (Thunk a b c d ms) = key `seq` Thunk a b (k . c) d (unsafeCoerce key : ms)

runThunk :: forall f i. Thunk f i -> f i
runThunk (Thunk _ _ render arg _) = render arg

{-# INLINEABLE buildThunk #-}
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
{-# SPECIALISE buildThunk :: (f i -> V.VDom a w) -> V.VDomSpec BrowserDOM a w -> V.Machine BrowserDOM (Thunk f i) (DomNode BrowserDOM) #-}
#else
{-# SPECIALISE buildThunk :: (f i -> V.VDom a w) -> V.VDomSpec MemDOM a w -> V.Machine MemDOM (Thunk f i) (DomNode MemDOM) #-}
#endif
buildThunk
  :: forall m f i a w
   . (MonadDOM m)
  => (f i -> V.VDom a w)
  -> V.VDomSpec m a w
  -> V.Machine m (Thunk f i) (DomNode m)
buildThunk toVDom = renderThunk
  where
    renderThunk :: V.VDomSpec m a w -> V.Machine m (Thunk f i) (DomNode m)
    renderThunk spec t = do
      vdom <- V.buildVDom spec (toVDom (runThunk t))
      pure $ V.Step (V.extract vdom) (ThunkState {thunk = t, vdom}) patchThunk haltThunk

    patchThunk :: ThunkState m f i a w -> Thunk f i -> m (V.Step m (Thunk f i) (DomNode m))
    patchThunk state t2 = do
      let ThunkState {vdom = prev, thunk = t1} = state
      if unsafeEqThunk t1 t2
        then pure $ V.Step (V.extract prev) state patchThunk haltThunk
        else do
          vdom <- V.step prev (toVDom (runThunk t2))
          pure $ V.Step (V.extract vdom) (ThunkState {vdom, thunk = t2}) patchThunk haltThunk

    haltThunk :: ThunkState m f i a w -> m ()
    haltThunk state = V.halt state.vdom
