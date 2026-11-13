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

data ThunkState m f i a w r = ThunkState
  { vdom :: V.Step m (V.VDom a w) (DomNode m)
  , thunk :: Thunk f i
  , watched :: r
  }

-- | What a renderer learns of a thunk's renders: 'running' goes around
-- each render that runs the thunk (its first build, or a patch with a
-- thunk that is not equal), and says what of that render to keep (the
-- child components it met, say); 'skipped' is told it on each patch that
-- skips the thunk, and may give the node that now stands for what the
-- thunk rendered, if that is no longer the one it built.
data ThunkWatch m a w r = ThunkWatch
  { running :: forall x. V.VDom a w -> m x -> m (x, r)
  , skipped :: r -> m (Maybe (DomNode m))
  }

-- | A watch that learns nothing.
unwatched :: (Monad m) => ThunkWatch m a w ()
unwatched = ThunkWatch {running = \_ act -> (,()) <$> act, skipped = \_ -> pure Nothing}

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

{-# INLINEABLE hydrateThunk #-}

{-# INLINEABLE buildThunkWith #-}

{-# INLINEABLE hydrateThunkWith #-}
#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
{-# SPECIALISE buildThunk :: (f i -> V.VDom a w) -> V.VDomSpec BrowserDOM a w -> V.Machine BrowserDOM (Thunk f i) (DomNode BrowserDOM) #-}
{-# SPECIALISE hydrateThunk :: (f i -> V.VDom a w) -> V.VDomSpec BrowserDOM a w -> DomNode BrowserDOM -> V.Machine BrowserDOM (Thunk f i) (DomNode BrowserDOM) #-}
#else
{-# SPECIALISE buildThunk :: (f i -> V.VDom a w) -> V.VDomSpec MemDOM a w -> V.Machine MemDOM (Thunk f i) (DomNode MemDOM) #-}
{-# SPECIALISE hydrateThunk :: (f i -> V.VDom a w) -> V.VDomSpec MemDOM a w -> DomNode MemDOM -> V.Machine MemDOM (Thunk f i) (DomNode MemDOM) #-}
#endif
buildThunk
  :: forall m f i a w
   . (MonadDOM m)
  => (f i -> V.VDom a w)
  -> V.VDomSpec m a w
  -> V.Machine m (Thunk f i) (DomNode m)
buildThunk = buildThunkWith unwatched

-- | 'buildThunk', telling the watch of each render.
buildThunkWith
  :: forall m f i a w r
   . (MonadDOM m)
  => ThunkWatch m a w r
  -> (f i -> V.VDom a w)
  -> V.VDomSpec m a w
  -> V.Machine m (Thunk f i) (DomNode m)
buildThunkWith watch@ThunkWatch {running} toVDom spec t = do
  let vdom = toVDom (runThunk t)
  (step, r) <- running vdom (V.buildVDom spec vdom)
  pure (thunkStep watch toVDom t r step)

-- | 'buildThunk' by adopting the node given, as
-- 'Halogen.VDom.DOM.hydrateVDom' adopts one: the thunk is rendered, and
-- what it renders is hydrated. A thunk is a widget, so this is what a
-- 'Halogen.VDom.DOM.VDomSpec' whose widgets are thunks gives as its
-- @hydrateWidget@.
hydrateThunk
  :: forall m f i a w
   . (MonadDOM m)
  => (f i -> V.VDom a w)
  -> V.VDomSpec m a w
  -> DomNode m
  -> V.Machine m (Thunk f i) (DomNode m)
hydrateThunk = hydrateThunkWith unwatched

-- | 'hydrateThunk', telling the watch of each render: the one that
-- adopts the node is a render that runs the thunk.
hydrateThunkWith
  :: forall m f i a w r
   . (MonadDOM m)
  => ThunkWatch m a w r
  -> (f i -> V.VDom a w)
  -> V.VDomSpec m a w
  -> DomNode m
  -> V.Machine m (Thunk f i) (DomNode m)
hydrateThunkWith watch@ThunkWatch {running} toVDom spec candidate t = do
  let vdom = toVDom (runThunk t)
  (step, r) <- running vdom (V.hydrateVDom spec (Just candidate) vdom)
  pure (thunkStep watch toVDom t r step)

-- | The machine of a thunk that has rendered this.
thunkStep
  :: forall m f i a w r
   . (Monad m)
  => ThunkWatch m a w r
  -> (f i -> V.VDom a w)
  -> Thunk f i
  -> r
  -> V.Step m (V.VDom a w) (DomNode m)
  -> V.Step m (Thunk f i) (DomNode m)
thunkStep ThunkWatch {running, skipped} toVDom t0 r0 vdom0 =
  V.Step (V.extract vdom0) (ThunkState {thunk = t0, vdom = vdom0, watched = r0}) patchThunk haltThunk
  where
    patchThunk :: ThunkState m f i a w r -> Thunk f i -> m (V.Step m (Thunk f i) (DomNode m))
    patchThunk state t2 = do
      let ThunkState {vdom = prev, thunk = t1, watched} = state
      if unsafeEqThunk t1 t2
        then do
          moved <- skipped watched
          pure $ V.Step (fromMaybe (V.extract prev) moved) state patchThunk haltThunk
        else do
          let next = toVDom (runThunk t2)
          (vdom, r) <- running next (V.step prev next)
          pure $ V.Step (V.extract vdom) (ThunkState {vdom, thunk = t2, watched = r}) patchThunk haltThunk

    haltThunk :: ThunkState m f i a w r -> m ()
    haltThunk state = V.halt state.vdom
