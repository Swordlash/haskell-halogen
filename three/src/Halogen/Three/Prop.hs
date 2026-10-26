-- | Applies a 'Prop' list to a Three.js object, and keeps applying it
-- across patches, as "Halogen.Canvas.Pixi.Prop" does for a Pixi display
-- object.
--
-- A handler is registered once and its target swapped through a cell on
-- every patch, so that a handler changing identity between renders does not
-- churn registrations. Every other prop is applied again only when its value
-- changed: a material whose colour changed is patched (its kind kept), a
-- geometry that changed is replaced and the old one disposed of.
module Halogen.Three.Prop
  ( buildProp
  , readEvent
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Map.Strict qualified as M
import Data.Primitive.MutVar
import Halogen.JSBits (JSVal)
import Halogen.Three.Core
import Halogen.Three.FFI qualified as FFI
import Halogen.Three.Monad
import Halogen.Three.Types
import Halogen.VDom.Machine qualified as V
import Halogen.VDom.Utils qualified as Util
import Protolude hiding (Handler, state)

type Listeners i = Map Text (FFI.Callback, MutVar (PrimState ThreeDOM) (ThreeEvent -> Maybe i))

data PropState i = PropState
  { listeners :: MutVar (PrimState ThreeDOM) (Listeners i)
  , props :: Map Text (Prop i)
  }

buildProp
  :: forall i
   . (i -> ThreeDOM ())
  -> IO Bool
  -- ^ Whether a click now ends a drag of the camera rather than being one.
  -> FFI.Holder
  -> FFI.Object
  -> V.Machine ThreeDOM [Prop i] ()
buildProp emit swallowClick holder object = render
  where
    render :: V.Machine ThreeDOM [Prop i] ()
    render next = do
      listeners <- newMutVar mempty
      props <- Util.strMapWithIxE next propKey (applyProp listeners)
      pure $ V.Step () PropState {listeners, props} patch halt

    patch :: PropState i -> [Prop i] -> ThreeDOM (V.Step ThreeDOM [Prop i] ())
    patch state next = do
      listeners <- newMutVar mempty
      props <-
        Util.diffWithKeyAndIxE
          state.props
          next
          propKey
          (diffProp state.listeners listeners)
          (removeProp state.listeners)
          (applyProp listeners)
      pure $ V.Step () PropState {listeners, props} patch halt

    -- Only the handlers: the object's geometry and material are disposed
    -- of when the reconciler takes the object out (see
    -- "Halogen.Three.Monad"), which it does before halting its props.
    halt :: PropState i -> ThreeDOM ()
    halt state = do
      registered <- readMutVar state.listeners
      for_ (M.toList registered) $ \(name, (callback, _)) -> liftIO $ do
        FFI.removeHandler object name callback
        FFI.freeCallback callback

    applyProp :: MutVar (PrimState ThreeDOM) (Listeners i) -> Text -> Int -> Prop i -> ThreeDOM (Prop i)
    applyProp listeners _ _ prop = do
      case prop of
        Handler eventType f -> do
          let name = eventName eventType
          target <- newMutVar f
          callback <- liftIO $ FFI.mkCallback $ \raw -> do
            swallowed <- if eventType == Click then swallowClick else pure False
            unless swallowed $ do
              event <- readEvent raw
              current <- readMutVar target
              traverse_ (runThreeDOM . emit) (current event)
          liftIO $ FFI.addHandler object name callback
          atomicModifyMutVar'_ listeners (M.insert name (callback, target))
        _ -> liftIO $ paint prop
      pure prop

    -- A handler still present keeps its registration and its callback;
    -- only what it points at changes.
    diffProp
      :: MutVar (PrimState ThreeDOM) (Listeners i)
      -> MutVar (PrimState ThreeDOM) (Listeners i)
      -> Text
      -> Int
      -> Prop i
      -> Prop i
      -> ThreeDOM (Prop i)
    diffProp previous listeners _ _ old new = do
      case (old, new) of
        (Handler eventType _, Handler _ f) -> do
          registered <- readMutVar previous
          for_ (M.lookup (eventName eventType) registered) $ \entry@(_, target) -> do
            writeMutVar target f
            atomicModifyMutVar'_ listeners (M.insert (eventName eventType) entry)
        _ | same old new -> pass
        _ -> liftIO $ paint new
      pure new

    -- Every prop undoes itself: a patch that drops one keeps the object,
    -- and whatever the prop did to it is still there.
    removeProp :: MutVar (PrimState ThreeDOM) (Listeners i) -> Text -> Prop i -> ThreeDOM ()
    removeProp previous _ prop = liftIO $ case prop of
      Handler eventType _ -> do
        registered <- readMutVar previous
        for_ (M.lookup (eventName eventType) registered) $ \(callback, _) -> do
          FFI.removeHandler object (eventName eventType) callback
          FFI.freeCallback callback
      Shape _ -> FFI.clearGeometry holder object
      Paint _ -> FFI.clearMaterial holder object
      Position _ -> FFI.setPosition object 0 0 0
      Rotation _ -> FFI.setRotation object 0 0 0
      Scale _ -> FFI.setScale object 1 1 1
      Visible _ -> FFI.setVisible object True
      Name _ -> FFI.setName object ""
      Light _ _ -> FFI.setLight object 0xffffff 1

    -- Handlers are never equal by this test and never reach it.
    same :: Prop i -> Prop i -> Bool
    same a b = case (a, b) of
      (Shape x, Shape y) -> x == y
      (Paint x, Paint y) -> x == y
      (Position x, Position y) -> x == y
      (Rotation x, Rotation y) -> x == y
      (Scale x, Scale y) -> x == y
      (Visible x, Visible y) -> x == y
      (Name x, Name y) -> x == y
      (Light c x, Light d y) -> c == d && x == y
      _ -> False

    paint :: Prop i -> IO ()
    paint = \case
      Shape geometry -> setGeometry holder object geometry
      Paint material -> setMaterial holder object material
      Position (Vec3 x y z) -> FFI.setPosition object x y z
      Rotation (Euler x y z) -> FFI.setRotation object x y z
      Scale (Vec3 x y z) -> FFI.setScale object x y z
      Visible shown -> FFI.setVisible object shown
      Name name -> FFI.setName object name
      Light color intensity -> FFI.setLight object color intensity
      Handler _ _ -> pass

setGeometry :: FFI.Holder -> FFI.Object -> Geometry -> IO ()
setGeometry holder object = \case
  Box width height depth -> FFI.setGeometry holder object 0 width height depth 0
  Sphere radius around down -> FFI.setGeometry holder object 1 radius (fromIntegral around) (fromIntegral down) 0
  Plane width height -> FFI.setGeometry holder object 2 width height 0 0
  Cylinder top bottom height around -> FFI.setGeometry holder object 3 top bottom height (fromIntegral around)
  Torus radius tube radial tubular -> FFI.setGeometry holder object 4 radius tube (fromIntegral radial) (fromIntegral tubular)

setMaterial :: FFI.Holder -> FFI.Object -> Material -> IO ()
setMaterial holder object = \case
  Basic {color, opacity, wireframe} -> FFI.setMaterial holder object 0 color opacity wireframe 0 0
  Standard {color, opacity, wireframe, metalness, roughness} -> FFI.setMaterial holder object 1 color opacity wireframe metalness roughness
  Phong {color, opacity, wireframe, shininess} -> FFI.setMaterial holder object 2 color opacity wireframe shininess 0

-- | An object's event as the page hands it over, read at once.
readEvent :: JSVal -> IO ThreeEvent
readEvent raw = do
  x <- FFI.eventX raw
  y <- FFI.eventY raw
  z <- FFI.eventZ raw
  object <- FFI.eventName raw
  pressed <- FFI.eventButton raw
  bits <- FFI.eventModifiers raw
  let held = Modifiers {shiftKey = testBit bits 0, ctrlKey = testBit bits 1, altKey = testBit bits 2, metaKey = testBit bits 3}
  pure ThreeEvent {point = Vec3 x y z, object, pressed, held}

atomicModifyMutVar'_ :: (PrimMonad m) => MutVar (PrimState m) a -> (a -> a) -> m ()
atomicModifyMutVar'_ ref f = atomicModifyMutVar' ref ((,()) . f)
