-- | The canvas counterpart to "Halogen.VDom.DOM.Prop": applies a
-- 'CanvasProp' list to a Pixi display object, and keeps applying it across
-- patches.
--
-- The listener discipline is the one 'Halogen.VDom.DOM.Prop.buildProp' uses,
-- and for the same reason: a listener is registered with Pixi once and its
-- target is swapped through a cell on every patch, so that a handler
-- changing identity between renders does not churn registrations. What is
-- different here is the teardown - when the last handler goes, the object's
-- @eventMode@ has to go back with it, or an element that has stopped being
-- interactive keeps swallowing hit tests from whatever is beneath it.
module Halogen.Canvas.Pixi.Prop
  ( buildCanvasProp
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Map.Strict qualified as M
import Data.Primitive.MutVar
import Halogen.Canvas.Core
import Halogen.Canvas.Pixi.FFI qualified as FFI
import Halogen.Canvas.Pixi.Monad
import Halogen.Canvas.Types
import Halogen.VDom.Machine qualified as V
import Halogen.VDom.Utils qualified as Util
import Protolude hiding (Handler, state)

type Listeners i = Map Text (FFI.Callback, MutVar (PrimState PixiDOM) (FFI.Event -> Maybe i))

data PropState i = PropState
  { listeners :: MutVar (PrimState PixiDOM) (Listeners i)
  , props :: Map Text (CanvasProp FFI.Event i)
  }

buildCanvasProp
  :: forall m i
   . (Monad m)
  => (forall x. PixiDOM x -> m x)
  -> (forall x. m x -> PixiDOM x)
  -> (i -> m ())
  -> FFI.Application
  -> FFI.Object
  -> V.Machine m [CanvasProp FFI.Event i] ()
buildCanvasProp runDom toDom emit application object = render
  where
    render :: V.Machine m [CanvasProp FFI.Event i] ()
    render next = do
      listeners <- runDom $ newMutVar mempty
      props <- Util.strMapWithIxE next propKey (applyProp listeners)
      runDom $ syncEventMode props
      pure $ V.Step () PropState {listeners, props} patch halt

    patch :: PropState i -> [CanvasProp FFI.Event i] -> m (V.Step m [CanvasProp FFI.Event i] ())
    patch state next = do
      listeners <- runDom $ newMutVar mempty
      props <-
        Util.diffWithKeyAndIxE
          state.props
          next
          propKey
          (diffProp state.listeners listeners)
          (removeProp state.listeners)
          (applyProp listeners)
      runDom $ syncEventMode props
      pure $ V.Step () PropState {listeners, props} patch halt

    halt :: PropState i -> m ()
    halt state = runDom $ do
      registered <- readMutVar state.listeners
      for_ (M.toList registered) $ \(eventName, (callback, _)) -> liftIO $ do
        FFI.removeListener object eventName callback
        FFI.freeCallback callback

    -- An element is hit-testable exactly when it is listening, unless it
    -- says otherwise. Recomputed from the whole prop map rather than nudged
    -- on each add and remove, so that the last handler going away is not a
    -- case anyone has to remember to handle.
    syncEventMode :: Map Text (CanvasProp FFI.Event i) -> PixiDOM ()
    syncEventMode props =
      case [mode | Interactive mode <- M.elems props] of
        mode : _ -> liftIO $ FFI.setEventMode object (eventModeName mode)
        [] ->
          liftIO
            $ FFI.setEventMode object
            $ if any isHandler (M.elems props) then "static" else "passive"

    isHandler = \case
      Handler _ _ -> True
      _ -> False

    applyProp
      :: MutVar (PrimState PixiDOM) (Listeners i)
      -> Text
      -> Int
      -> CanvasProp FFI.Event i
      -> m (CanvasProp FFI.Event i)
    applyProp listeners _ _ prop = do
      case prop of
        Handler eventType f -> runDom $ do
          let eventName = pointerEventName eventType
          target <- newMutVar f
          callback <- liftIO $ FFI.mkCallback $ \event -> runPixiDOM $ do
            current <- readMutVar target
            toDom $ traverse_ emit (current event)
          liftIO $ FFI.addListener object eventName callback
          atomicModifyMutVar'_ listeners (M.insert eventName (callback, target))
        _ -> runDom $ repaint prop
      pure prop

    -- A handler that is still present keeps its registration and its
    -- callback; only what it points at changes.
    diffProp
      :: MutVar (PrimState PixiDOM) (Listeners i)
      -> MutVar (PrimState PixiDOM) (Listeners i)
      -> Text
      -> Int
      -> CanvasProp FFI.Event i
      -> CanvasProp FFI.Event i
      -> m (CanvasProp FFI.Event i)
    diffProp previous listeners _ _ old new = do
      case (old, new) of
        (Handler eventType _, Handler _ f) -> runDom $ do
          let eventName = pointerEventName eventType
          registered <- readMutVar previous
          case M.lookup eventName registered of
            Just entry@(_, target) -> do
              writeMutVar target f
              atomicModifyMutVar'_ listeners (M.insert eventName entry)
            Nothing -> pass
        _ | samePaint old new -> pass
        _ -> runDom $ repaint new
      pure new

    removeProp
      :: MutVar (PrimState PixiDOM) (Listeners i)
      -> Text
      -> CanvasProp FFI.Event i
      -> m ()
    removeProp previous _ prop = runDom $ case prop of
      Handler eventType _ -> do
        let eventName = pointerEventName eventType
        registered <- readMutVar previous
        for_ (M.lookup eventName registered) $ \(callback, _) -> liftIO $ do
          FFI.removeListener object eventName callback
          FFI.freeCallback callback
      Cursor _ -> liftIO $ FFI.setCursor object "default"
      Hit _ -> liftIO $ FFI.clearHitArea object
      Outline _ _ -> liftIO $ FFI.clearOutline object
      _ -> pass

    -- Repaint only when the description changed. Handlers are never equal by
    -- this test and never reach it.
    samePaint :: CanvasProp FFI.Event i -> CanvasProp FFI.Event i -> Bool
    samePaint a b = case (a, b) of
      (Draw x, Draw y) -> x == y
      (Place x, Place y) -> x == y
      (Label x sx, Label y sy) -> x == y && sx == sy
      (Src x sx, Src y sy) -> x == y && sx == sy
      (Interactive x, Interactive y) -> x == y
      (Cursor x, Cursor y) -> x == y
      (Hit x, Hit y) -> x == y
      (Outline x px, Outline y py) -> x == y && px == py
      _ -> False

    -- An outline is measured, so anything that changes what there is to
    -- measure invalidates it. Rather than work out which props those are,
    -- every repaint re-measures; it costs nothing on an object with no
    -- outline, and it means a new kind of prop cannot forget to.
    repaint :: CanvasProp FFI.Event i -> PixiDOM ()
    repaint prop = paint prop *> liftIO (FFI.refreshOutline object)

    paint :: CanvasProp FFI.Event i -> PixiDOM ()
    paint = \case
      Draw shape -> liftIO $ drawShape object shape
      Place t -> liftIO $ applyTransform object t
      Label value TextStyle {font, fontSize, textColor, textAlign} -> liftIO $ case font of
        SystemFont family ->
          FFI.setSystemText object value family fontSize textColor (textAlignName textAlign)
        AssetFont {family, source} ->
          FFI.setAssetText application object value family source fontSize textColor (textAlignName textAlign)
      Src (Texture asset) (Point width height) -> liftIO $ do
        FFI.setTexture application object asset
        FFI.centerAnchor object
        FFI.setSize object width height
      Interactive _ -> pass -- handled by syncEventMode
      Outline StrokeStyle {strokeColor, strokeWidth, strokeAlpha} padding ->
        liftIO $ FFI.setOutline application object strokeColor strokeWidth strokeAlpha padding
      Cursor value -> liftIO $ FFI.setCursor object value
      Hit area -> liftIO $ case area of
        RectHit (Point x y) (Point width height) ->
          FFI.setRectHitArea application object x y width height
        CircleHit (Point x y) radius ->
          FFI.setCircleHitArea application object x y radius
      Handler _ _ -> pass

atomicModifyMutVar'_ :: (PrimMonad m) => MutVar (PrimState m) a -> (a -> a) -> m ()
atomicModifyMutVar'_ ref f = atomicModifyMutVar' ref ((,()) . f)

applyTransform :: FFI.Object -> Transform -> IO ()
applyTransform object Transform {position = Point x y, scale = Point scaleX scaleY, rotation} = do
  FFI.setPosition object x y
  FFI.setScale object scaleX scaleY
  FFI.setRotation object rotation

drawShape :: FFI.Object -> Shape -> IO ()
drawShape object shape = do
  FFI.clearGraphics object
  case shape of
    Line (Point startX startY) (Point endX endY) stroke -> do
      FFI.moveTo object startX startY
      FFI.lineTo object endX endY
      applyStroke object stroke
    Rectangle (Point x y) (Point width height) fill stroke -> do
      FFI.rect object (-width / 2) (-height / 2) width height
      applyPaint object fill stroke
      FFI.setPosition object x y
    Circle (Point x y) radius fill stroke -> do
      FFI.circle object 0 0 radius
      applyPaint object fill stroke
      FFI.setPosition object x y
    Ellipse (Point x y) (Point radiusX radiusY) fill stroke -> do
      FFI.ellipse object 0 0 radiusX radiusY
      applyPaint object fill stroke
      FFI.setPosition object x y
    QuadraticBezier (Point startX startY) (Point controlX controlY) (Point endX endY) stroke -> do
      FFI.moveTo object startX startY
      FFI.quadraticCurveTo object controlX controlY endX endY
      applyStroke object stroke
    Bezier (Point startX startY) (Point c1X c1Y) (Point c2X c2Y) (Point endX endY) stroke -> do
      FFI.moveTo object startX startY
      FFI.bezierCurveTo object c1X c1Y c2X c2Y endX endY
      applyStroke object stroke
    Arc (Point x y) radius startAngle endAngle anticlockwise stroke -> do
      FFI.arc object x y radius startAngle endAngle anticlockwise
      applyStroke object stroke

applyPaint :: FFI.Object -> Maybe FillStyle -> Maybe StrokeStyle -> IO ()
applyPaint object fill stroke = do
  traverse_ (\FillStyle {fillColor, fillAlpha} -> FFI.fill object fillColor fillAlpha) fill
  traverse_ (applyStroke object) stroke

applyStroke :: FFI.Object -> StrokeStyle -> IO ()
applyStroke object StrokeStyle {strokeColor, strokeWidth, strokeAlpha} =
  FFI.stroke object strokeColor strokeWidth strokeAlpha
