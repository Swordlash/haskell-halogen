{-# LANGUAGE TemplateHaskell #-}

-- | The bindings to @jsbits/three.js@, declared once for every backend:
-- natively they are 'browserJS''s inert stubs, so the package type-checks
-- and a scene renders into nothing.
module Halogen.Three.FFI
  ( Holder
  , Object
  , Canvas
  , Timer
  , Callback
  , canvas
  , newHolder
  , initialize
  , ready
  , destroy
  , setPerspective
  , setOrthographic
  , setBackground
  , requestRender
  , setRoot
  , newObject
  , insertChildIx
  , removeChild
  , parentOf
  , sameObject
  , setGeometry
  , clearGeometry
  , setMaterial
  , clearMaterial
  , setPosition
  , setRotation
  , setScale
  , setVisible
  , setName
  , setLight
  , addHandler
  , removeHandler
  , onGesture
  , eventX
  , eventY
  , eventZ
  , eventName
  , eventButton
  , eventModifiers
  , pointerId
  , isPrimary
  , clientX
  , clientY
  , button
  , deltaY
  , preventDefault
  , canvasHeight
  , scheduleTimeout
  , cancelTimeout
  , mkCallback
  , freeCallback
  )
where

import Data.Foreign
import Halogen.JSBits hiding (mkCallback)
import Protolude
import Web.DOM.Internal.Types (HTMLElement (..))

-- | What a mounted scene keeps on the page's side: the Three.js module once
-- it has loaded, the renderer, the scene and the camera.
newtype Holder = Holder (Foreign Holder)
  deriving newtype (Inert)

-- | A Three.js @Object3D@: a group, a mesh, a light.
newtype Object = Object (Foreign Object)
  deriving newtype (Inert)

newtype Timer = Timer (Foreign Timer)
  deriving newtype (Inert)

newtype Canvas = Canvas (Foreign HTMLElement)

canvas :: HTMLElement -> Canvas
canvas (HTMLElement value) = Canvas value

$( browserJS
     ["jsbits/three.js"]
     [ ("newHolder", "halogen_three_new_holder", Safe, [t|IO Holder|])
     , ("initializeRaw", "halogen_three_initialize", Safe, [t|Holder -> JSText -> Canvas -> Callback -> IO ()|])
     , ("ready", "halogen_three_ready", Safe, [t|Holder -> IO Bool|])
     , ("destroy", "halogen_three_destroy", Safe, [t|Holder -> IO ()|])
     , ("setPerspective", "halogen_three_set_perspective", Safe, [t|Holder -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()|])
     , ("setOrthographic", "halogen_three_set_orthographic", Safe, [t|Holder -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()|])
     , ("setBackground", "halogen_three_set_background", Safe, [t|Holder -> Int -> IO ()|])
     , ("requestRender", "halogen_three_request_render", Safe, [t|Holder -> IO ()|])
     , ("setRoot", "halogen_three_set_root", Safe, [t|Holder -> Object -> IO ()|])
     , ("newObjectRaw", "halogen_three_new_object", Safe, [t|Holder -> JSText -> IO (Nullable Object)|])
     , ("insertChildIx", "halogen_three_insert_child_ix", Safe, [t|Object -> Object -> Int -> IO ()|])
     , ("removeChild", "halogen_three_remove_child", Safe, [t|Object -> Object -> IO ()|])
     , ("parentOfRaw", "halogen_three_parent_of", Safe, [t|Object -> IO (Nullable Object)|])
     , ("sameObject", "halogen_three_same_object", Safe, [t|Object -> Object -> IO Bool|])
     , ("setGeometry", "halogen_three_set_geometry", Safe, [t|Holder -> Object -> Int -> Double -> Double -> Double -> Double -> IO ()|])
     , ("clearGeometry", "halogen_three_clear_geometry", Safe, [t|Holder -> Object -> IO ()|])
     , ("setMaterial", "halogen_three_set_material", Safe, [t|Holder -> Object -> Int -> Int -> Double -> Bool -> Double -> Double -> IO ()|])
     , ("clearMaterial", "halogen_three_clear_material", Safe, [t|Holder -> Object -> IO ()|])
     , ("setPosition", "halogen_three_set_position", Safe, [t|Object -> Double -> Double -> Double -> IO ()|])
     , ("setRotation", "halogen_three_set_rotation", Safe, [t|Object -> Double -> Double -> Double -> IO ()|])
     , ("setScale", "halogen_three_set_scale", Safe, [t|Object -> Double -> Double -> Double -> IO ()|])
     , ("setVisible", "halogen_three_set_visible", Safe, [t|Object -> Bool -> IO ()|])
     , ("setNameRaw", "halogen_three_set_name", Safe, [t|Object -> JSText -> IO ()|])
     , ("setLight", "halogen_three_set_light", Safe, [t|Object -> Int -> Double -> IO ()|])
     , ("onRaw", "halogen_three_on", Safe, [t|Object -> JSText -> Callback -> IO ()|])
     , ("offRaw", "halogen_three_off", Safe, [t|Object -> JSText -> Callback -> IO ()|])
     , ("onGestureRaw", "halogen_three_on_gesture", Safe, [t|Holder -> JSText -> Callback -> IO ()|])
     , ("eventX", "halogen_three_event_x", Safe, [t|JSVal -> IO Double|])
     , ("eventY", "halogen_three_event_y", Safe, [t|JSVal -> IO Double|])
     , ("eventZ", "halogen_three_event_z", Safe, [t|JSVal -> IO Double|])
     , ("eventNameRaw", "halogen_three_event_name", Safe, [t|JSVal -> IO JSText|])
     , ("eventButton", "halogen_three_event_button", Safe, [t|JSVal -> IO Int|])
     , ("eventModifiers", "halogen_three_event_modifiers", Safe, [t|JSVal -> IO Int|])
     , ("pointerId", "halogen_three_pointer_id", Safe, [t|JSVal -> IO Int|])
     , ("isPrimary", "halogen_three_is_primary", Safe, [t|JSVal -> IO Bool|])
     , ("clientX", "halogen_three_client_x", Safe, [t|JSVal -> IO Double|])
     , ("clientY", "halogen_three_client_y", Safe, [t|JSVal -> IO Double|])
     , ("button", "halogen_three_button", Safe, [t|JSVal -> IO Int|])
     , ("deltaY", "halogen_three_delta_y", Safe, [t|JSVal -> IO Double|])
     , ("preventDefault", "halogen_three_prevent_default", Safe, [t|JSVal -> IO ()|])
     , ("canvasHeight", "halogen_three_canvas_height", Safe, [t|Holder -> IO Double|])
     , ("scheduleTimeout", "halogen_three_schedule_timeout", Safe, [t|Callback -> Int -> IO Timer|])
     , ("cancelTimeout", "halogen_three_cancel_timeout", Safe, [t|Timer -> IO ()|])
     ]
 )

-- | Load Three.js from the URL and start a renderer on the canvas; the
-- callback runs once it is up, or has failed to come up ('ready' says
-- which). Natively at once, and never ready.
initialize :: Holder -> Text -> Canvas -> Callback -> IO ()
initialize holder url canvas' done
  | inBrowser = initializeRaw holder (toJSText url) canvas' done
  | otherwise = invokeCallback done inert

newObject :: Holder -> Text -> IO (Maybe Object)
newObject holder name = fmap Object . nullableToMaybe <$> newObjectRaw holder (toJSText name)

parentOf :: Object -> IO (Maybe Object)
parentOf object = fmap Object . nullableToMaybe <$> parentOfRaw object

setName :: Object -> Text -> IO ()
setName object = setNameRaw object . toJSText

addHandler :: Object -> Text -> Callback -> IO ()
addHandler object eventType = onRaw object (toJSText eventType)

removeHandler :: Object -> Text -> Callback -> IO ()
removeHandler object eventType = offRaw object (toJSText eventType)

-- | The renderer's own handler of a kind of pointer event on the canvas
-- ("down", "move", "up", "wheel"), run before the objects' handlers.
onGesture :: Holder -> Text -> Callback -> IO ()
onGesture holder kind = onGestureRaw holder (toJSText kind)

eventName :: JSVal -> IO Text
eventName event = fromJSText <$> eventNameRaw event

-- | A handler the page calls with an event. It runs at once, so that it
-- can prevent the event's default (the wheel's scroll), and so that the
-- renderer's handlers have run before the objects' do.
mkCallback :: (JSVal -> IO ()) -> IO Callback
mkCallback = mkSyncCallback
