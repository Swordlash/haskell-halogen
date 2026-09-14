module Halogen.Canvas
  ( Renderer (..)
  , MountedRenderer (..)
  , component
  )
where

import Data.Row (type Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Subscription qualified as HS
import Protolude
import Web.DOM.Internal.Types (HTMLElement)

-- | An imperative rendering backend hidden behind a declarative update API.
data Renderer scene event = Renderer
  { mount :: HTMLElement -> (event -> IO ()) -> IO (MountedRenderer scene)
  }

-- | A mounted backend. Halogen calls 'update' whenever its input changes and
-- 'destroy' when the component is finalized.
data MountedRenderer scene = MountedRenderer
  { update :: scene -> IO ()
  , destroy :: IO ()
  }

data CanvasState scene = CanvasState
  { scene :: scene
  , mounted :: Maybe (MountedRenderer scene)
  }

data Action scene event
  = Mount
  | Receive scene
  | Emit event
  | Unmount

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "halogen-canvas"

-- | A reusable Halogen component that owns a canvas DOM node while delegating
-- rendering to a backend.
component :: forall scene event. Renderer scene event -> H.Component H.VoidF scene event IO
component renderer =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \scene -> pure CanvasState {scene, mounted = Nothing}
      , render
      , eval =
          H.mkEval $
            H.defaultEval
              { H.handleAction = handleAction
              , H.initialize = Just Mount
              , H.finalize = Just Unmount
              , H.receive = Just . Receive
              }
      }
  where
    render :: CanvasState scene -> H.ComponentHTML (Action scene event) Empty IO
    render _ =
      HH.canvas
        [ HP.ref canvasRef
        , HP.styleText "display:block;width:100%;height:100%;touch-action:none"
        ]

    handleAction = \case
      Mount -> do
        canvas <- H.getHTMLElementRef canvasRef
        for_ canvas $ \element -> do
          events <- liftIO HS.create
          void $ H.subscribe $ Emit <$> events.emitter
          mounted <- liftIO $ renderer.mount element (HS.notify events.listener)
          current <- gets (.scene)
          liftIO $ mounted.update current
          modify $ \currentState -> currentState {mounted = Just mounted}
      Receive scene -> do
        modify $ \currentState -> currentState {scene}
        gets (.mounted) >>= traverse_ (\mounted -> liftIO $ mounted.update scene)
      Emit event -> H.raise event
      Unmount -> gets (.mounted) >>= traverse_ (liftIO . (.destroy))
