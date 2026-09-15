module Halogen.Canvas
  ( Renderer (..)
  , MountedRenderer (..)
  , component
  )
where

import Control.Monad.UUID
import Data.Row (type Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Subscription qualified as HS
import Protolude
import Web.DOM.Internal.Types (HTMLElement)

-- | An imperative rendering backend hidden behind a declarative update API.
newtype Renderer scene event m = Renderer
  { mount :: HTMLElement -> (event -> m ()) -> m (MountedRenderer scene m)
  }

-- | A mounted backend. Halogen calls 'update' whenever its input changes and
-- 'destroy' when the component is finalized.
data MountedRenderer scene m = MountedRenderer
  { update :: scene -> m ()
  , destroy :: m ()
  }

data CanvasState scene m = CanvasState
  { scene :: scene
  , mounted :: Maybe (MountedRenderer scene m)
  , canvasRef :: H.RefLabel
  }

data Action scene event
  = Mount
  | Receive scene
  | Emit event
  | Unmount

-- | A reusable Halogen component that owns a canvas DOM node while delegating
-- rendering to a backend.
component
  :: forall scene event m
   . (MonadIO m, MonadUUID m)
  => Renderer scene event m
  -> H.Component H.VoidF scene event m
component renderer =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \scene -> do
          uuid <- show <$> generateV4
          pure CanvasState {scene, mounted = Nothing, canvasRef = H.RefLabel $ "canvas-" <> uuid}
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
    render :: CanvasState scene m -> H.ComponentHTML (Action scene event) Empty m
    render CanvasState {canvasRef} =
      HH.canvas
        [ HP.ref canvasRef
        , HP.styleText "display:block;width:100%;height:100%;touch-action:none"
        ]

    handleAction = \case
      Mount -> do
        CanvasState {canvasRef} <- get
        canvas <- H.getHTMLElementRef canvasRef
        for_ canvas $ \element -> do
          events <- liftIO HS.create
          void $ H.subscribe $ Emit <$> events.emitter
          mounted <- lift $ renderer.mount element (liftIO . HS.notify events.listener)
          current <- gets (.scene)
          lift $ mounted.update current
          modify $ \currentState -> currentState {mounted = Just mounted}
      Receive scene -> do
        modify $ \currentState -> currentState {scene}
        gets (.mounted) >>= traverse_ (\mounted -> lift $ mounted.update scene)
      Emit event -> H.raise event
      Unmount -> gets (.mounted) >>= traverse_ (lift . (.destroy))
