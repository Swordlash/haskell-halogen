{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for a PixiJS scene graph.
--
-- The point of the exercise: the reconciler in "Halogen.VDom.DOM" is written
-- against a tree of nodes, and a Pixi display list is one. Nothing here is a
-- browser DOM node — 'DomNode' is 'FFI.Object', a Pixi @Container@ — which is
-- why the node types had to become associated before this module could exist.
--
-- What stands in for the document is the 'FFI.Application': element creation
-- needs it, because a @Container@ or @Graphics@ is constructed from the pixi
-- module the application holds.
--
-- There is deliberately no 'Halogen.VDom.DOM.Monad.MonadAttributes' instance.
-- A @Graphics@ is configured by replaying a drawing command sequence, not by
-- setting named string attributes, so the canvas scene language brings its
-- own prop applicator rather than reusing "Halogen.VDom.DOM.Prop".
module Halogen.Canvas.Pixi.Monad
  ( PixiDOM (..)
  , runPixiDOM
  )
where

import Control.Monad.Primitive (PrimMonad (..))
import Halogen.Canvas.Pixi.FFI qualified as FFI
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Types (ElemName (..))
import Protolude
import Web.Event.Event (EventType (..))

newtype PixiDOM a = PixiDOM (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance PrimMonad PixiDOM where
  type PrimState PixiDOM = PrimState IO
  primitive = PixiDOM . primitive

runPixiDOM :: PixiDOM a -> IO a
runPixiDOM (PixiDOM io) = io

instance MonadDOM PixiDOM where
  type DomNode PixiDOM = FFI.Object
  type DomElement PixiDOM = FFI.Object
  type DomDocument PixiDOM = FFI.Application
  type DomEventListener PixiDOM = FFI.Callback
  type DomEventTarget PixiDOM = FFI.Object

  -- A display object is its own node and its own event target; Pixi draws
  -- none of the distinctions the browser does.
  elementToNode = pure
  elementToEventTarget = pure

  createElement _ (ElemName name) application = liftIO $ case name of
    "container" -> FFI.newContainer application
    "graphics" -> FFI.newGraphics application
    "text" -> FFI.newText application
    "sprite" -> FFI.newSprite application
    other -> panic $ "Halogen.Canvas.Pixi: no such element: " <> other

  -- Pixi has text *objects*, which are elements with a position and a style,
  -- but no text *nodes* - nothing that is a bare string sitting among its
  -- siblings. A label is Elem "text" with its content in a prop, so the
  -- reconciler's text path is unreachable here.
  createTextNode _ _ = panic "Halogen.Canvas.Pixi: a Pixi label is an element, not a text node"
  setTextContent _ _ = panic "Halogen.Canvas.Pixi: a Pixi label is an element, not a text node"

  insertChildIx ix child parent = liftIO $ do
    FFI.addChild parent child
    FFI.setChildIndex parent child ix

  removeChild child parent = liftIO $ FFI.removeChild parent child
  parentNode = liftIO . FFI.parentOf

  mkEventListener handler = liftIO $ FFI.mkCallback (runPixiDOM . handler)

  addEventListener (EventType eventName) listener target =
    liftIO $ FFI.addListener target eventName listener

  removeEventListener (EventType eventName) listener target =
    liftIO $ FFI.removeListener target eventName listener
