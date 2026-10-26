-- | 'MonadDOM' for a Three.js scene graph, as "Halogen.Canvas.Pixi.Monad"
-- is for a Pixi display list: the reconciler in "Halogen.VDom.DOM" is
-- written against a tree of nodes, and an @Object3D@ with its children is
-- one. What stands in for the document is the 'FFI.Holder', which holds the
-- Three.js module objects are made from.
--
-- There is no 'Halogen.VDom.DOM.Monad.MonadAttributes' instance: a mesh is
-- configured by geometry and material values, not by named string
-- attributes, so the scene brings its own prop applicator
-- ("Halogen.Three.Prop").
module Halogen.Three.Monad
  ( ThreeDOM (..)
  , runThreeDOM
  )
where

import Control.Monad.Primitive (PrimMonad (..))
import Halogen.Three.FFI qualified as FFI
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Types (ElemName (..))
import Protolude
import System.IO.Error (userError)
import Web.Event.Event (EventType (..))
import Web.Event.Internal.Types (Event (..))

newtype ThreeDOM a = ThreeDOM (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance PrimMonad ThreeDOM where
  type PrimState ThreeDOM = PrimState IO
  primitive = ThreeDOM . primitive

runThreeDOM :: ThreeDOM a -> IO a
runThreeDOM (ThreeDOM io) = io

-- | A failure in the scene's description, raised in IO rather than from
-- pure code.
misuse :: Text -> ThreeDOM a
misuse what = liftIO (throwIO (userError (toS ("Halogen.Three: " <> what))))

instance MonadDOM ThreeDOM where
  type DomNode ThreeDOM = FFI.Object
  type DomElement ThreeDOM = FFI.Object
  type DomDocument ThreeDOM = FFI.Holder
  type DomEventListener ThreeDOM = FFI.Callback
  type DomEventTarget ThreeDOM = FFI.Object

  -- An object is its own node and its own event target.
  elementToNode = pure
  elementToEventTarget = pure

  createElement _ (ElemName name) holder =
    liftIO (FFI.newObject holder name) >>= \case
      Just object -> pure object
      Nothing -> misuse ("no such object: " <> name)

  -- A scene has no text: what is written in it is a texture, which is
  -- not a node among its siblings. The elements never make one.
  createTextNode _ _ = misuse "a scene has no text nodes"
  setTextContent _ _ = misuse "a scene has no text nodes"

  insertChildIx ix child parent = liftIO $ FFI.insertChildIx parent child ix

  -- Out of the tree and its own geometry and material disposed of: nothing
  -- comes back for an object the reconciler takes out, and Three.js keeps
  -- their GPU buffers until told. Its children's are theirs to let go of:
  -- halting an element takes each of its children out of it in turn.
  removeChild child parent = liftIO $ FFI.removeChild parent child
  parentNode = liftIO . FFI.parentOf

  -- Two handles for one object need not be equal (on wasm each crossing of
  -- the FFI makes a new one), so identity is asked of the page's side.
  sameNode a b = liftIO $ FFI.sameObject a b

  -- A scene is never adopted from a page: answering as if every object
  -- were empty and not an element makes hydrating one build it afresh.
  nextSibling _ = pure Nothing
  firstChild _ = pure Nothing
  nodeKind _ = pure OtherKind
  nodeToElement _ = pure Nothing

  mkEventListener handler = liftIO $ FFI.mkCallback (runThreeDOM . handler . Event)

  addEventListener (EventType name) listener target =
    liftIO $ FFI.addHandler target name listener

  removeEventListener (EventType name) listener target =
    liftIO $ FFI.removeHandler target name listener
