{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Halogen.VDom.DOM.Prop
  ( Prop (..)
  , ElemRef (..)
  , PropValue (..)
  , buildProp
  )
where

import Data.Foreign
import Data.Map.Strict qualified as M
import HPrelude hiding (state)
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Machine qualified as V
import Halogen.VDom.Types
import Halogen.VDom.Utils qualified as Util
import Web.DOM.Element
import Web.DOM.Internal.Types qualified as DOM
import Web.Event.Event
import Web.Event.Event qualified as DOM
import Web.HTML.Common

data Prop msg
  = Attribute (Maybe Namespace) AttrName Text
  | forall val. Property (PropName val) (PropValue val)
  | Handler EventType (Event -> Maybe msg)
  | Ref (ElemRef Element -> Maybe msg)

deriving instance Functor Prop

data ElemRef a
  = Created a
  | Removed a
  deriving (Functor)

type EventMap dom a = Map Text (DOM.EventListener, Ref dom (Event -> Maybe a))

data PropState dom a = PropState
  { events :: Ref dom (EventMap dom a)
  , props :: Map Text (Prop a)
  }

propToStrKey :: Prop i -> Text
propToStrKey = \case
  Attribute (Just (Namespace ns)) (AttrName attr) _ -> "attr/" <> ns <> ":" <> attr
  Attribute _ (AttrName attr) _ -> "attr/:" <> attr
  Property (PropName prop) _ -> "prop/" <> prop
  Handler (DOM.EventType ty) _ -> "handler/" <> ty
  Ref _ -> "ref"

-- Specialisations live with each backend now that the class is no longer
-- pinned to IO; the unfolding has to be exported for them to fire.
{-# INLINEABLE buildProp #-}

-- | Apply a property list to an element, and keep applying it across patches.
--
-- Both directions between the component monad and the DOM monad are needed
-- here, and only here. @runDom@ is the cheap one: setting an attribute is a
-- DOM effect that the caller sequences. @toDom@ is the expensive one, and it
-- exists because a registered event listener is called back /by the DOM/ —
-- when it fires it has to run component code, so the component monad must be
-- runnable from inside a DOM callback. That is what forces an unlift on the
-- caller; the reconciler itself never needs it.
buildProp
  :: forall dom m a
   . (MonadDOM dom, Monad m)
  => (forall x. dom x -> m x)
  -> (forall x. m x -> dom x)
  -> (a -> m ())
  -> DOM.Element
  -> V.Machine m [Prop a] ()
buildProp runDom toDom emit el = renderProp
  where
    renderProp :: V.Machine m [Prop a] ()
    renderProp ps1 = do
      events <- runDom $ newRef mempty
      ps1' <- Util.strMapWithIxE ps1 propToStrKey (applyProp events)
      let state =
            PropState
              { events
              , props = ps1'
              }
      pure $ V.Step () state patchProp haltProp

    patchProp :: PropState dom a -> [Prop a] -> m (V.Step m [Prop a] ())
    patchProp state ps2 = do
      events <- runDom $ newRef mempty
      let PropState {events = prevEvents, props = ps1} = state
          onThese = diffProp prevEvents events
          onThis = removeProp prevEvents
          onThat = applyProp events
      props <- Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
      let nextState =
            PropState
              { events
              , props
              }
      pure $ V.Step () nextState patchProp haltProp

    haltProp state = do
      case M.lookup "ref" state.props of
        Just (Ref f) ->
          mbEmit (f (Removed el))
        _ -> pass

    mbEmit = traverse_ emit

    applyProp :: Ref dom (EventMap dom a) -> Text -> Int -> Prop a -> m (Prop a)
    applyProp events _ _ v =
      case v of
        Attribute ns attr val -> do
          runDom $ setAttribute ns attr val el
          pure v
        Property prop val -> do
          runDom $ setProperty prop val el
          pure v
        Handler evty@(DOM.EventType ty) f -> do
          M.lookup ty
            <$> runDom (readRef events)
            >>= \case
              Just handler -> do
                runDom $ writeRef (snd handler) f
                pure v
              _ -> runDom $ do
                ref <- newRef f
                listener <- mkEventListener $ \ev -> do
                  f' <- readRef ref
                  toDom $ mbEmit (f' ev)
                modifyRef' events (M.insert ty (listener, ref))
                addEventListener evty listener $ toEventTarget el
                pure v
        Ref f -> do
          mbEmit (f (Created el))
          pure v

    diffProp
      :: Ref dom (EventMap dom a)
      -> Ref dom (EventMap dom a)
      -> Text
      -> Int
      -> Prop a
      -> Prop a
      -> m (Prop a)
    diffProp prevEvents events _ _ v1 v2 = do
      case (v1, v2) of
        (Attribute _ _ val1, Attribute ns2 attr2 val2) ->
          if val1 == val2
            then pure v2
            else do
              runDom $ setAttribute ns2 attr2 val2 el
              pure v2
        (Property _ val1, Property prop2 val2) ->
          case (val1 `unsafeRefEq'` val2, prop2) of
            (True, _) ->
              pure v2
            (_, "value") -> do
              isEqual <- runDom $ propertyEquals "value" val2 el
              if isEqual
                then pure v2
                else do
                  runDom $ setProperty prop2 val2 el
                  pure v2
            (_, _) -> do
              runDom $ setProperty prop2 val2 el
              pure v2
        (Handler _ _, Handler (DOM.EventType ty) f) -> do
          handler <- runDom $ (M.! ty) <$> readRef prevEvents
          runDom $ writeRef (snd handler) f
          runDom $ modifyRef' events (M.insert ty handler)
          pure v2
        (_, _) ->
          pure v2

    removeProp prevEvents _ v =
      case v of
        Attribute ns attr _ ->
          runDom $ removeAttribute ns attr el
        Property prop _ ->
          runDom $ removeProperty prop el
        Handler evty@(DOM.EventType ty) _ -> do
          handler <- runDom $ (M.! ty) <$> readRef prevEvents
          runDom $ removeEventListener evty (fst handler) $ toEventTarget el
        Ref _ -> pass
