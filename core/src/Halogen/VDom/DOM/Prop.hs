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

type EventMap m a = Map Text (DOM.EventListener, Ref m (Event -> Maybe a))

data PropState m a = PropState
  { events :: Ref m (EventMap m a)
  , props :: Map Text (Prop a)
  }

propToStrKey :: Prop i -> Text
propToStrKey = \case
  Attribute (Just (Namespace ns)) (AttrName attr) _ -> "attr/" <> ns <> ":" <> attr
  Attribute _ (AttrName attr) _ -> "attr/:" <> attr
  Property (PropName prop) _ -> "prop/" <> prop
  Handler (DOM.EventType ty) _ -> "handler/" <> ty
  Ref _ -> "ref"

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
{-# SPECIALISE buildProp :: (a -> IO ()) -> DOM.Element -> V.Machine IO [Prop a] () #-}
#endif
buildProp
  :: forall m a
   . (MonadDOM m)
  => (a -> m ())
  -> DOM.Element
  -> V.Machine m [Prop a] ()
buildProp emit el = renderProp
  where
    renderProp :: V.Machine m [Prop a] ()
    renderProp ps1 = do
      events <- newRef mempty
      ps1' <- Util.strMapWithIxE ps1 propToStrKey (applyProp events)
      let state =
            PropState
              { events
              , props = ps1'
              }
      pure $ V.Step () state patchProp haltProp

    patchProp :: PropState m a -> [Prop a] -> m (V.Step m [Prop a] ())
    patchProp state ps2 = do
      events <- newRef mempty
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

    applyProp :: Ref m (EventMap m a) -> Text -> Int -> Prop a -> m (Prop a)
    applyProp events _ _ v =
      case v of
        Attribute ns attr val -> do
          setAttribute ns attr val el
          pure v
        Property prop val -> do
          setProperty prop val el
          pure v
        Handler evty@(DOM.EventType ty) f -> do
          M.lookup ty
            <$> readRef events
            >>= \case
              Just handler -> do
                writeRef (snd handler) f
                pure v
              _ -> do
                ref <- newRef f
                listener <- mkEventListener $ \ev -> do
                  f' <- readRef ref
                  mbEmit (f' ev)
                modifyRef' events (M.insert ty (listener, ref))
                addEventListener evty listener $ toEventTarget el
                pure v
        Ref f -> do
          mbEmit (f (Created el))
          pure v

    diffProp
      :: Ref m (EventMap m a)
      -> Ref m (EventMap m a)
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
              setAttribute ns2 attr2 val2 el
              pure v2
        (Property _ val1, Property prop2 val2) ->
          case (val1 `unsafeRefEq'` val2, prop2) of
            (True, _) ->
              pure v2
            (_, "value") -> do
              isEqual <- propertyEquals "value" val2 el
              if isEqual
                then pure v2
                else do
                  setProperty prop2 val2 el
                  pure v2
            (_, _) -> do
              setProperty prop2 val2 el
              pure v2
        (Handler _ _, Handler (DOM.EventType ty) f) -> do
          handler <- (M.! ty) <$> readRef prevEvents
          writeRef (snd handler) f
          modifyRef' events (M.insert ty handler)
          pure v2
        (_, _) ->
          pure v2

    removeProp prevEvents _ v =
      case v of
        Attribute ns attr _ ->
          removeAttribute ns attr el
        Property prop _ ->
          removeProperty prop el
        Handler evty@(DOM.EventType ty) _ -> do
          handler <- (M.! ty) <$> readRef prevEvents
          removeEventListener evty (fst handler) $ toEventTarget el
        Ref _ -> pass
