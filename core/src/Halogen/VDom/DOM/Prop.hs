{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Halogen.VDom.DOM.Prop
  ( Prop (..)
  , HandlerOptions (..)
  , handlerOptions
  , EventMatch (..)
  , KeyMatch (..)
  , keyMatch
  , ElemRef (..)
  , PropValue (..)
  , buildProp
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Map.Strict qualified as M
import Data.Primitive.MutVar
import HPrelude hiding (state)
import Halogen.VDom.DOM.HandlerOptions
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Machine qualified as V
import Halogen.VDom.Types
import Halogen.VDom.Utils qualified as Util
import Web.DOM.Element (Element)
import Web.DOM.Internal.Types qualified as DOM
import Web.Event.Event hiding (defaultPrevented, target, type_)
import Web.Event.Event qualified as DOM
import Web.HTML.Common

data Prop msg
  = Attribute (Maybe Namespace) AttrName Text
  | forall val. Property (PropName val) (PropValue val)
  | -- | A listener for events of a type: the function says, while the event
    -- is being dispatched, what message it makes, if any, and the options
    -- which events it is asked about and what the listener does to the
    -- event itself ('HandlerOptions').
    Handler EventType HandlerOptions (Event -> Maybe msg)
  | Ref (ElemRef Element -> Maybe msg)

deriving instance Functor Prop

data ElemRef a
  = Created a
  | Removed a
  deriving (Functor)

type EventMap m a = Map Text (DomEventListener m, MutVar (PrimState m) (HandlerOptions, Event -> Maybe a))

data PropState m a = PropState
  { events :: MutVar (PrimState m) (EventMap m a)
  , props :: Map Text (Prop a)
  }

propToStrKey :: Prop i -> Text
propToStrKey = \case
  Attribute (Just (Namespace ns)) (AttrName attr) _ -> "attr/" <> ns <> ":" <> attr
  Attribute _ (AttrName attr) _ -> "attr/:" <> attr
  Property (PropName prop) _ -> "prop/" <> prop
  Handler (DOM.EventType ty) _ _ -> "handler/" <> ty
  Ref _ -> "ref"

{-# INLINEABLE buildProp #-}
#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
{-# SPECIALISE buildProp :: (a -> BrowserDOM ()) -> DOM.Element -> V.Machine BrowserDOM [Prop a] () #-}
#else
{-# SPECIALISE buildProp :: (a -> MemDOM ()) -> DOM.Element -> V.Machine MemDOM [Prop a] () #-}
#endif

-- | Apply a property list to an element, and keep applying it across patches.
buildProp
  :: forall m a
   . (MonadAttributes m, MonadIO m, DomElement m ~ DOM.Element)
  => (a -> m ())
  -> DOM.Element
  -> V.Machine m [Prop a] ()
buildProp emit el = renderProp
  where
    renderProp :: V.Machine m [Prop a] ()
    renderProp ps1 = do
      events <- newMutVar mempty
      ps1' <- Util.strMapWithIxE ps1 propToStrKey (applyProp events)
      let state =
            PropState
              { events
              , props = ps1'
              }
      pure $ V.Step () state patchProp haltProp

    patchProp :: PropState m a -> [Prop a] -> m (V.Step m [Prop a] ())
    patchProp state ps2 = do
      events <- newMutVar mempty
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

    mbEmit :: Maybe a -> m ()
    mbEmit = traverse_ emit

    applyProp :: MutVar (PrimState m) (EventMap m a) -> Text -> Int -> Prop a -> m (Prop a)
    applyProp events _ _ v =
      case v of
        Attribute ns attr val -> do
          setAttribute ns attr val el
          pure v
        Property prop val -> do
          setProperty prop val el
          pure v
        Handler evty@(DOM.EventType ty) options f -> do
          M.lookup ty
            <$> readMutVar events
            >>= \case
              Just handler -> do
                replaceHandler evty handler options f
                pure v
              _ -> do
                ref <- newMutVar (options, f)
                -- The options are acted on here, in the listener, while the
                -- event is being dispatched: 'emit' may only queue the
                -- message for later (see 'HandlerOptions'). A handler with
                -- no match declared, the usual kind, costs nothing more
                -- than the test of its 'matching'.
                listener <- mkEventListener $ \ev -> do
                  (options', f') <- readMutVar ref
                  case options'.matching of
                    Nothing -> for_ (f' ev) $ \msg -> do
                      actOn options' ev
                      emit msg
                    -- The function decides before the listener acts, as
                    -- it does with no match: one that reads the event in
                    -- the listener ('Web.Event.Event.defaultPreventedNow')
                    -- sees it as the page dispatched it, not as this
                    -- handler's own 'preventDefault' left it.
                    Just match -> when (matchesEvent match ev) $ do
                      let message = f' ev
                      message `seq` actOn options' ev
                      traverse_ emit message
                atomicModifyMutVar'_ events (M.insert ty (listener, ref))
                target <- elementToEventTarget el
                addEventListener evty listener target
                when (options /= handlerOptions) $ setListenerOptions evty options listener target
                pure v
        Ref f -> do
          mbEmit (f (Created el))
          pure v

    diffProp
      :: MutVar (PrimState m) (EventMap m a)
      -> MutVar (PrimState m) (EventMap m a)
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
        -- A property is written only when its value changes, compared as the
        -- value the element would hold, which is what purescript-halogen-vdom's
        -- refEq decides for JavaScript scalars. Writing it on every patch would
        -- undo whatever the page changed in between: classes a JavaScript
        -- widget added, or a checkbox the user ticked.
        --
        -- @value@ is the exception, compared against the element itself on
        -- every patch: the user edits it, so an unchanged rendered value is not
        -- evidence that the element still holds it, and a component that turns
        -- an edit down expects its value put back.
        (Property _ val1, Property prop2 val2)
          | prop2 == "value" -> do
              isEqual <- propertyEquals "value" val2 el
              unless isEqual $ setProperty prop2 val2 el
              pure v2
          | propScalar val1 == propScalar val2 ->
              pure v2
          | otherwise -> do
              setProperty prop2 val2 el
              pure v2
        (Handler {}, Handler evty@(DOM.EventType ty) options f) -> do
          handler <- (M.! ty) <$> readMutVar prevEvents
          replaceHandler evty handler options f
          atomicModifyMutVar'_ events (M.insert ty handler)
          pure v2
        -- A ref kept across a patch is told the element went from the old
        -- function and came to the new one. A ref holds a function, not a
        -- label, so the patch cannot tell whether the label changed; when
        -- it did, the old label has to stop holding the element and the
        -- new one has to start. The driver clears a label on 'Removed'
        -- only while it holds this very element (see @renderPass@ in
        -- "Halogen.IO.Driver.Mount"), so 'Removed' first and 'Created'
        -- second leave the element under the new label and nothing of it
        -- under the old one, and an unchanged label is cleared and written
        -- again with the same element. The other order would lose it: the
        -- 'Removed' after the 'Created' would find the element it removes
        -- and take the label away.
        (Ref f1, Ref f2) -> do
          mbEmit (f1 (Removed el))
          mbEmit (f2 (Created el))
          pure v2
        (_, _) ->
          pure v2

    -- Give a listener that stays the options and the function it uses
    -- next, and tell the backend when the options changed.
    replaceHandler :: DOM.EventType -> (DomEventListener m, MutVar (PrimState m) (HandlerOptions, Event -> Maybe a)) -> HandlerOptions -> (Event -> Maybe a) -> m ()
    replaceHandler evty (listener, ref) options f = do
      (previous, _) <- readMutVar ref
      writeMutVar ref (options, f)
      when (previous /= options)
        $ elementToEventTarget el
        >>= setListenerOptions evty options listener

    actOn :: HandlerOptions -> Event -> m ()
    actOn options ev = do
      when options.preventDefault $ DOM.preventDefault ev
      when options.stopPropagation $ DOM.stopPropagation ev

    removeProp prevEvents _ v =
      case v of
        Attribute ns attr _ ->
          removeAttribute ns attr el
        Property prop _ ->
          removeProperty prop el
        Handler evty@(DOM.EventType ty) _ _ -> do
          handler <- (M.! ty) <$> readMutVar prevEvents
          elementToEventTarget el >>= removeEventListener evty (fst handler)
        -- A ref that a patch drops lets go of the element, which stays: no
        -- halt is coming to say so while the element is there, and when it
        -- comes, 'haltProp' finds no ref among the props to tell.
        Ref f ->
          mbEmit (f (Removed el))

-- | 'atomicModifyMutVar'' with the result discarded.
atomicModifyMutVar'_ :: (PrimMonad m) => MutVar (PrimState m) a -> (a -> a) -> m ()
atomicModifyMutVar'_ ref f = atomicModifyMutVar' ref ((,()) . f)
