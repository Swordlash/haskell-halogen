module Halogen.VDom.DOM
  ( VDomSpec (..)
  , VDomMachine
  , VDomStep
  , buildVDom
  , buildText
  , buildElem
  , buildWidget
  , buildKeyed
  )
where

import HPrelude hiding (state)
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Machine
import Halogen.VDom.Types
import Halogen.VDom.Utils

{-# INLINEABLE buildVDom #-}

{-# INLINEABLE buildText #-}

{-# INLINEABLE buildKeyed #-}

{-# INLINEABLE buildElem #-}

{-# INLINEABLE buildWidget #-}

type VDomMachine dom m a w = Machine m (VDom a w) (DomNode dom)

type VDomStep dom m a w = Step m (VDom a w) (DomNode dom)

-- | How to render a 'VDom' tree.
--
-- The two monads are deliberately separate. @m@ is the component monad: it is
-- what child component slots evaluate in, and it is whatever the application
-- chose. @dom@ is the monad the DOM itself is spoken in, and it is fixed by
-- the backend. 'runDom' is the bridge, and it only ever runs in this
-- direction — see "Halogen.VDom.DOM.Prop" for the one place that needs the
-- other one, and why it has to pay for an unlift to get it.
data VDomSpec dom m a w = VDomSpec
  { runDom :: forall x. dom x -> m x
  , buildWidget :: VDomSpec dom m a w -> Machine m w (DomNode dom)
  , buildAttributes :: DomElement dom -> Machine m a ()
  , document :: DomDocument dom
  }

buildVDom :: (MonadDOM dom, Monad m) => VDomSpec dom m a w -> VDomMachine dom m a w
buildVDom spec = build
  where
    build = \case
      Text txt -> buildText spec build txt
      Elem ns n props children -> buildElem spec build ns n props children
      Keyed ns n props children -> buildKeyed spec build ns n props children
      Widget w -> buildWidget spec build w
      Grafted g -> build (runGraft g)

----------------------------------------------------------------------

data TextState dom m a w = TextState
  { runDom :: forall x. dom x -> m x
  , build :: VDomMachine dom m a w
  , node :: DomNode dom
  , value :: Text
  }

buildText :: (MonadDOM dom, Monad m) => VDomSpec dom m a w -> VDomMachine dom m a w -> Text -> m (VDomStep dom m a w)
buildText VDomSpec {runDom, document = doc} build value = do
  node <- runDom $ createTextNode value doc
  let state = TextState {runDom, build, node, value}
  pure $ Step node state patchText haltText

patchText :: (MonadDOM dom, Monad m) => TextState dom m a w -> VDom a w -> m (VDomStep dom m a w)
patchText state@TextState {runDom, build, node, value = value1} vdom =
  case vdom of
    Text value2
      | value1 == value2 ->
          pure $ Step node state patchText haltText
      | otherwise -> do
          let nextState = TextState {runDom, build, node, value = value2}
          runDom $ setTextContent value2 node
          pure $ Step node nextState patchText haltText
    _ -> do
      haltText state
      build vdom

haltText :: (MonadDOM dom, Monad m) => TextState dom m a w -> m ()
haltText TextState {runDom, node} =
  runDom $ traverse_ (removeChild node) =<< parentNode node

----------------------------------------------------------------------

data KeyedState dom m a w = KeyedState
  { runDom :: forall x. dom x -> m x
  , build :: VDomMachine dom m a w
  , node :: DomNode dom
  , attrs :: Step m a ()
  , ns :: Maybe Namespace
  , name :: ElemName
  , children :: Map Text (VDomStep dom m a w)
  , length :: Int
  }

buildKeyed :: (MonadDOM dom, Monad m) => VDomSpec dom m a w -> VDomMachine dom m a w -> Maybe Namespace -> ElemName -> a -> [(Text, VDom a w)] -> m (VDomStep dom m a w)
buildKeyed spec@VDomSpec {runDom, document = doc} build ns1 name1 as1 ch1 = do
  el <- runDom $ createElement ns1 name1 doc
  node <- runDom $ elementToNode el
  let onChild _ ix (_, vdom) = do
        res <- build vdom
        runDom $ insertChildIx ix (extract res) node
        pure res
  children <- strMapWithIxE ch1 fst onChild
  attrs <- spec.buildAttributes el as1
  let state =
        KeyedState
          { runDom
          , build
          , node
          , attrs
          , ns = ns1
          , name = name1
          , children
          , length = length ch1
          }
  pure $ Step node state patchKeyed haltKeyed

patchKeyed :: (MonadDOM dom, Monad m) => KeyedState dom m a w -> VDom a w -> m (VDomStep dom m a w)
patchKeyed state@KeyedState {runDom, build, node, attrs, ns = ns1, name = name1, children = ch1, length = len1} vdom =
  case vdom of
    Grafted g ->
      patchKeyed state (runGraft g)
    Keyed ns2 name2 as2 ch2 | (ns1, name1) == (ns2, name2) ->
      case (len1, length ch2) of
        (0, 0) -> do
          attrs2 <- step attrs as2
          let nextState =
                KeyedState
                  { runDom
                  , build
                  , node
                  , attrs = attrs2
                  , ns = ns2
                  , name = name2
                  , children = ch1
                  , length = 0
                  }
          pure $ Step node nextState patchKeyed haltKeyed
        (_, len2) -> do
          let onThese _ ix' s (_, v) = do
                res <- step s v
                runDom $ insertChildIx ix' (extract res) node
                pure res
              onThis _ = halt
              onThat _ ix (_, v) = do
                res <- build v
                runDom $ insertChildIx ix (extract res) node
                pure res
          children2 <- diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
          attrs2 <- step attrs as2
          let nextState =
                KeyedState
                  { runDom
                  , build
                  , node
                  , attrs = attrs2
                  , ns = ns2
                  , name = name2
                  , children = children2
                  , length = len2
                  }
          pure $ Step node nextState patchKeyed haltKeyed
    _ -> do
      haltKeyed state
      build vdom

haltKeyed :: (MonadDOM dom, Monad m) => KeyedState dom m a w -> m ()
haltKeyed (KeyedState {runDom, node, attrs, children}) = do
  parent <- runDom $ parentNode node
  runDom $ traverse_ (removeChild node) parent
  for_ children halt
  halt attrs

----------------------------------------------------------------------

data ElemState dom m a w = ElemState
  { runDom :: forall x. dom x -> m x
  , build :: VDomMachine dom m a w
  , node :: DomNode dom
  , attrs :: Step m a ()
  , ns :: Maybe Namespace
  , name :: ElemName
  , children :: [VDomStep dom m a w]
  }

buildElem
  :: (MonadDOM dom, Monad m)
  => VDomSpec dom m a w
  -> VDomMachine dom m a w
  -> Maybe Namespace
  -> ElemName
  -> a
  -> [VDom a w]
  -> m (VDomStep dom m a w)
buildElem spec@VDomSpec {runDom, document = doc} build ns1 name1 as1 ch1 = do
  el <- runDom $ createElement ns1 name1 doc
  node <- runDom $ elementToNode el
  let onChild ix child = do
        res <- build child
        runDom $ insertChildIx ix (extract res) node
        pure res

  children <- for (zip [0 ..] ch1) (uncurry onChild)
  attrs <- spec.buildAttributes el as1
  let state = ElemState {runDom, build, node, attrs, ns = ns1, name = name1, children}
  pure $ Step node state patchElem haltElem

patchElem :: (MonadDOM dom, Monad m) => ElemState dom m a w -> VDom a w -> m (VDomStep dom m a w)
patchElem state@ElemState {runDom, build, node, attrs, ns = ns1, name = name1, children = ch1} vdom =
  case vdom of
    Grafted g ->
      patchElem state (runGraft g)
    Elem ns2 name2 as2 ch2 | (ns1, name1) == (ns2, name2) ->
      case (ch1, ch2) of
        ([], []) -> do
          attrs2 <- step attrs as2
          let nextState = ElemState {runDom, build, node, attrs = attrs2, ns = ns2, name = name2, children = ch1}
          pure $ Step node nextState patchElem haltElem
        _ -> do
          let onThese ix s v = do
                res <- step s v
                runDom $ insertChildIx ix (extract res) node
                pure $ Just res
              onThis _ s = halt s $> Nothing
              onThat ix v = do
                res <- build v
                runDom $ insertChildIx ix (extract res) node
                pure $ Just res
          children2 <- diffWithIxE ch1 ch2 onThese onThis onThat
          attrs2 <- step attrs as2
          let nextState = ElemState {runDom, build, node, attrs = attrs2, ns = ns2, name = name2, children = children2}
          pure $ Step node nextState patchElem haltElem
    _ -> do
      haltElem state
      build vdom

haltElem :: (MonadDOM dom, Monad m) => ElemState dom m a w -> m ()
haltElem ElemState {runDom, node, attrs, children} = do
  runDom $ traverse_ (removeChild node) =<< parentNode node
  for_ children halt
  halt attrs

----------------------------------------------------------------------

-- The runDom field is not used here; it is what pins @dom@. Every other
-- occurrence is under DomNode, which is a non-injective family, so without it
-- GHC cannot tell two WidgetStates apart.
data WidgetState dom m a w = WidgetState
  { runDom :: forall x. dom x -> m x
  , build :: VDomMachine dom m a w
  , widget :: Step m w (DomNode dom)
  }

buildWidget :: (Monad m) => VDomSpec dom m a w -> VDomMachine dom m a w -> w -> m (VDomStep dom m a w)
buildWidget spec@VDomSpec {runDom} build w = do
  res@(Step node _ _ _) <- spec.buildWidget spec w
  pure $ Step node (WidgetState {runDom, build, widget = res}) patchWidget haltWidget

patchWidget :: (Monad m) => WidgetState dom m a w -> VDom a w -> m (VDomStep dom m a w)
patchWidget state@WidgetState {runDom, build, widget} vdom =
  case vdom of
    Grafted g -> patchWidget state (runGraft g)
    Widget w -> do
      res@(Step n _ _ _) <- step widget w

      pure $ Step n (WidgetState {runDom, build, widget = res}) patchWidget haltWidget
    _ -> do
      haltWidget state
      build vdom

haltWidget :: WidgetState dom m a w -> m ()
haltWidget WidgetState {widget} = halt widget
