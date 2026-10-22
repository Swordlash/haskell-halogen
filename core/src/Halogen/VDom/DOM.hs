module Halogen.VDom.DOM
  ( VDomSpec (..)
  , VDomMachine
  , VDomStep
  , buildVDom
  , hydrateVDom
  , buildText
  , buildElem
  , buildWidget
  , buildKeyed
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
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

{-# INLINEABLE hydrateVDom #-}

type VDomMachine m a w = Machine m (VDom a w) (DomNode m)

type VDomStep m a w = Step m (VDom a w) (DomNode m)

-- | How to render a 'VDom' tree.
data VDomSpec m a w = VDomSpec
  { buildWidget :: VDomSpec m a w -> Machine m w (DomNode m)
  , hydrateWidget :: VDomSpec m a w -> DomNode m -> Machine m w (DomNode m)
  -- ^ A widget built by adopting the node given, which is in the tree
  -- already, as 'hydrateVDom' adopts one: on the same terms, so it either
  -- keeps the node, or builds a node of its own and takes the one given out
  -- of the tree, or builds one and leaves the node given where it is, for
  -- what comes after the widget. A widget that is never adopted is
  -- @\spec _ -> spec.buildWidget spec@, the last of the three; one that
  -- renders a thunk is 'Halogen.VDom.Thunk.hydrateThunk'.
  , buildAttributes :: DomElement m -> Machine m a ()
  , document :: DomDocument m
  }

buildVDom :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w
buildVDom spec = build
  where
    build = \case
      Text txt -> buildText spec build txt
      Elem ns n props children -> buildElem spec build ns n props children
      Keyed ns n props children -> buildKeyed spec build ns n props children
      Widget w -> buildWidget spec build w
      Grafted g -> build (runGraft g)

-- | Build a tree by adopting the nodes already there, from this one on:
-- the HTML a page arrived with, rendered ahead of the program by
-- "Halogen.Static", rather than building nodes of its own. With no node it
-- is 'buildVDom'.
--
-- The machine it hands back is the one 'buildVDom' would have built over
-- the same nodes, so patching it is patching any other: hydrating only
-- builds differently, once.
--
-- A text node is kept, its text set if it is not the one rendered, and an
-- element is kept if it has the name and the namespace rendered; then its
-- children are hydrated in turn against its own, its attributes and
-- properties set and its listeners attached, which is what HTML cannot
-- carry. Attributes the element has and the component does not render are
-- left as they are: the reading methods of 'MonadDOM' do not list them.
--
-- Nothing a page holds is an error. What the page has and the component
-- does not render is taken out of the tree, and what the component renders
-- and the page does not have is built afresh, so that whatever the markup
-- the tree ends as the component's. That covers what the HTML parser makes
-- of a page differently from the DOM the component renders (see
-- 'Halogen.VDom.DOM.Monad.Native.renderToText'): an empty text node, which
-- parses to none, is built and the node after it left for what follows; a
-- @tbody@ the parser made up is taken out and the rows built inside the
-- table; and so on. Comments are removed wherever children are hydrated:
-- the page has one between two texts, for the parser to keep them apart,
-- and one left in place would shift every index a later patch inserts at.
--
-- Children are matched in order and nothing more: a node the page is
-- missing shifts the ones after it, each of which is then matched with the
-- node one place on, adopting it if it has the same name and replacing it
-- if not. That is what hydrating a page rendered from other state costs,
-- and why the page should be rendered from the state the program starts
-- in.
--
-- A node that is not adopted is either taken out of the tree, when the
-- node built stands in its place, or left where it is for the next node to
-- adopt, when the node built is one the page had no node for (an empty
-- text). Either way the node built is not in the tree yet: whoever gave
-- the node to adopt puts the one handed back where it was, as children are
-- put in their place here.
hydrateVDom :: (MonadDOM m) => VDomSpec m a w -> Maybe (DomNode m) -> VDomMachine m a w
hydrateVDom spec = \case
  Nothing -> build
  Just candidate -> hydrate candidate
  where
    build = buildVDom spec
    hydrate candidate = \case
      Text txt -> hydrateText spec build candidate txt
      Elem ns n props children -> hydrateElem spec build candidate ns n props children
      Keyed ns n props children -> hydrateKeyed spec build candidate ns n props children
      Widget w -> hydrateWidget spec build candidate w
      Grafted g -> hydrate candidate (runGraft g)

-- | Hydrate a node's children against the ones it has, in order: each
-- hydrator is given the node at its position (if any is left), and what it
-- hands back is put at that position. What is left after the last is taken
-- out, as is every comment on the way.
--
-- The next node to adopt is the one after what was just put in place, which
-- is the one after the node adopted or replaced, or the node itself when it
-- was left for this one. That is how the walk goes on without asking which
-- of the three happened, which would mean comparing nodes, and a backend's
-- handles to one node need not be equal (on WebAssembly each crossing makes
-- a new one).
hydrateChildren
  :: forall m x
   . (MonadDOM m)
  => DomNode m
  -> [Maybe (DomNode m) -> m (Step m x (DomNode m))]
  -> m [Step m x (DomNode m)]
hydrateChildren parent hydrators = do
  start <- skipComments =<< firstChild parent
  go 0 start hydrators
  where
    go :: Int -> Maybe (DomNode m) -> [Maybe (DomNode m) -> m (Step m x (DomNode m))] -> m [Step m x (DomNode m)]
    go _ cursor [] = removeFrom cursor $> []
    go ix cursor (hydrator : rest) = do
      res <- hydrator cursor
      let node = extract res
      insertChildIx ix node parent
      next <- skipComments =<< nextSibling node
      (res :) <$> go (ix + 1) next rest

    skipComments = \case
      Nothing -> pure Nothing
      Just node ->
        nodeKind node >>= \case
          CommentKind -> do
            next <- nextSibling node
            removeChild node parent
            skipComments next
          _ -> pure (Just node)

    removeFrom = \case
      Nothing -> pass
      Just node -> do
        next <- nextSibling node
        removeChild node parent
        removeFrom next

-- | The candidate as an element, if it is one with this name in this
-- namespace. HTML's is the XHTML one in a browser and none in a component's
-- HTML, so the two count as the same; and an HTML element's name is
-- compared without case, as a browser lowers the case of the name it is
-- created with, so that a component rendering @DIV@ builds a @div@.
adoptable :: (MonadDOM m) => DomNode m -> Maybe Namespace -> ElemName -> m (Maybe (DomElement m))
adoptable candidate ns (ElemName name) =
  nodeKind candidate >>= \case
    ElementKind ns' (ElemName name')
      | html ns' == html ns && same (html ns) name' name -> nodeToElement candidate
    _ -> pure Nothing
  where
    html = \case
      Just (Namespace uri) | uri /= "http://www.w3.org/1999/xhtml" -> Just uri
      _ -> Nothing
    same = \case
      Nothing -> (==) `on` T.toLower
      Just _ -> (==)

-- | Build in place of the candidate, which goes.
replacing :: (MonadDOM m) => DomNode m -> m (VDomStep m a w) -> m (VDomStep m a w)
replacing candidate fresh = do
  traverse_ (removeChild candidate) =<< parentNode candidate
  fresh

----------------------------------------------------------------------

data TextState m a w = TextState
  { build :: VDomMachine m a w
  , node :: DomNode m
  , value :: Text
  }

buildText :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> Text -> m (VDomStep m a w)
buildText VDomSpec {document = doc} build value = do
  node <- createTextNode value doc
  let state = TextState {build, node, value}
  pure $ Step node state patchText haltText

-- | A text node is adopted, its text set if it differs. An empty text has
-- no node in a parsed page, so it adopts only an empty one, and is built
-- before anything else, leaving that for what comes next.
hydrateText :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> DomNode m -> Text -> m (VDomStep m a w)
hydrateText spec build candidate value =
  nodeKind candidate >>= \case
    TextKind current
      | T.null value && not (T.null current) -> buildText spec build value
      | otherwise -> do
          when (current /= value) $ setTextContent value candidate
          let state = TextState {build, node = candidate, value}
          pure $ Step candidate state patchText haltText
    _
      | T.null value -> buildText spec build value
      | otherwise -> replacing candidate (buildText spec build value)

patchText :: (MonadDOM m) => TextState m a w -> VDom a w -> m (VDomStep m a w)
patchText state@TextState {build, node, value = value1} vdom =
  case vdom of
    Text value2
      | value1 == value2 ->
          pure $ Step node state patchText haltText
      | otherwise -> do
          let nextState = TextState {build, node, value = value2}
          setTextContent value2 node
          pure $ Step node nextState patchText haltText
    _ -> do
      haltText state
      build vdom

haltText :: (MonadDOM m) => TextState m a w -> m ()
haltText TextState {node} =
  traverse_ (removeChild node) =<< parentNode node

----------------------------------------------------------------------

data KeyedState m a w = KeyedState
  { build :: VDomMachine m a w
  , node :: DomNode m
  , attrs :: Step m a ()
  , ns :: Maybe Namespace
  , name :: ElemName
  , children :: Map Text (VDomStep m a w)
  , length :: Int
  }

buildKeyed :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> Maybe Namespace -> ElemName -> a -> [(Text, VDom a w)] -> m (VDomStep m a w)
buildKeyed spec@VDomSpec {document = doc} build ns1 name1 as1 ch1 = do
  el <- createElement ns1 name1 doc
  node <- elementToNode el
  let onChild _ ix (_, vdom) = do
        res <- build vdom
        insertChildIx ix (extract res) node
        pure res
  children <- strMapWithIxE ch1 fst onChild
  attrs <- spec.buildAttributes el as1
  let state =
        KeyedState
          { build
          , node
          , attrs
          , ns = ns1
          , name = name1
          , children
          , length = length ch1
          }
  pure $ Step node state patchKeyed haltKeyed

hydrateKeyed :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> DomNode m -> Maybe Namespace -> ElemName -> a -> [(Text, VDom a w)] -> m (VDomStep m a w)
hydrateKeyed spec build candidate ns1 name1 as1 ch1 =
  adoptable candidate ns1 name1 >>= \case
    Nothing -> replacing candidate (buildKeyed spec build ns1 name1 as1 ch1)
    Just el -> do
      -- One child per key, the last of any duplicates, as a build has it.
      let entries = map snd (effective fst ch1)
      steps <- hydrateChildren candidate [\c -> hydrateVDom spec c vdom | (_, vdom) <- entries]
      attrs <- spec.buildAttributes el as1
      let state =
            KeyedState
              { build
              , node = candidate
              , attrs
              , ns = ns1
              , name = name1
              , children = M.fromList (zip (map fst entries) steps)
              , length = length ch1
              }
      pure $ Step candidate state patchKeyed haltKeyed

patchKeyed :: (MonadDOM m) => KeyedState m a w -> VDom a w -> m (VDomStep m a w)
patchKeyed state@KeyedState {build, node, attrs, ns = ns1, name = name1, children = ch1, length = len1} vdom =
  case vdom of
    Grafted g ->
      patchKeyed state (runGraft g)
    Keyed ns2 name2 as2 ch2 | (ns1, name1) == (ns2, name2) ->
      case (len1, length ch2) of
        (0, 0) -> do
          attrs2 <- step attrs as2
          let nextState =
                KeyedState
                  { build
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
                insertChildIx ix' (extract res) node
                pure res
              onThis _ = halt
              onThat _ ix (_, v) = do
                res <- build v
                insertChildIx ix (extract res) node
                pure res
          children2 <- diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
          attrs2 <- step attrs as2
          let nextState =
                KeyedState
                  { build
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

haltKeyed :: (MonadDOM m) => KeyedState m a w -> m ()
haltKeyed (KeyedState {node, attrs, children}) = do
  parent <- parentNode node
  traverse_ (removeChild node) parent
  for_ children halt
  halt attrs

----------------------------------------------------------------------

data ElemState m a w = ElemState
  { build :: VDomMachine m a w
  , node :: DomNode m
  , attrs :: Step m a ()
  , ns :: Maybe Namespace
  , name :: ElemName
  , children :: [VDomStep m a w]
  }

buildElem
  :: (MonadDOM m)
  => VDomSpec m a w
  -> VDomMachine m a w
  -> Maybe Namespace
  -> ElemName
  -> a
  -> [VDom a w]
  -> m (VDomStep m a w)
buildElem spec@VDomSpec {document = doc} build ns1 name1 as1 ch1 = do
  el <- createElement ns1 name1 doc
  node <- elementToNode el
  let onChild ix child = do
        res <- build child
        insertChildIx ix (extract res) node
        pure res

  children <- for (zip [0 ..] ch1) (uncurry onChild)
  attrs <- spec.buildAttributes el as1
  let state = ElemState {build, node, attrs, ns = ns1, name = name1, children}
  pure $ Step node state patchElem haltElem

hydrateElem
  :: (MonadDOM m)
  => VDomSpec m a w
  -> VDomMachine m a w
  -> DomNode m
  -> Maybe Namespace
  -> ElemName
  -> a
  -> [VDom a w]
  -> m (VDomStep m a w)
hydrateElem spec build candidate ns1 name1 as1 ch1 =
  adoptable candidate ns1 name1 >>= \case
    Nothing -> replacing candidate (buildElem spec build ns1 name1 as1 ch1)
    Just el -> do
      children <- hydrateChildren candidate (map (\child c -> hydrateVDom spec c child) ch1)
      attrs <- spec.buildAttributes el as1
      let state = ElemState {build, node = candidate, attrs, ns = ns1, name = name1, children}
      pure $ Step candidate state patchElem haltElem

patchElem :: (MonadDOM m) => ElemState m a w -> VDom a w -> m (VDomStep m a w)
patchElem state@ElemState {build, node, attrs, ns = ns1, name = name1, children = ch1} vdom =
  case vdom of
    Grafted g ->
      patchElem state (runGraft g)
    Elem ns2 name2 as2 ch2 | (ns1, name1) == (ns2, name2) ->
      case (ch1, ch2) of
        ([], []) -> do
          attrs2 <- step attrs as2
          let nextState = ElemState {build, node, attrs = attrs2, ns = ns2, name = name2, children = ch1}
          pure $ Step node nextState patchElem haltElem
        _ -> do
          let onThese ix s v = do
                res <- step s v
                insertChildIx ix (extract res) node
                pure $ Just res
              onThis _ s = halt s $> Nothing
              onThat ix v = do
                res <- build v
                insertChildIx ix (extract res) node
                pure $ Just res
          children2 <- diffWithIxE ch1 ch2 onThese onThis onThat
          attrs2 <- step attrs as2
          let nextState = ElemState {build, node, attrs = attrs2, ns = ns2, name = name2, children = children2}
          pure $ Step node nextState patchElem haltElem
    _ -> do
      haltElem state
      build vdom

haltElem :: (MonadDOM m) => ElemState m a w -> m ()
haltElem ElemState {node, attrs, children} = do
  traverse_ (removeChild node) =<< parentNode node
  for_ children halt
  halt attrs

----------------------------------------------------------------------

data WidgetState m a w = WidgetState
  { build :: VDomMachine m a w
  , widget :: Step m w (DomNode m)
  }

buildWidget :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> w -> m (VDomStep m a w)
buildWidget spec build w = do
  res@(Step node _ _ _) <- spec.buildWidget spec w
  pure $ Step node (WidgetState {build, widget = res}) patchWidget haltWidget

hydrateWidget :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> DomNode m -> w -> m (VDomStep m a w)
hydrateWidget spec build candidate w = do
  res@(Step node _ _ _) <- spec.hydrateWidget spec candidate w
  pure $ Step node (WidgetState {build, widget = res}) patchWidget haltWidget

patchWidget :: (Monad m) => WidgetState m a w -> VDom a w -> m (VDomStep m a w)
patchWidget state@WidgetState {build, widget} vdom =
  case vdom of
    Grafted g -> patchWidget state (runGraft g)
    Widget w -> do
      res@(Step n _ _ _) <- step widget w

      pure $ Step n (WidgetState {build, widget = res}) patchWidget haltWidget
    _ -> do
      haltWidget state
      build vdom

haltWidget :: WidgetState m a w -> m ()
haltWidget WidgetState {widget} = halt widget
