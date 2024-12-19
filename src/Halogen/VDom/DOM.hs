module Halogen.VDom.DOM
  ( VDomSpec (..)
  , buildVDom
  , buildText
  , buildElem
  , buildWidget
  )
where

import Halogen.VDom.DOM.Monad
import Halogen.VDom.Machine
import Halogen.VDom.Types
import Halogen.VDom.Utils
import Protolude hiding (state)
import Web.DOM.Element
import Web.DOM.Internal.Types

type VDomMachine m a w = Machine m (VDom a w) Node

type VDomStep m a w = Step m (VDom a w) Node

data VDomSpec m a w = VDomSpec
  { buildWidget :: VDomSpec m a w -> Machine m w Node
  , buildAttributes :: Element -> Machine m a ()
  , document :: Document
  }

buildVDom :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w
buildVDom spec = build
  where
    build = \case
      Text txt -> buildText spec build txt
      Elem ns n props children -> buildElem spec build ns n props children
      Widget w -> buildWidget spec build w
      Grafted g -> build (runGraft g)

----------------------------------------------------------------------

data TextState m a w = TextState
  { build :: VDomMachine m a w
  , node :: Node
  , value :: Text
  }

buildText :: (MonadDOM m) => VDomSpec m a w -> VDomMachine m a w -> Text -> m (VDomStep m a w)
buildText spec build value = do
  node <- createTextNode value spec.document
  let state = TextState {..}
  pure $ Step node state patchText haltText

patchText :: (MonadDOM m) => TextState m a w -> VDom a w -> m (VDomStep m a w)
patchText state vdom = do
  let TextState {build, node, value = value1} = state
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

data ElemState m a w = ElemState
  { build :: VDomMachine m a w
  , node :: Node
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
buildElem spec build ns1 name1 as1 ch1 = do
  el <- createElement ns1 name1 spec.document
  let node = elementToNode el
      onChild ix child = do
        res <- build child
        insertChildIx ix (extract res) node
        pure res

  children <- for (zip [0 ..] ch1) (uncurry onChild)
  attrs <- spec.buildAttributes el as1
  let state = ElemState {build, node, attrs, ns = ns1, name = name1, children}
  pure $ Step node state patchElem haltElem

patchElem :: (MonadDOM m) => ElemState m a w -> VDom a w -> m (VDomStep m a w)
patchElem state vdom = do
  let ElemState {build, node, attrs, ns = ns1, name = name1, children = ch1} = state
  case vdom of
    Grafted g ->
      patchElem state (runGraft g)
    Elem ns2 name2 as2 ch2 | (ns1, name1) == (ns2, name2) ->
      case (ch1, ch2) of
        ([], []) -> do
          attrs2 <- step attrs as2
          let nextState = ElemState {attrs = attrs2, ns = ns2, name = name2, children = ch1, ..}
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
          let nextState = ElemState {attrs = attrs2, ns = ns2, name = name2, children = children2, ..}
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
  , widget :: Step m w Node
  }

buildWidget :: (Monad m) => VDomSpec m a w -> VDomMachine m a w -> w -> m (VDomStep m a w)
buildWidget spec build w = do
  res@(Step node _ _ _) <- spec.buildWidget spec w
  pure $ Step node (WidgetState {build, widget = res}) patchWidget haltWidget

patchWidget :: (Monad m) => WidgetState m a w -> VDom a w -> m (VDomStep m a w)
patchWidget state vdom = do
  let WidgetState {build, widget} = state
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
