module Hascript.VDom.Build where

import Protolude hiding (state)
import Hascript.VDom.Machine
import Web.DOM.Types
import Web.DOM.Monad
import Hascript.VDom.Utils
import Hascript.VDom.VDom

type VDomMachine m a w = Machine m (VDOM a w) (Node m)
type VDomStep    m a w = Step    m (VDOM a w) (Node m)

data VDomSpec m a w = VDomSpec
  { buildComponent :: VDomSpec m a w -> Machine m w (Node m)
  , buildAttributes :: Element m -> Machine m a ()
  , document :: Document m
  }

buildVDom :: MonadDOM m => VDomSpec m a w -> VDomMachine m a w
buildVDom spec = build
  where
    build = \case
      Text txt -> buildText spec build txt
      Element ns n props children -> buildElem spec build ns n props children
      Component w -> buildComponent spec build w
      Grafted g -> build (runGraft g)

----------------------------------------------------------------------

data TextState m a w
  = TextState
  { build :: VDomMachine m a w
  , node  :: Node m
  , value :: Text
  }

buildText :: MonadDOM m => VDomSpec m a w -> VDomMachine m a w -> Text -> m (VDomStep m a w)
buildText spec build value = do
  node <- createTextNode value spec.document
  let state = TextState{..}
  pure $ Step node state patchText haltText

patchText :: MonadDOM m => TextState m a w -> VDOM a w -> m (VDomStep m a w)
patchText state vdom = do
  let TextState { build, node, value = value1 } = state
  case vdom of
    Text value2
      | value1 == value2 ->
          pure $ Step node state patchText haltText
      | otherwise -> do
          let nextState = TextState { build, node, value = value2 }
          setTextContent value2 node
          pure $ Step node nextState patchText haltText
    _ -> do
      haltText state
      build vdom

haltText :: MonadDOM m => TextState m a w -> m ()
haltText TextState{node} = do
  parent <- parentNode node
  removeChild node parent

----------------------------------------------------------------------

data ElemState m a w
  = ElemState
  { build :: VDomMachine m a w
  , node :: Node m
  , attrs :: Step m a ()
  , ns :: Maybe Namespace
  , name :: ElemName
  , children :: [VDomStep m a w]
  }

buildElem 
  :: MonadDOM m 
  => VDomSpec m a w
  -> VDomMachine m a w 
  -> Maybe Namespace 
  -> ElemName 
  -> a
  -> [VDOM a w] 
  -> m (VDomStep m a w)
buildElem spec build ns1 name1 as1 ch1 = do
  el <- createElement ns1 name1 spec.document

  let
    node = elementToNode el
    onChild ix child = do
      res <- build child
      insertChildIx ix (extract res) node
      pure res
  
  children <- for (zip [0..] ch1) (uncurry onChild)
  attrs <- spec.buildAttributes el as1
  let state = ElemState { build, node, attrs, ns = ns1, name = name1, children }
  pure $ Step node state patchElem haltElem

patchElem :: MonadDOM m => ElemState m a w -> VDOM a w -> m (VDomStep m a w)
patchElem state vdom = do
  let ElemState { build, node, attrs, ns = ns1, name = name1, children = ch1 } = state
  case vdom of
    Grafted g ->
      patchElem state (runGraft g)
    Element ns2 name2 as2 ch2 | (ns1, name1) == (ns2, name2) ->
      case (ch1, ch2) of
        ([], []) -> do
          attrs2 <- step attrs as2
          let nextState = ElemState { attrs = attrs2, ns = ns2, name = name2, children = ch1, .. }
          pure $ Step node nextState patchElem haltElem
        _ -> do
          let
            onThese ix s v = do
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
          let nextState = ElemState { attrs = attrs2, ns = ns2, name = name2, children = children2, .. }
          pure $ Step node nextState patchElem haltElem
    _ -> do
      haltElem state
      build vdom

haltElem :: MonadDOM m => ElemState m a w -> m ()
haltElem ElemState{node, attrs, children} = do
  parent <- parentNode node
  removeChild node parent
  for_ children halt
  halt attrs

----------------------------------------------------------------------

data ComponentState m a w 
  = ComponentState
  { build :: VDomMachine m a w
  , component :: Step m w (Node m)
  }

buildComponent :: Monad m => VDomSpec m a w -> VDomMachine m a w -> w -> m (VDomStep m a w)
buildComponent spec build w = do
  res@(Step node _ _ _) <- spec.buildComponent spec w
  pure $ Step node (ComponentState { build, component = res }) patchComponent haltComponent

patchComponent :: Monad m => ComponentState m a w -> VDOM a w -> m (VDomStep m a w)
patchComponent state vdom = do
  let ComponentState { build, component } = state
  case vdom of
    Grafted g -> patchComponent state (runGraft g)
    Component w -> do
      res@(Step n _ _ _) <- step component w
          
      pure $ Step n (ComponentState { build, component = res }) patchComponent haltComponent
    _ -> do
      haltComponent state
      build vdom

haltComponent :: ComponentState m a w -> m ()
haltComponent ComponentState{component} = halt component