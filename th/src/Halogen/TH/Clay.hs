{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | CSS with Haskell in it, as Clay's 'Css'.
--
-- The template is tokenized by css-syntax with the Haskell cut out, and
-- read as blocks: a selector with rules nested in it (as in SCSS), a
-- declaration, @\@media@, @\@keyframes@, @\@font-face@ or @\@import@. Each
-- becomes Clay's own rule: a declaration is 'key' over the property and
-- the value as text (a vendor prefix is written, not added), a selector is
-- 'element' over its text, nested with '?', so the stylesheet rendered is
-- the one written. A selector starting with @&@ refines the enclosing one
-- (@&:hover@, @&.open@), one starting with @>@ is a child.
--
-- Haskell may stand for a whole value (anything with a 'Val' instance), a
-- part of one (as a 'Val' too), a whole selector, or a rule among rules
-- (a 'Css', such as a mixin).
module Halogen.TH.Clay
  ( cssExp
  )
where

import Clay.Property qualified as CP
import Clay.Selector qualified as Sel
import Clay.Stylesheet (Feature (..), MediaType (..), fontFace, importUrl, key, keyframes, query, queryNot, queryOnly, (&), (<?), (?))
import Data.CSS.Syntax.Tokens (Token)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.TH.Css
import Halogen.TH.Template
import Language.Haskell.TH (Exp (..), Lit (..), Name, Q)
import Text.Read (readMaybe)

cssExp :: Env -> String -> Q Exp
cssExp env src = do
  (template, holes) <- orFail (extract (T.pack src))
  items <- orFail (parseRules holes template)
  bs <- orFail (blocks holes items)
  bodyExp env holes bs
  where
    orFail = either (uncurry (failAt env)) pure

-- | Rules as one 'Css'.
bodyExp :: Env -> Holes -> [Block Rule] -> Q Exp
bodyExp env holes bs = do
  e <- blocksExp env holes (ruleExp env holes) (\x -> ListE [x]) bs
  pure $ case e of
    ListE [x] -> x
    _ -> AppE (VarE 'sequence_) e

ruleExp :: Env -> Holes -> Rule -> Q Exp
ruleExp env holes = \case
  Decl name value -> AppE (AppE (VarE 'key) (keyE name)) <$> valueExp env holes (pieces value)
  Nested selectors items -> do
    body <- nested items
    rules <- selectorRules selectors body
    pure (one rules)
  Media q items -> do
    body <- nested items
    (fn, ty, features) <- mediaQuery env holes q
    pure (AppE (AppE (AppE (VarE fn) ty) (ListE features)) body)
  Keyframes name items -> do
    -- The stops through the statements among them, each stop a list of
    -- (percentage, declarations); Haskell among them is such a list.
    stops <- blocksExp env holes (fmap ListE . stop) (\x -> ListE [x]) =<< orFail (blocks holes items)
    pure (AppE (AppE (VarE 'keyframes) (packE name)) (AppE (VarE 'concat) stops))
  FontFace items -> AppE (VarE 'fontFace) <$> nested items
  Import url -> pure (AppE (VarE 'importUrl) (packE url))
  where
    orFail = either (uncurry (failAt env)) pure
    nested items = orFail (blocks holes items) >>= bodyExp env holes

    one = \case
      [e] -> e
      es -> AppE (VarE 'sequence_) (ListE es)

    -- A selector list: the plain selectors as one combined selector, a
    -- refinement of the enclosing selector or a child each on its own.
    selectorRules :: Text -> Exp -> Q [Exp]
    selectorRules selectors body = do
      parts <- mapM part (splitCommas selectors)
      let plain = [e | Left e <- parts]
          own = [e | Right e <- parts]
          combined = [AppE (AppE (VarE '(?)) (foldr1 (\a b -> AppE (AppE (VarE '(<>)) a) b) plain)) body | not (null plain)]
      pure (combined <> map ($ body) own)

    part :: Text -> Q (Either Exp (Exp -> Exp))
    part s = case T.uncons s of
      Just ('&', rest)
        | T.null rest -> failAt env 0 "& alone selects nothing new"
        | Just (' ', _) <- T.uncons rest -> Left <$> selectorE (T.strip rest)
        | Just ('>', rest') <- T.uncons (T.strip rest) -> child (T.strip rest')
        | otherwise -> do
            r <- orFail (refinementE rest)
            pure (Right (AppE (AppE (VarE '(&)) r)))
      Just ('>', rest) -> child (T.strip rest)
      _ -> Left <$> selectorE s
      where
        child rest = do
          e <- selectorE rest
          pure (Right (AppE (AppE (VarE '(<?)) e)))

    -- Text, with Text from Haskell in it, or a Selector from Haskell alone.
    selectorE :: Text -> Q Exp
    selectorE s = case pieces s of
      [Ref i] -> hole i
      ps -> AppE (VarE 'Sel.element) <$> textExp env holes ps

    stop :: Rule -> Q [Exp]
    stop = \case
      Nested sels items -> do
        body <- nested items
        mapM (\s -> do n <- orFail (stopNumber s); pure (TupE [Just (LitE (RationalL n)), Just body])) (splitCommas sels)
      _ -> failAt env 0 "@keyframes holds stops: from, to or a percentage, each with its declarations"

    stopNumber s = case s of
      "from" -> Right 0
      "to" -> Right 100
      _ | Just (n, '%') <- T.unsnoc s, Just d <- readMaybe (T.unpack n) -> Right (toRational (d :: Double))
      _ -> Left (0, "a keyframe is from, to or a percentage: " <> s)

    hole i = case IntMap.lookup i holes of
      Just h -> env.expression h.line h.code
      Nothing -> failAt env 0 ("no hole " <> T.pack (show i))

-- | A value as written: a 'Val' alone, text alone, or text with Haskell
-- in it joined as a 'Value'.
valueExp :: Env -> Holes -> [Piece] -> Q Exp
valueExp env holes = \case
  [Ref i] -> hole i
  ps -> do
    es <- mapM piece ps
    pure $ case es of
      [e] -> e
      _ -> AppE (VarE 'mconcat) (ListE es)
  where
    piece (Lit t) = pure (valueE t)
    piece (Ref i) = AppE (VarE 'CP.value) <$> hole i
    hole i = case IntMap.lookup i holes of
      Just h -> env.expression h.line h.code
      Nothing -> failAt env 0 ("no hole " <> T.pack (show i))

-- | The media query after @\@media@: @[not | only] [type] [and (feature[: value])]…@.
mediaQuery :: Env -> Holes -> [Token] -> Q (Name, Exp, [Exp])
mediaQuery env holes q = do
  m <- either (uncurry (failAt env)) pure (parseMedia q)
  let fn = case m.modifier of
        Just "not" -> 'queryNot
        Just "only" -> 'queryOnly
        _ -> 'query
  features <- mapM feature m.features
  pure (fn, AppE (ConE 'MediaType) (valueE m.mediaType), features)
  where
    feature (name, value) = AppE (AppE (ConE 'Feature) (packE name)) <$> maybe (pure (ConE 'Nothing)) (fmap (AppE (ConE 'Just)) . featureValue . pieces) value
    -- A 'Value', whatever the Haskell in it is.
    featureValue = \case
      [Ref i] -> AppE (VarE 'CP.value) <$> hole i
      ps -> valueExp env holes ps
    hole i = case IntMap.lookup i holes of
      Just h -> env.expression h.line h.code
      Nothing -> failAt env 0 ("no hole " <> T.pack (show i))

-- | What follows @&@ as a 'Refinement': classes, ids, pseudo-classes,
-- pseudo-elements and attribute selectors, in any number.
refinementE :: Text -> Either Err Exp
refinementE t = do
  ps <- either (Left . (0,)) Right (parseRefinement t)
  Right (AppE (ConE 'Sel.Refinement) (ListE (map predicate ps)))
  where
    predicate = \case
      Class n -> AppE (ConE 'Sel.Class) (packE n)
      Id n -> AppE (ConE 'Sel.Id) (packE n)
      Pseudo n -> AppE (ConE 'Sel.Pseudo) (packE n)
      PseudoElem n -> AppE (ConE 'Sel.PseudoElem) (packE n)
      Attr n -> AppE (ConE 'Sel.Attr) (packE n)

keyE :: Text -> Exp
keyE name = AppE (ConE 'CP.Key) (AppE (ConE 'CP.Plain) (packE name))

valueE :: Text -> Exp
valueE t = AppE (ConE 'CP.Value) (AppE (ConE 'CP.Plain) (packE t))

packE :: Text -> Exp
packE t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))
