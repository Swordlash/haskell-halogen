{-# LANGUAGE TemplateHaskellQuotes #-}

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
import Data.CSS.Syntax.Tokens (Token (..), serialize, tokenize)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.TH.Template
import Language.Haskell.TH (Exp (..), Lit (..), Name, Q)
import Text.Read (readMaybe)

data Rule
  = Decl Text Text
  -- ^ A property and its value, as written.
  | Nested Text [Item Rule]
  -- ^ A selector list, as written, and the rules under it.
  | Media [Token] [Item Rule]
  | Keyframes Text [Item Rule]
  | FontFace [Item Rule]
  | Import Text

cssExp :: Env -> String -> Q Exp
cssExp env src = do
  (template, holes) <- orFail (extract (T.pack src))
  (items, rest) <- orFail (parseItems holes (tokenize template))
  case rest of
    [] -> pure ()
    _ -> failAt env 0 "a } closes nothing"
  bs <- orFail (blocks holes items)
  bodyExp env holes bs
  where
    orFail = either (uncurry (failAt env)) pure

-- | The rules up to the @}@ that closes them (left in the rest), or the end.
parseItems :: Holes -> [Token] -> Either Err ([Item Rule], [Token])
parseItems holes = go []
  where
    go acc ts = case dropWhile skippable ts of
      [] -> Right (reverse acc, [])
      rest@(RightCurlyBracket : _) -> Right (reverse acc, rest)
      Ident ph : rest
        | Just i <- isPlaceholder ph
        , Just h <- IntMap.lookup i holes ->
            case h.kind of
              StmtHole -> go (Ctl i : acc) rest
              ValueHole -> case dropWhile (== Whitespace) rest of
                LeftCurlyBracket : _ -> rule (Ident ph : rest)
                Colon : _ -> Left (h.line, "a property name cannot be Haskell")
                _ -> go (Val i : acc) rest
      rest -> rule rest
      where
        rule ts' = do
          (prelude, after) <- splitPrelude ts'
          case after of
            LeftCurlyBracket : body -> do
              (items, rest') <- parseItems holes body
              case rest' of
                RightCurlyBracket : rest'' -> do
                  r <- block prelude items
                  go (One r : acc) rest''
                _ -> Left (0, "{ after " <> T.strip (serialize prelude) <> " is never closed by }")
            _ -> do
              r <- statement prelude
              go (One r : acc) after

    skippable t = t == Whitespace || t == Semicolon

    -- The tokens before the next ; { or } outside brackets.
    splitPrelude :: [Token] -> Either Err ([Token], [Token])
    splitPrelude = walk (0 :: Int) []
      where
        walk _ acc [] = Right (reverse acc, [])
        walk 0 acc rest@(t : _) | t `elem` [Semicolon, LeftCurlyBracket, RightCurlyBracket] = Right (reverse acc, rest)
        walk d acc (t : rest) = walk (d + delta t) (t : acc) rest
        delta = \case
          LeftParen -> 1
          Function _ -> 1
          LeftSquareBracket -> 1
          RightParen -> -1
          RightSquareBracket -> -1
          _ -> 0

    block :: [Token] -> [Item Rule] -> Either Err Rule
    block prelude items = case dropWhile (== Whitespace) prelude of
      AtKeyword "media" : q -> Right (Media q items)
      AtKeyword "keyframes" : q -> case filter (/= Whitespace) q of
        [Ident name] -> Right (Keyframes name items)
        [String name] -> Right (Keyframes name items)
        _ -> Left (0, "@keyframes takes a name: " <> T.strip (serialize prelude))
      AtKeyword "font-face" : _ -> Right (FontFace items)
      AtKeyword other : _ -> Left (0, "Clay has no @" <> other <> " rule")
      _ -> Right (Nested (T.strip (serialize prelude)) items)

    statement :: [Token] -> Either Err Rule
    statement prelude = case dropWhile (== Whitespace) prelude of
      AtKeyword "import" : q -> case filter (/= Whitespace) q of
        [String url] -> Right (Import url)
        [Url url] -> Right (Import url)
        _ -> Left (0, "@import takes a quoted file: " <> T.strip (serialize prelude))
      AtKeyword other : _ -> Left (0, "Clay has no @" <> other <> " rule")
      Ident name : rest | Colon : value <- dropWhile (== Whitespace) rest -> Right (Decl name (T.strip (serialize value)))
      _ -> Left (0, "not a declaration: " <> T.strip (serialize prelude))

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
    stops <- mapM stop =<< orFail (blocks holes items)
    pure (AppE (AppE (VarE 'keyframes) (packE name)) (ListE (concat stops)))
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

    stop :: Block Rule -> Q [Exp]
    stop = \case
      Leaf (Nested sels items) -> do
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
  let ts = filter (/= Whitespace) q
      (fn, ts') = case ts of
        Ident "not" : rest -> ('queryNot, rest)
        Ident "only" : rest -> ('queryOnly, rest)
        rest -> ('query, rest)
      (ty, ts'') = case ts' of
        Ident t : rest | t /= "and" -> (t, rest)
        rest -> ("all", rest)
  features <- featuresOf ts''
  pure (fn, AppE (ConE 'MediaType) (valueE ty), features)
  where
    featuresOf = \case
      [] -> pure []
      Ident "and" : rest -> featuresOf rest
      LeftParen : Ident name : rest -> do
        let (inside, after) = break (== RightParen) rest
        value <- case inside of
          [] -> pure (ConE 'Nothing)
          Colon : v -> AppE (ConE 'Just) <$> featureValue (pieces (T.strip (serialize v)))
          _ -> failAt env 0 ("a media feature is (name) or (name: value): " <> serialize (LeftParen : Ident name : inside))
        more <- featuresOf (drop 1 after)
        pure (AppE (AppE (ConE 'Feature) (packE name)) value : more)
      other -> failAt env 0 ("not a media query: " <> serialize other)

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
  ps <- predicates t
  case ps of
    [] -> Left (0, "& needs a class, id, pseudo-class or attribute after it")
    _ -> Right (AppE (ConE 'Sel.Refinement) (ListE ps))
  where
    predicates s = case T.uncons s of
      Nothing -> Right []
      Just ('.', rest) -> simple 'Sel.Class rest
      Just ('#', rest) -> simple 'Sel.Id rest
      Just (':', rest) | Just (':', rest') <- T.uncons rest -> simple 'Sel.PseudoElem rest'
      Just (':', rest) -> simple 'Sel.Pseudo rest
      Just ('[', rest) ->
        let (inside, after) = T.break (== ']') rest
        in (AppE (ConE 'Sel.Attr) (packE inside) :) <$> predicates (T.drop 1 after)
      _ -> Left (0, "not a refinement of the selector: &" <> t)
    simple con rest =
      let (name, after) = nameAndRest rest
      in (AppE (ConE con) (packE name) :) <$> predicates after
    -- A name runs to the next . # : or [ outside parentheses.
    nameAndRest = walk (0 :: Int) []
      where
        walk _ acc "" = (T.pack (reverse acc), "")
        walk d acc s = case T.uncons s of
          Just (c, rest)
            | d == 0 && c `elem` (".#:[" :: String) -> (T.pack (reverse acc), s)
            | c == '(' -> walk (d + 1) (c : acc) rest
            | c == ')' -> walk (d - 1) (c : acc) rest
            | otherwise -> walk d (c : acc) rest
          Nothing -> (T.pack (reverse acc), "")

-- | A selector list split at its commas, outside parentheses.
splitCommas :: Text -> [Text]
splitCommas = map T.strip . walk (0 :: Int) [] []
  where
    walk _ acc parts "" = reverse (T.pack (reverse acc) : parts)
    walk d acc parts s = case T.uncons s of
      Just (',', rest) | d == 0 -> walk d [] (T.pack (reverse acc) : parts) rest
      Just (c, rest) -> walk (d + (if c == '(' || c == '[' then 1 else if c == ')' || c == ']' then -1 else 0)) (c : acc) parts rest
      Nothing -> reverse (T.pack (reverse acc) : parts)

keyE :: Text -> Exp
keyE name = AppE (ConE 'CP.Key) (AppE (ConE 'CP.Plain) (packE name))

valueE :: Text -> Exp
valueE t = AppE (ConE 'CP.Value) (AppE (ConE 'CP.Plain) (packE t))

packE :: Text -> Exp
packE t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))
