{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | What the two quoters share: the Haskell inside a template, and the
-- shape of a template around it.
--
-- Before the HTML or the CSS is parsed, every piece of Haskell is cut out
-- and a placeholder left in its place, so the parsers see plain text.
-- Afterwards the placeholders in the parsed text are looked up again. The
-- pieces are of three kinds: @{{ expression }}@ is a value, @{% … %}@ is a
-- statement that opens or closes a block (@if@, @elif@, @else@, @for@,
-- @let@, @end@), and @{# … #}@ is a comment, dropped.
--
-- A sequence of things from a template (the children of an element, its
-- attributes, the rules of a style) with statements among them is grouped
-- into 'Block's, and 'blocksExp' turns the groups into one list expression:
-- the plain things are list literals, the values are spliced in through a
-- function the caller gives, and the statements become conditionals, list
-- comprehensions and lets.
module Halogen.TH.Template
  ( -- * The Haskell in a template
    Hole (..)
  , HoleKind (..)
  , Holes
  , extract
  , Piece (..)
  , pieces
  , isPlaceholder

    -- * Statements and blocks
  , Stmt (..)
  , parseStmt
  , Item (..)
  , Block (..)
  , blocks

    -- * Code generation
  , Err
  , Env (..)
  , failAt
  , blocksExp
  , textExp
  , holeLine
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Language.Haskell.TH (Exp (..), Lit (..), Pat, Q)
import Language.Haskell.TH qualified as TH
import Text.Megaparsec
import Text.Megaparsec.Char (space, space1)

-- | A piece of Haskell cut out of the template.
data Hole = Hole
  { kind :: HoleKind
  , line :: Int
  -- ^ The line of the template it starts on, from 1.
  , code :: Text
  }
  deriving (Eq, Show)

data HoleKind
  = ValueHole
  -- ^ @{{ … }}@
  | StmtHole
  -- ^ @{% … %}@
  deriving (Eq, Show)

type Holes = IntMap Hole

-- | The placeholder for the hole with an index: a run of characters no
-- HTML or CSS tokenizer breaks, from the Unicode private use area.
placeholder :: Int -> Text
placeholder i = T.singleton open <> T.pack (show i) <> T.singleton close

open, close :: Char
open = '\xE000'
close = '\xE001'

isPlaceholder :: Text -> Maybe Int
isPlaceholder t = case T.uncons t of
  Just (c, rest) | c == open, Just (digits, c') <- T.unsnoc rest, c' == close, not (T.null digits), T.all (`elem` ['0' .. '9']) digits -> Just (read (T.unpack digits))
  _ -> Nothing

-- | What went wrong, on which line of the template (0: no line).
type Err = (Int, Text)

-- | Cut the Haskell out of a template: the template with placeholders, and
-- the holes by index.
extract :: Text -> Either Err (Text, Holes)
extract src = case parse (many part <* eof) "" src of
  Left bundle -> Left (bundleErr bundle)
  Right parts -> Right (assemble parts)
  where
    bundleErr bundle = case NE.head (bundleErrors bundle) of
      FancyError o es
        | [ErrorCustom (Unclosed opener closer)] <- Set.toList es -> (lineAt o, opener <> " is never closed by " <> closer)
      e -> (lineAt (errorOffset e), T.pack (parseErrorTextPretty e))
    lineAt o = 1 + T.count "\n" (T.take o src)

    assemble = go 0 [] IntMap.empty
      where
        go _ acc holes [] = (T.concat (reverse acc), holes)
        go n acc holes (Plain t : rest) = go n (t : acc) holes rest
        go n acc holes (Comment : rest) = go n acc holes rest
        go n acc holes (Code kind ln code : rest) = go (n + 1) (placeholder n : acc) (IntMap.insert n (Hole kind ln code) holes) rest

-- | A part of a template: plain text, a hole with its kind and line, or a
-- comment.
data Part
  = Plain Text
  | Code HoleKind Int Text
  | Comment

-- | What a scan of the template can say that megaparsec's own words do not.
data TemplateError = Unclosed Text Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent TemplateError where
  showErrorComponent (Unclosed opener closer) = T.unpack (opener <> " is never closed by " <> closer)

type Scanner = Parsec TemplateError Text

part :: Scanner Part
part =
  choice
    [ holePart ValueHole "{{" "}}" valueBody
    , holePart StmtHole "{%" "%}" codeBody
    , Comment <$ (chunk "{#" *> plainBody "#}")
    , Plain <$> takeWhile1P Nothing (/= '{')
    , Plain . T.singleton <$> single '{'
    ]

-- | A hole from its opening to its closing delimiter; never closed, the
-- error is the hole's, at the line it opens on.
holePart :: HoleKind -> Text -> Text -> (Text -> Scanner Text) -> Scanner Part
holePart kind opener closer body = do
  o <- getOffset
  ln <- unPos . sourceLine <$> getSourcePos
  _ <- chunk opener
  code <- region (const (FancyError o (Set.singleton (ErrorCustom (Unclosed opener closer))))) (body closer)
  pure (Code kind ln (T.strip code))

-- | A value ends at the first @}}@ outside a string or character literal
-- with every brace opened inside it closed, so a record update can sit
-- inside.
valueBody :: Text -> Scanner Text
valueBody closer = T.concat <$> manyTill piece (chunk closer)
  where
    piece = choice [literal, braced, T.singleton <$> anySingleBut '}']
    braced = do
      inner <- single '{' *> manyTill piece (single '}')
      pure ("{" <> T.concat inner <> "}")

-- | A statement ends at the first @%}@ outside a literal.
codeBody :: Text -> Scanner Text
codeBody closer = T.concat <$> manyTill (literal <|> (T.singleton <$> anySingle)) (chunk closer)

plainBody :: Text -> Scanner Text
plainBody closer = T.concat <$> manyTill (T.singleton <$> anySingle) (chunk closer)

-- | A string literal, a character literal, or a name, each taken whole,
-- so that a brace or a quote inside a literal is not a delimiter. A quote
-- after a name is part of it (@x'@), and one before a name is a Template
-- Haskell name (@'foo@): a character literal is only a quote that holds
-- one character or an escape and is closed by another.
literal :: Scanner Text
literal = choice [stringLit, charLit, name]
  where
    stringLit = do
      body <- single '"' *> manyTill (escape <|> (T.singleton <$> anySingle)) (single '"')
      pure ("\"" <> T.concat body <> "\"")
    charLit = try $ do
      body <- single '\'' *> (escape <|> (T.singleton <$> anySingleBut '\'')) <* single '\''
      pure ("'" <> body <> "'")
    -- An escape: one character, or a numeric code (\x7d, \65).
    escape = do
      c <- single '\\' *> anySingle
      more <- if c == 'x' || c == 'o' || isDigit c then takeWhileP Nothing isAlphaNum else pure ""
      pure (T.cons '\\' (T.cons c more))
    name = takeWhile1P Nothing (\c -> isAlphaNum c || c == '_' || c == '\'')

-- | Text with placeholders, split around them.
data Piece
  = Lit Text
  | Ref Int
  deriving (Eq, Show)

pieces :: Text -> [Piece]
pieces t
  | T.null t = []
  | otherwise =
      let (before, rest) = T.break (== open) t
      in [Lit before | not (T.null before)] <> case T.uncons rest of
        Nothing -> []
        Just (_, rest') ->
          let (digits, rest'') = T.break (== close) rest'
          in Ref (read (T.unpack digits)) : pieces (T.drop 1 rest'')

-- | A statement: what a @{% … %}@ says.
data Stmt
  = If Text
  | Elif Text
  | Else
  | End
  | For Text Text
  -- ^ A pattern, and the list it ranges over.
  | Let Text Text
  -- ^ A pattern, and what it is bound to.
  deriving (Eq, Show)

-- | The Haskell of a statement is kept as written (only trimmed): a string
-- literal in it keeps its spaces.
parseStmt :: Text -> Either Text Stmt
parseStmt t = case parse (space *> stmt <* eof) "" t of
  Right s -> Right s
  Left _ -> Left ("not a statement: {% " <> t <> " %}; the statements are if, elif, else, for … in …, let … = …, end")
  where
    stmt :: Parsec Void Text Stmt
    stmt =
      choice
        [ Else <$ keyword "else"
        , End <$ choice (map keyword ["end", "endif", "endfor", "endlet"])
        , If <$> (keyword "if" *> rest)
        , Elif <$> (keyword "elif" *> rest)
        , For <$> (keyword "for" *> upTo (space1 *> chunk "in" *> space1)) <*> rest
        , Let <$> (keyword "let" *> upTo (single '=')) <*> rest
        ]
    -- A keyword is followed by space or the end, so "endif" is not "end".
    keyword w = try (chunk w <* (space1 <|> eof))
    -- Everything up to a delimiter, which a pattern cannot hold.
    upTo end = T.strip . T.pack <$> someTill anySingle (try end)
    rest = do
      r <- T.strip <$> takeRest
      if T.null r then fail "nothing after the keyword" else pure r

-- | One thing of a sequence in a template, with the holes among them.
data Item a
  = One a
  | Val Int
  | Ctl Int
  deriving (Eq, Show, Functor)

-- | A sequence grouped by its statements.
data Block a
  = Leaf a
  | Splice Int
  | Cond [(Int, [Block a])] (Maybe [Block a])
  -- ^ The @if@ and @elif@ branches, each by its hole, and the @else@.
  | Loop Int [Block a]
  | Bind Int [Block a]
  -- ^ A @let@, over the rest of the sequence.
  deriving (Eq, Show, Functor)

-- | Group a sequence by its statements. Every block opened must be closed
-- in the same sequence: a @{% for %}@ among the children of one element
-- ends there too.
blocks :: Holes -> [Item a] -> Either Err [Block a]
blocks holes items = do
  (bs, term) <- sequenceUntil items
  case term of
    Nothing -> Right bs
    Just (s, i, _) -> Left (at i, "{% " <> keyword s <> " %} closes nothing")
  where
    at = holeLine holes
    keyword = \case
      If _ -> "if"
      Elif _ -> "elif"
      Else -> "else"
      End -> "end"
      For _ _ -> "for"
      Let _ _ -> "let"

    stmtOf i = case IntMap.lookup i holes of
      Just h -> either (Left . (at i,)) Right (parseStmt h.code)
      Nothing -> Left (0, "no hole " <> T.pack (show i))

    -- The blocks up to a statement that closes or continues one (elif,
    -- else, end), which is handed back with what follows it.
    sequenceUntil :: [Item a] -> Either Err ([Block a], Maybe (Stmt, Int, [Item a]))
    sequenceUntil [] = Right ([], Nothing)
    sequenceUntil (One x : rest) = prepend (Leaf x) <$> sequenceUntil rest
    sequenceUntil (Val i : rest) = prepend (Splice i) <$> sequenceUntil rest
    sequenceUntil (Ctl i : rest) = do
      s <- stmtOf i
      case s of
        If _ -> do
          (b, after) <- conditional i rest
          prepend b <$> sequenceUntil after
        For _ _ -> do
          (body, term) <- sequenceUntil rest
          case term of
            Just (End, _, after) -> prepend (Loop i body) <$> sequenceUntil after
            Just (other, j, _) -> Left (at j, "{% " <> keyword other <> " %} inside the {% for %} of line " <> T.pack (show (at i)) <> ", which is not closed")
            Nothing -> Left (at i, "{% for %} is never closed by {% end %}")
        Let _ _ -> do
          (body, term) <- sequenceUntil rest
          Right ([Bind i body], term)
        _ -> Right ([], Just (s, i, rest))

    prepend b (bs, term) = (b : bs, term)

    -- The branches of an if, from its first body on, and what follows the end.
    conditional :: Int -> [Item a] -> Either Err (Block a, [Item a])
    conditional i rest = do
      (body, term) <- sequenceUntil rest
      case term of
        Just (Elif _, j, after) -> do
          (next, after') <- conditional j after
          case next of
            Cond branches els -> Right (Cond ((i, body) : branches) els, after')
            _ -> Left (0, "impossible: an elif grouped as something else")
        Just (Else, _, after) -> do
          (els, term') <- sequenceUntil after
          case term' of
            Just (End, _, after') -> Right (Cond [(i, body)] (Just els), after')
            Just (other, j, _) -> Left (at j, "{% " <> keyword other <> " %} after an {% else %}")
            Nothing -> Left (at i, "{% if %} with an else is never closed by {% end %}")
        Just (End, _, after) -> Right (Cond [(i, body)] Nothing, after)
        _ -> Left (at i, "{% if %} is never closed by {% end %}")

holeLine :: Holes -> Int -> Int
holeLine holes i = maybe 0 (.line) (IntMap.lookup i holes)

-- | How the Haskell in a hole becomes code, and how a quoter fails: the
-- parser of expressions and patterns, and the message of a failure, each
-- given the line of the template (0: none).
data Env = Env
  { expression :: Int -> Text -> Q Exp
  , pattern :: Int -> Text -> Q Pat
  , report :: Int -> Text -> String
  }

failAt :: Env -> Int -> Text -> Q a
failAt env ln msg = fail (env.report ln msg)

-- | The blocks as one expression of a list: the plain things through
-- @leaf@, each one element, the values through @splice@, each a list.
blocksExp :: Env -> Holes -> (a -> Q Exp) -> (Exp -> Exp) -> [Block a] -> Q Exp
blocksExp env holes leaf splice = fmap join . mapM segment . group
  where
    join = \case
      [] -> ListE []
      [e] -> e
      es -> AppE (VarE 'concat) (ListE es)

    -- Adjacent plain things make one list literal.
    group :: [Block a] -> [Either [a] (Block a)]
    group = foldr step []
      where
        step (Leaf x) (Left xs : rest) = Left (x : xs) : rest
        step (Leaf x) rest = Left [x] : rest
        step b rest = Right b : rest

    segment (Left leaves) = ListE <$> mapM leaf leaves
    segment (Right b) = case b of
      Leaf x -> ListE . pure <$> leaf x
      Splice i -> splice <$> value i
      Cond branches els -> foldr branch (maybe (pure (ListE [])) (blocksExp env holes leaf splice) els) branches
      Loop i body -> do
        (p, list) <- loop i
        inner <- blocksExp env holes leaf splice body
        pure (AppE (VarE 'concat) (CompE [TH.BindS p list, TH.NoBindS inner]))
      Bind i rest -> do
        (p, rhs) <- binding i
        inner <- blocksExp env holes leaf splice rest
        pure (LetE [TH.ValD p (TH.NormalB rhs) []] inner)

    branch (i, body) rest = CondE <$> condition i <*> blocksExp env holes leaf splice body <*> rest

    hole i = case IntMap.lookup i holes of
      Just h -> pure h
      Nothing -> failAt env 0 ("no hole " <> T.pack (show i))

    value i = do
      h <- hole i
      env.expression h.line h.code

    condition i = do
      h <- hole i
      case parseStmt h.code of
        Right (If c) -> env.expression h.line c
        Right (Elif c) -> env.expression h.line c
        _ -> failAt env (holeLine holes i) "impossible: a branch that is not an if"

    loop i = do
      h <- hole i
      case parseStmt h.code of
        Right (For p list) -> (,) <$> env.pattern h.line p <*> env.expression h.line list
        _ -> failAt env (holeLine holes i) "impossible: a loop that is not a for"

    binding i = do
      h <- hole i
      case parseStmt h.code of
        Right (Let p rhs) -> (,) <$> env.pattern h.line p <*> env.expression h.line rhs
        _ -> failAt env (holeLine holes i) "impossible: a binding that is not a let"

-- | Text with holes as an expression of 'Text': the holes must be 'Text'
-- too, and statements among them shape it (@class="a {% if b %}b{% end %}"@).
-- A literal alone is packed; nothing is an empty text.
textExp :: Env -> Holes -> [Piece] -> Q Exp
textExp env holes ps = case ps of
  [] -> pure (lit "")
  [Lit t] -> pure (lit t)
  _
    | any statement ps -> do
        bs <- either (uncurry (failAt env)) pure (blocks holes (map item ps))
        AppE (VarE 'mconcat) <$> blocksExp env holes (pure . lit . unLit) (\e -> ListE [e]) bs
    | otherwise -> do
        es <- mapM piece ps
        pure (AppE (VarE 'mconcat) (ListE es))
  where
    lit t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))
    piece (Lit t) = pure (lit t)
    piece (Ref i) = case IntMap.lookup i holes of
      Just h -> env.expression h.line h.code
      Nothing -> failAt env 0 ("no hole " <> T.pack (show i))
    statement = \case
      Ref i | Just h <- IntMap.lookup i holes -> h.kind == StmtHole
      _ -> False
    item = \case
      Lit t -> One (Lit t)
      Ref i -> if statement (Ref i) then Ctl i else Val i
    unLit = \case
      Lit t -> t
      Ref _ -> ""
