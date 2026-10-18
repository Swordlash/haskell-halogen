{-# LANGUAGE OverloadedRecordDot #-}

-- | The shape of a style template: css-syntax tokenizes it (with the
-- Haskell cut out, see "Halogen.TH.Template"), and the parsers here read
-- the tokens as rules, as SCSS would: a selector with rules nested in it,
-- a declaration, an at-rule with a block or a statement. The media query
-- and the refinement of a selector have parsers of their own.
module Halogen.TH.Css
  ( Rule (..)
  , parseRules
  , MediaQuery (..)
  , parseMedia
  , Predicate (..)
  , parseRefinement
  , splitCommas
  )
where

import Data.CSS.Syntax.Tokens (Token (..), serialize, tokenize)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen.TH.Template (Err, Hole (..), HoleKind (..), Holes, Item (..), isPlaceholder)
import Text.Megaparsec hiding (Token)

data Rule
  = Decl Text Text
  -- ^ A property and its value, as written.
  | Nested Text [Item Rule]
  -- ^ A selector list, as written, and the rules under it.
  | Media [Token] [Item Rule]
  | Keyframes Text [Item Rule]
  | FontFace [Item Rule]
  | Import Text

-- | A token of css-syntax as megaparsec's: ordered, for the sets in its
-- errors.
newtype Tok = Tok {tok :: Token}
  deriving (Eq, Show)

instance Ord Tok where
  compare = comparing (show . tok)

instance VisualStream [Tok] where
  showTokens _ = T.unpack . serialize . map (.tok) . NE.toList

-- | What a parse can say in its own words, and on which line of the
-- template (0: none).
data CssError = CssError Int Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CssError where
  showErrorComponent (CssError _ t) = T.unpack t

type Parser = Parsec CssError [Tok]

failing :: Int -> Text -> Parser a
failing ln = customFailure . CssError ln

-- | The first error of a parse, as the quoter reports it.
errOf :: ParseErrorBundle [Tok] CssError -> Err
errOf bundle = case NE.head (bundleErrors bundle) of
  FancyError _ es | [ErrorCustom (CssError ln t)] <- Set.toList es -> (ln, t)
  e -> (0, T.strip (T.pack (parseErrorTextPretty e)))

run :: Parser a -> [Token] -> Either Err a
run p = either (Left . errOf) Right . parse p "" . map Tok

symbol :: Token -> Parser Token
symbol t = (.tok) <$> satisfy ((== t) . (.tok)) <?> T.unpack (serialize [t])

anyIdent :: Parser Text
anyIdent = token (\(Tok t) -> case t of Ident i -> Just i; _ -> Nothing) Set.empty <?> "a name"

ident :: Text -> Parser Text
ident i = try (anyIdent >>= \i' -> if i' == i then pure i else fail "another name") <?> T.unpack i

ws :: Parser ()
ws = skipMany (symbol Whitespace)

-- | A group in brackets, with everything inside it, nested groups included.
group :: Parser [Token]
group = do
  o <- (.tok) <$> satisfy (opener . (.tok))
  inner <- concat <$> manyTill (group <|> (pure . (.tok) <$> satisfy (not . closer . (.tok)))) (lookAhead (satisfy (closer . (.tok))))
  c <- (.tok) <$> satisfy (closer . (.tok))
  pure (o : inner <> [c])
  where
    opener = \case
      LeftParen -> True
      Function _ -> True
      LeftSquareBracket -> True
      _ -> False
    closer = \case
      RightParen -> True
      RightSquareBracket -> True
      _ -> False

----------------------------------------------------------------------
-- Rules

-- | The rules of a template, with the holes among them.
parseRules :: Holes -> Text -> Either Err [Item Rule]
parseRules holes src = run (rules holes <* end) (tokenize src)
  where
    end = eof <|> (symbol RightCurlyBracket *> failing 0 "a } closes nothing")

-- | Rules up to a @}@ or the end; whitespace and stray semicolons between
-- them are nothing.
rules :: Holes -> Parser [Item Rule]
rules holes = skip *> many (item holes <* skip)
  where
    skip = skipMany (satisfy ((`elem` [Whitespace, Semicolon]) . (.tok)))

-- | A hole on its own (a statement, or a value: a whole rule from
-- Haskell), or a rule; a hole where a property's name would be is refused.
item :: Holes -> Parser (Item Rule)
item holes = do
  ahead <- optional (lookAhead hole)
  case ahead of
    Just (i, h)
      | h.kind == StmtHole -> Ctl i <$ hole
      | otherwise -> do
          next <- lookAhead (hole *> ws *> optional anySingle)
          case (.tok) <$> next of
            Just LeftCurlyBracket -> One <$> ruleOrDecl holes
            Just Colon -> failing h.line "a property name cannot be Haskell"
            _ -> Val i <$ hole
    Nothing -> One <$> ruleOrDecl holes
  where
    hole = token (\(Tok t) -> case t of Ident ph | Just i <- isPlaceholder ph, Just h <- IntMap.lookup i holes -> Just (i, h); _ -> Nothing) Set.empty

-- | Tokens up to the next @;@, @{@ or @}@ outside brackets, then a block
-- or the end of a statement.
ruleOrDecl :: Holes -> Parser Rule
ruleOrDecl holes = do
  prelude <- concat <$> some (group <|> (pure . (.tok) <$> satisfy (plain . (.tok))))
  opened <- optional (symbol LeftCurlyBracket)
  case opened of
    Just _ -> do
      body <- rules holes
      _ <- symbol RightCurlyBracket <|> failing 0 ("{ after " <> T.strip (serialize prelude) <> " is never closed by }")
      either (failing 0) pure (block prelude body)
    Nothing -> either (failing 0) pure (statement prelude)
  where
    plain = \case
      Semicolon -> False
      LeftCurlyBracket -> False
      RightCurlyBracket -> False
      RightParen -> False
      RightSquareBracket -> False
      LeftParen -> False
      Function _ -> False
      LeftSquareBracket -> False
      _ -> True

block :: [Token] -> [Item Rule] -> Either Text Rule
block prelude items = case dropWhile (== Whitespace) prelude of
  AtKeyword "media" : q -> Right (Media q items)
  AtKeyword "keyframes" : q -> case filter (/= Whitespace) q of
    [Ident name] -> Right (Keyframes name items)
    [String name] -> Right (Keyframes name items)
    _ -> Left ("@keyframes takes a name: " <> T.strip (serialize prelude))
  AtKeyword "font-face" : _ -> Right (FontFace items)
  AtKeyword other : _ -> Left ("Clay has no @" <> other <> " rule")
  _ -> Right (Nested (T.strip (serialize prelude)) items)

statement :: [Token] -> Either Text Rule
statement prelude = case dropWhile (== Whitespace) prelude of
  AtKeyword "import" : q -> case filter (/= Whitespace) q of
    [String url] -> Right (Import url)
    [Url url] -> Right (Import url)
    _ -> Left ("@import takes a quoted file: " <> T.strip (serialize prelude))
  AtKeyword other : _ -> Left ("Clay has no @" <> other <> " rule")
  Ident name : rest | Colon : value <- dropWhile (== Whitespace) rest -> Right (Decl name (T.strip (serialize value)))
  _ -> Left ("not a declaration: " <> T.strip (serialize prelude))

----------------------------------------------------------------------
-- Media queries

-- | @[not | only] [type] [and (feature[: value])]…@: the modifier, the
-- type (@all@ when none) and the features with their values as written.
data MediaQuery = MediaQuery
  { modifier :: Maybe Text
  , mediaType :: Text
  , features :: [(Text, Maybe Text)]
  }

parseMedia :: [Token] -> Either Err MediaQuery
parseMedia = run $ do
  ws
  modifier <- optional ((ident "not" <|> ident "only") <* ws)
  mediaType <- option "all" (try (anyIdent >>= \t -> if t == "and" then fail "and" else pure t) <* ws)
  features <- many (optional (ident "and" <* ws) *> feature <* ws)
  eof
  pure MediaQuery {modifier, mediaType, features}
  where
    feature = do
      _ <- symbol LeftParen <* ws
      name <- anyIdent <* ws
      value <- optional (symbol Colon *> (T.strip . serialize . concat <$> many (group <|> (pure . (.tok) <$> satisfy ((/= RightParen) . (.tok))))))
      _ <- symbol RightParen <|> failing 0 ("a media feature is (name) or (name: value), not (" <> name <> " …")
      pure (name, value)

----------------------------------------------------------------------
-- Selectors

-- | What follows @&@: classes, ids, pseudo-classes, pseudo-elements and
-- attribute selectors, in any number.
data Predicate
  = Class Text
  | Id Text
  | Pseudo Text
  | PseudoElem Text
  | Attr Text
  deriving (Eq, Show)

parseRefinement :: Text -> Either Text [Predicate]
parseRefinement t
  | T.null t = Left "& needs a class, id, pseudo-class or attribute after it"
  | otherwise = either (const (Left ("not a refinement of the selector: &" <> t))) Right (parse (some predicate <* eof) "" t)
  where
    predicate :: Parsec Void Text Predicate
    predicate =
      choice
        [ Class <$> (single '.' *> name)
        , Id <$> (single '#' *> name)
        , PseudoElem <$> (try (chunk "::") *> name)
        , Pseudo <$> (single ':' *> name)
        , Attr <$> (single '[' *> takeWhileP Nothing (/= ']') <* single ']')
        ]
    -- A name runs to the next . # : or [ outside parentheses.
    name = T.concat <$> some (parens <|> takeWhile1P Nothing (`notElem` (".#:[()" :: String)))
    parens = do
      inner <- single '(' *> manyTill (parens <|> (T.singleton <$> anySingle)) (single ')')
      pure ("(" <> T.concat inner <> ")")

-- | A selector list split at its commas, outside brackets.
splitCommas :: Text -> [Text]
splitCommas t = either (const [T.strip t]) id (parse (sepBy segment (single ',') <* eof) "" t)
  where
    segment :: Parsec Void Text Text
    segment = T.strip . T.concat <$> many (bracketed <|> takeWhile1P Nothing (`notElem` (",([" :: String)))
    bracketed = do
      o <- single '(' <|> single '['
      inner <- manyTill (bracketed <|> (T.singleton <$> anySingle)) (single (if o == '(' then ')' else ']'))
      pure (T.cons o (T.concat inner <> T.singleton (if o == '(' then ')' else ']')))
