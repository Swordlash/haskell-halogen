-- | The CSS selectors the in-memory DOM ("Halogen.VDom.DOM.Monad.Native")
-- understands, for @querySelector@, @closest@ and a handler's declared
-- 'Halogen.VDom.DOM.HandlerOptions.Within': what tests and handlers write,
-- not the whole of CSS.
--
-- Understood:
--
-- * a type (@a@, @svg@), @*@, @#id@, @.class@;
-- * @[attr]@, and @[attr=value]@ with @=@, @~=@, @|=@, @^=@, @$=@ and @*=@,
--   the value an identifier or a quoted string, with an optional @i@ or @s@
--   before the @]@ (ASCII case-insensitive or sensitive);
-- * compound selectors of those (@a.ref[href^="/p/"]@);
-- * the descendant combinator (white space) and the child combinator (@>@);
-- * lists, separated by commas;
-- * @:not(…)@ of a list of compound selectors.
--
-- Everything else is refused with the reason, never matched as nothing: a
-- test that passes because its selector was not understood is the worst
-- outcome. That is any other pseudo-class (@:hover@, @:first-child@,
-- @:checked@), a pseudo-element, the sibling combinators (@+@, @~@), a
-- namespace (@svg|a@), and a selector inside @:not@ with a combinator. A
-- selector that is not CSS at all is refused too, as a browser refuses it
-- with a @SyntaxError@.
--
-- As in a browser for an HTML document, a type and an attribute's name are
-- compared without regard to case on an HTML element and as written on any
-- other (SVG's @foreignObject@); an attribute's value, an id and a class as
-- written, unless the selector says @i@.
module Halogen.VDom.DOM.Monad.Native.Selector
  ( Selector
  , Refusal (..)
  , parseSelector
  , Subject (..)
  , matches
  )
where

import Data.Text qualified as T
import HPrelude hiding (Complex, Prefix, Selector)
import Numeric (readHex)
import Prelude (String)

-- | A list of complex selectors: an element matches when one of them does.
newtype Selector = Selector [Complex]
  deriving stock (Eq, Show)

-- | The rightmost compound, and the steps to its left, nearest first.
data Complex = Complex Compound [(Combinator, Compound)]
  deriving stock (Eq, Show)

data Combinator = Descendant | Child
  deriving stock (Eq, Show)

-- | A type (or 'Nothing' for any element) and the simple selectors with it.
data Compound = Compound (Maybe Text) [Simple]
  deriving stock (Eq, Show)

data Simple
  = Id Text
  | Class Text
  | -- | The attribute's name, and what its value is compared with, if
    -- anything: how, the value, and whether without regard to case.
    Attribute Text (Maybe (Operator, Text, Bool))
  | Not [Compound]
  deriving stock (Eq, Show)

data Operator = Equals | Includes | DashMatch | Prefix | Suffix | Substring
  deriving stock (Eq, Show)

-- | Why a selector was refused: one this engine does not understand,
-- though CSS has it, or one that is not CSS.
data Refusal
  = Unsupported Text
  | Malformed Text
  deriving stock (Eq, Show)

-- | How the engine sees an element: its tag, whether it is an HTML element
-- (whose tag and attribute names are compared without regard to case), an
-- attribute by name, and its parent element ('Nothing' at the top, or under
-- a document).
data Subject m e = Subject
  { tag :: e -> Text
  , html :: e -> Bool
  , attribute :: e -> Text -> m (Maybe Text)
  , parent :: e -> m (Maybe e)
  }

-- | Whether the element matches the selector.
matches :: forall m e. (Monad m) => Subject m e -> Selector -> e -> m Bool
matches subject (Selector complexes) element = anyM (`complex` element) complexes
  where
    complex :: Complex -> e -> m Bool
    complex (Complex rightmost steps) e = andM [compound rightmost e, leftward steps e]

    leftward :: [(Combinator, Compound)] -> e -> m Bool
    leftward [] _ = pure True
    leftward ((Child, c) : rest) e =
      subject.parent e >>= \case
        Nothing -> pure False
        Just p -> andM [compound c p, leftward rest p]
    leftward ((Descendant, c) : rest) e = ancestors e
      where
        ancestors x =
          subject.parent x >>= \case
            Nothing -> pure False
            Just p -> do
              here <- andM [compound c p, leftward rest p]
              if here then pure True else ancestors p

    compound :: Compound -> e -> m Bool
    compound (Compound wanted simples) e =
      andM ((pure (maybe True (typeMatches e) wanted)) : map (`simple` e) simples)

    typeMatches e wanted
      | subject.html e = T.toLower wanted == T.toLower (subject.tag e)
      | otherwise = wanted == subject.tag e

    attributeOf e name = subject.attribute e (if subject.html e then T.toLower name else name)

    simple :: Simple -> e -> m Bool
    simple s e = case s of
      Id wanted -> (== Just wanted) <$> attributeOf e "id"
      Class wanted -> maybe False (elem wanted . asciiWords) <$> attributeOf e "class"
      Attribute name test -> maybe False (\v -> maybe True (compareValue v) test) <$> attributeOf e name
      Not inner -> not <$> anyM (`compound` e) inner

    compareValue actual (operator, wanted, insensitive) =
      let fold' = if insensitive then T.map asciiLower else identity
          a = fold' actual
          w = fold' wanted
       in case operator of
            Equals -> a == w
            Includes -> not (T.null w) && not (T.any isAsciiSpace w) && w `elem` asciiWords a
            DashMatch -> a == w || (w <> "-") `T.isPrefixOf` a
            Prefix -> not (T.null w) && w `T.isPrefixOf` a
            Suffix -> not (T.null w) && w `T.isSuffixOf` a
            Substring -> not (T.null w) && w `T.isInfixOf` a

    anyM :: (a -> m Bool) -> [a] -> m Bool
    anyM _ [] = pure False
    anyM f (x : xs) = f x >>= \hit -> if hit then pure True else anyM f xs

    andM :: [m Bool] -> m Bool
    andM [] = pure True
    andM (x : xs) = x >>= \ok -> if ok then andM xs else pure False

asciiWords :: Text -> [Text]
asciiWords = filter (not . T.null) . T.split isAsciiSpace

isAsciiSpace :: Char -> Bool
isAsciiSpace c = c `elem` [' ', '\t', '\n', '\r', '\f']

asciiLower :: Char -> Char
asciiLower c
  | c >= 'A' && c <= 'Z' = chr (ord c + 32)
  | otherwise = c

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | The selector, or why it is refused.
parseSelector :: Text -> Either Refusal Selector
parseSelector source = do
  (complexes, rest) <- selectorList (T.unpack source)
  case rest of
    [] -> pure (Selector complexes)
    c : _ -> Left (Malformed ("unexpected " <> quoteChar c))

type Parser a = String -> Either Refusal (a, String)

malformed :: Text -> Either Refusal a
malformed = Left . Malformed

unsupported :: Text -> Either Refusal a
unsupported = Left . Unsupported

quoteChar :: Char -> Text
quoteChar c = "\"" <> T.singleton c <> "\""

-- A comma-separated list of complex selectors, up to the end or a @)@.
selectorList :: Parser [Complex]
selectorList input = do
  (leading, rest) <- complexSelector (skipSpace input)
  case skipSpace rest of
    ',' : more -> do
      (others, rest') <- selectorList more
      pure (leading : others, rest')
    rest' -> pure ([leading], rest')

complexSelector :: Parser Complex
complexSelector input = do
  (leading, rest) <- compoundSelector input
  go (Complex leading []) rest
  where
    go acc@(Complex rightmost steps) rest =
      let afterSpace = skipSpace rest
          spaced = afterSpace /= rest
       in case afterSpace of
            '>' : more -> next Child more
            '+' : _ -> unsupported "the next-sibling combinator (+)"
            '~' : _ -> unsupported "the subsequent-sibling combinator (~)"
            '|' : '|' : _ -> unsupported "the column combinator (||)"
            c : _ | spaced && c /= ',' && c /= ')' -> next Descendant afterSpace
            _ -> pure (acc, afterSpace)
      where
        next combinator more = do
          (c, more') <- compoundSelector (skipSpace more)
          go (Complex c ((combinator, rightmost) : steps)) more'

-- A compound selector: a type or @*@, then simple selectors, at least one
-- of the two.
compoundSelector :: Parser Compound
compoundSelector input = do
  (wanted, rest) <- typeSelector input
  (simples, rest') <- simpleSelectors rest
  case (wanted, simples) of
    (Nothing, []) -> case rest' of
      [] -> malformed "a selector is missing (at the end)"
      c : _ -> malformed ("a selector is missing before " <> quoteChar c)
    _ -> case rest' of
      '|' : _ -> unsupported "namespaces (|)"
      _ -> pure (Compound (join wanted) simples, rest')
  where
    -- @Just Nothing@ for @*@, which any element matches.
    typeSelector = \case
      '*' : rest -> pure (Just Nothing, rest)
      s | startsIdentifier s -> do
        (name, rest) <- identifier s
        pure (Just (Just name), rest)
      s -> pure (Nothing, s)

simpleSelectors :: Parser [Simple]
simpleSelectors input = case input of
  '#' : rest
    | startsIdentifier rest -> do
        (name, rest') <- identifier rest
        more (Id name) rest'
    | otherwise -> malformed "an id after # must be an identifier"
  '.' : rest
    | startsIdentifier rest -> do
        (name, rest') <- identifier rest
        more (Class name) rest'
    | otherwise -> malformed "a class after . must be an identifier"
  '[' : rest -> do
    (s, rest') <- attributeSelector (skipSpace rest)
    more s rest'
  ':' : ':' : _ -> unsupported "pseudo-elements (::)"
  ':' : rest -> do
    (name, rest') <- if startsIdentifier rest then identifier rest else malformed "a pseudo-class needs a name after :"
    case (T.toLower name, rest') of
      ("not", '(' : inner) -> do
        (compounds, after) <- compoundList (skipSpace inner)
        case skipSpace after of
          ')' : after' -> more (Not compounds) after'
          _ -> malformed ":not( is not closed"
      _ -> unsupported ("the pseudo-class :" <> name <> " (only :not is understood)")
  _ -> pure ([], input)
  where
    more s rest = do
      (others, rest') <- simpleSelectors rest
      pure (s : others, rest')

-- The compound selectors inside a @:not@, separated by commas.
compoundList :: Parser [Compound]
compoundList input = do
  (Complex c steps, rest) <- complexSelector input
  unless (null steps) $ unsupported "a combinator inside :not( ) (only compound selectors are understood there)"
  case skipSpace rest of
    ',' : more -> do
      (others, rest') <- compoundList (skipSpace more)
      pure (c : others, rest')
    rest' -> pure ([c], rest')

attributeSelector :: Parser Simple
attributeSelector input = do
  (name, rest) <- if startsIdentifier input then identifier input else malformed "an attribute selector needs a name after ["
  case skipSpace rest of
    '|' : c : _ | c /= '=' -> unsupported "namespaces (|) in an attribute selector"
    ']' : rest' -> pure (Attribute name Nothing, rest')
    rest' -> do
      (operator, afterOperator) <- case rest' of
        '=' : r -> pure (Equals, r)
        '~' : '=' : r -> pure (Includes, r)
        '|' : '=' : r -> pure (DashMatch, r)
        '^' : '=' : r -> pure (Prefix, r)
        '$' : '=' : r -> pure (Suffix, r)
        '*' : '=' : r -> pure (Substring, r)
        _ -> malformed ("the attribute selector [" <> name <> " is not closed, or its operator is not one of = ~= |= ^= $= *=")
      (value, afterValue) <- case skipSpace afterOperator of
        q : r | q == '"' || q == '\'' -> quoted q r
        r | startsIdentifier r -> identifier r
        _ -> malformed ("the value of [" <> name <> "] must be an identifier or a quoted string")
      (insensitive, afterFlag) <- case skipSpace afterValue of
        f : r | f `elem` ['i', 'I'] && endsWord r -> pure (True, r)
        f : r | f `elem` ['s', 'S'] && endsWord r -> pure (False, r)
        r -> pure (False, r)
      case skipSpace afterFlag of
        ']' : r -> pure (Attribute name (Just (operator, value, insensitive)), r)
        _ -> malformed ("the attribute selector [" <> name <> " is not closed")
  where
    endsWord r = case skipSpace r of
      ']' : _ -> True
      _ -> False

-- A quoted string, after its opening quote, with CSS's escapes.
quoted :: Char -> Parser Text
quoted q = go []
  where
    go acc = \case
      [] -> malformed "a quoted string is not closed"
      c : rest
        | c == q -> pure (T.pack (reverse acc), rest)
        | c == '\n' -> malformed "a quoted string holds a line break"
      '\\' : '\n' : rest -> go acc rest
      '\\' : rest -> do
        (c, rest') <- escape rest
        go (c : acc) rest'
      c : rest -> go (c : acc) rest

-- Whether an identifier starts here: a letter, @_@, a character beyond
-- ASCII or an escape, after an optional @-@ (or @--@).
startsIdentifier :: String -> Bool
startsIdentifier = \case
  '-' : '-' : _ -> True
  '-' : rest -> startsName rest
  s -> startsName s
  where
    startsName = \case
      '\\' : c : _ -> c /= '\n'
      c : _ -> isNameStart c
      [] -> False

isNameStart :: Char -> Bool
isNameStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || ord c >= 0x80

isNameChar :: Char -> Bool
isNameChar c = isNameStart c || (isAlphaNum c && ord c < 0x80) || c == '-'

-- An identifier, with CSS's escapes; 'startsIdentifier' has said one starts.
identifier :: Parser Text
identifier = go []
  where
    go acc = \case
      '\\' : rest@(c : _) | c /= '\n' -> do
        (e, rest') <- escape rest
        go (e : acc) rest'
      c : rest | isNameChar c -> go (c : acc) rest
      rest -> pure (T.pack (reverse acc), rest)

-- What a backslash escapes: up to six hexadecimal digits, and one white
-- space after them, or the character itself.
escape :: Parser Char
escape = \case
  [] -> pure ('\xFFFD', [])
  s@(c : rest)
    | isHexDigit c ->
        let (digits, after) = splitAt 6 s
            hex = takeWhile isHexDigit digits
            remaining = drop (length hex) digits <> after
            code = case readHex hex of
              [(n, "")] -> n
              _ -> 0xFFFD
            char = if code == 0 || code > 0x10FFFF || (code >= 0xD800 && code <= 0xDFFF) then '\xFFFD' else chr code
         in pure
              ( char
              , case remaining of
                  '\r' : '\n' : r -> r
                  w : r | isAsciiSpace w -> r
                  r -> r
              )
    | otherwise -> pure (c, rest)

skipSpace :: String -> String
skipSpace = dropWhile isAsciiSpace
