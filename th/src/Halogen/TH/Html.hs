{-# LANGUAGE TemplateHaskellQuotes #-}

-- | HTML with Haskell in it, as a Halogen 'HTML' expression.
--
-- The template is tokenized by tagsoup with the Haskell cut out, and built
-- into a tree strictly: every tag closed, in order, or the quote fails with
-- the line. Each element becomes its function from "Halogen.HTML.Elements"
-- (an unknown name, 'HH.element' with it), each attribute its property
-- from "Halogen.HTML.Properties" or its handler from "Halogen.HTML.Events"
-- (an unknown name, 'HP.attr'), so the result is typed as if written by
-- hand.
--
-- Text between tags follows JSX: a text with a newline in it has every
-- line trimmed and the lines joined by one space, so indentation is not
-- content; a text on one line is kept as it is, spaces and all.
--
-- An @\<svg>@ element and everything in it are SVG: elements from
-- "Halogen.Svg.Elements", attributes from "Halogen.Svg.Attributes" (see
-- "Halogen.TH.Svg.Names"), names in their case; a @\<foreignObject>@ holds
-- HTML again. The @svg@ quoters start in SVG.
module Halogen.TH.Html
  ( Dialect (..)
  , htmlExp
  )
where

import Data.Char (toLower)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.HTML.Core (AttrName (..), ClassName (..), ElemName (..))
import Halogen.HTML.Core qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Svg.Attributes qualified as SA
import Halogen.TH.Html.Children (children, props, styled, unknownHtml, unknownSvg, viewBoxOf)
import Halogen.TH.Html.Names
import Halogen.TH.Svg.Names qualified as Svg
import Halogen.TH.Template
import Language.Haskell.TH (Exp (..), Lit (..), Q)
import Text.HTML.TagSoup (ParseOptions (..), Tag (..), parseOptions, parseTagsOptions)
import Text.Read (readMaybe)

-- | A node of the template.
data Node
  = -- | Its line, name, attributes and children.
    Element Int Text [Item Attr] [Item Node]
  | TextNode Text

-- | An attribute as written: its value may hold placeholders; an attribute
-- without a value has an empty one.
data Attr = Attr Text Text

-- | Which names a template's elements and attributes are read as.
data Dialect = Html | Svg
  deriving (Eq, Show)

-- | The quote as an expression: one element (@single@), or a list of nodes,
-- starting in a dialect.
htmlExp :: Dialect -> Env -> Bool -> String -> Q Exp
htmlExp dialect env single src = do
  (template, holes) <- orFail (extract env.startColumn (T.pack src))
  let tags = parseTagsOptions parseOptions {optTagPosition = True, optTagWarning = True} (selfClosing template)
  items <- orFail (build dialect holes tags)
  bs <- orFail (blocks holes items)
  if single
    then case bs of
      [Leaf n] -> nodeExp dialect env holes n
      [] -> failAt env 0 "nothing in the quote"
      _ -> failAt env 0 "one element, please; for several, or for {% for %} or {% if %} at the top, hhs (or svgs) gives a list"
    else blocksExp env holes (nodeExp dialect env holes) (AppE (VarE 'children)) bs
  where
    orFail = either (uncurry (failAt env)) pure

-- | A hole right before the @/>@ of a self-closing tag (@\<path d={{ p }}/>@)
-- would take the slash into its unquoted value: a space keeps them apart.
-- Only in a tag, outside quotes: in text or a quoted value, @/>@ is text.
selfClosing :: Text -> Text
selfClosing = T.pack . go Nothing . T.unpack
  where
    -- In a tag (Just), and in which quote if any.
    go :: Maybe (Maybe Char) -> String -> String
    go _ [] = []
    go Nothing (c : cs) = c : go (if c == '<' then Just Nothing else Nothing) cs
    go (Just Nothing) (c : '/' : '>' : cs) | c == placeholderClose = c : ' ' : '/' : '>' : go Nothing cs
    go (Just Nothing) (c : cs)
      | c == '>' = c : go Nothing cs
      | c == '"' || c == '\'' = c : go (Just (Just c)) cs
      | otherwise = c : go (Just Nothing) cs
    go (Just (Just q)) (c : cs) = c : go (Just (if c == q then Nothing else Just q)) cs
    -- The last character of a placeholder ("Halogen.TH.Template").
    placeholderClose = '\xE001'

-- | The tag stream as a tree, strictly. Only HTML has void elements, and
-- only outside an @\<svg>@.
build :: Dialect -> Holes -> [Tag Text] -> Either Err [Item Node]
build dialect holes = go 1 [] []
  where
    -- The line, the open elements (innermost first, each with its children
    -- so far reversed), the roots so far reversed.
    go :: Int -> [(Int, Text, [Item Attr], [Item Node])] -> [Item Node] -> [Tag Text] -> Either Err [Item Node]
    go _ [] roots [] = Right (reverse roots)
    go _ ((ln, name, _, _) : _) _ [] = Left (ln, "<" <> name <> "> is never closed")
    go ln stack roots (tag : rest) = case tag of
      TagPosition row _ -> go row stack roots rest
      TagComment _ -> go ln stack roots rest
      -- A statement right after a quoted value is no attribute to tagsoup.
      TagWarning w
        | T.any (== '\xE000') w || "\\57344" `T.isInfixOf` w -> go ln stack roots rest
        | otherwise -> Left (ln, w)
      TagText t -> continue (textItems holes t) rest
      TagOpen name attrs
        | isVoid name && dialectAt stack == Html ->
            -- A void element's end tag, if written, closes nothing.
            continue [One (Element ln name (attrItems holes attrs) [])] (skipClose name rest)
        | otherwise -> go ln ((ln, name, attrItems holes attrs, []) : stack) roots rest
      TagClose name -> case stack of
        (l, n, as, kids) : stack'
          | same n name -> let (stack'', roots') = add [One (Element l n as (reverse kids))] stack' roots in go ln stack'' roots' rest
          | otherwise -> Left (ln, "</" <> name <> "> closes <" <> n <> "> of line " <> T.pack (show l))
        [] -> Left (ln, "</" <> name <> "> closes nothing")
      where
        continue items rest' = let (stack', roots') = add items stack roots in go ln stack' roots' rest'

    -- The dialect inside the open elements.
    dialectAt stack = foldr (\(_, n, _, _) d -> inside d n) dialect stack

    add items stack roots = case stack of
      (l, n, as, kids) : stack' -> ((l, n, as, reverse items <> kids) : stack', roots)
      [] -> ([], reverse items <> roots)

    same a b = T.map toLower a == T.map toLower b

    skipClose name = \case
      TagPosition _ _ : TagClose n : rest | same n name -> rest
      TagClose n : rest | same n name -> rest
      rest -> rest

-- | The attributes, with the holes among them. A hole written against an
-- attribute (@{% if c %}open{% end %}@) comes glued to its name: the name
-- is split, and the value stays with its last part.
attrItems :: Holes -> [(Text, Text)] -> [Item Attr]
attrItems holes = concatMap $ \(name, value) -> case pieces name of
  [Lit n] -> [One (Attr n value)]
  ps ->
    zipWith
      ( \p isLast -> case p of
          Lit n -> One (Attr n (if isLast then value else ""))
          Ref i -> item holes i
      )
      ps
      (map (== length ps) [1 ..])

-- | A text node, with the holes in it, after the whitespace rule.
textItems :: Holes -> Text -> [Item Node]
textItems holes t = map piece (pieces trimmed)
  where
    trimmed
      | T.any (== '\n') t = T.intercalate " " (filter (not . T.null) (map T.strip (T.lines t)))
      | otherwise = t
    piece (Lit s) = One (TextNode s)
    piece (Ref i) = item holes i

item :: Holes -> Int -> Item a
item holes i = case (.kind) <$> IntMap.lookup i holes of
  Just StmtHole -> Ctl i
  _ -> Val i

-- | The dialect of an element's own name and attributes, inside a parent
-- in one: an @\<svg>@ starts SVG.
own :: Dialect -> Text -> Dialect
own d name
  | d == Html && T.map toLower name == "svg" = Svg
  | otherwise = d

-- | The dialect of an element's children: a @\<foreignObject>@ holds HTML.
inside :: Dialect -> Text -> Dialect
inside d name = case own d name of
  Svg | name == "foreignObject" -> Html
  d' -> d'

nodeExp :: Dialect -> Env -> Holes -> Node -> Q Exp
nodeExp parent env holes = \case
  TextNode t -> pure (AppE (VarE 'HH.text) (packE t))
  Element ln written attrs kids -> do
    let dialect = own parent written
        -- An <svg> in HTML, whatever its case (<SVG> is HTML too), is the
        -- SVG element, whose name SVG writes in lower case.
        name = if parent == Html && dialect == Svg then "svg" else written
    attrBlocks <- orFail (blocks holes attrs)
    kidBlocks <- orFail (blocks holes kids)
    let lookUp = case dialect of
          Html -> element name
          Svg -> Svg.element name
        plain = case dialect of
          Html -> 'unknownHtml
          Svg -> 'unknownSvg
    propsE <- blocksExp env holes (attrExp dialect name (isJust lookUp) env holes ln) (AppE (VarE 'props)) attrBlocks
    kidsE <- blocksExp env holes (nodeExp (inside parent name) env holes) (AppE (VarE 'children)) kidBlocks
    case lookUp of
      Just (WithChildren n) -> pure (AppE (AppE (VarE n) propsE) kidsE)
      Just (NoChildren n)
        | null kids -> pure (AppE (VarE n) propsE)
        | otherwise -> failAt env ln ("<" <> name <> "> takes no children")
      Nothing -> pure (AppE (AppE (AppE (VarE plain) (AppE (ConE 'ElemName) (packE name))) propsE) kidsE)
  where
    orFail = either (uncurry (failAt env)) pure

-- | One attribute as a property, handler or attribute. On an element
-- without a function of its own ('unknownHtml', 'unknownSvg') only the
-- class, the style, a ref and handlers are typed; the rest is text.
attrExp :: Dialect -> Text -> Bool -> Env -> Holes -> Int -> Attr -> Q Exp
attrExp dialect tag known env holes ln (Attr name value) = case event lower of
  Just handler -> case single of
    Just i -> AppE (VarE handler) <$> hole i
    Nothing -> bad (name <> " takes a handler in {{ }}")
  Nothing -> case typed =<< (case dialect of Html -> property lower; Svg -> Svg.property tag name) of
    Just (TextProp n) -> AppE (VarE n) <$> text
    Just ClassProp -> AppE (VarE 'HP.class_) . AppE (ConE 'ClassName) <$> text
    Just (BoolProp n) -> case (ps, single) of
      ([], _) -> pure (AppE (VarE n) (ConE 'True))
      (_, Just i) -> AppE (VarE n) <$> hole i
      ([Lit l], _)
        | l == name || l == "true" -> pure (AppE (VarE n) (ConE 'True))
        | l == "false" -> pure (AppE (VarE n) (ConE 'False))
      _ -> bad (name <> " is a boolean attribute: write it alone, or with a Bool in {{ }}")
    Just (IntProp n) -> case (ps, single) of
      (_, Just i) -> AppE (VarE n) <$> hole i
      ([Lit l], _) | Just k <- readMaybe (T.unpack l) -> pure (AppE (VarE n) (LitE (IntegerL k)))
      _ -> bad (name <> " takes a whole number, or an Int in {{ }}")
    Just (DoubleProp n) -> case (ps, single) of
      (_, Just i) -> AppE (VarE n) <$> hole i
      ([Lit l], _) | Just d <- readMaybe (T.unpack l) -> pure (AppE (VarE n) (LitE (RationalL (toRational (d :: Double)))))
      -- In SVG, a length with its unit (@100%@, @10em@) is written as it is.
      _ | dialect == Svg -> attr
      _ -> bad (name <> " takes a number, or a Double in {{ }}")
    Just (TypedProp n) -> case single of
      Just i -> AppE (VarE n) <$> hole i
      Nothing -> attr
    Just StyleProp -> case single of
      Just i -> AppE (VarE 'HP.styleText) . AppE (VarE 'styled) <$> hole i
      Nothing -> AppE (VarE 'HP.styleText) <$> text
    Just RefProp -> case single of
      Just i -> AppE (VarE 'HP.ref) <$> hole i
      Nothing -> bad "ref takes a RefLabel in {{ }}"
    Just SvgClassProp -> AppE (VarE 'SA.class_) . AppE (ConE 'ClassName) <$> text
    Just ViewBoxProp -> case single of
      Just i -> AppE (VarE 'viewBoxOf) <$> hole i
      Nothing -> attr
    Nothing -> attr
  where
    typed kind
      | known = Just kind
      | otherwise = case kind of
          ClassProp -> Just kind
          SvgClassProp -> Just kind
          StyleProp -> Just kind
          RefProp -> Just kind
          _ -> Nothing
    lower = T.map toLower name
    ps = pieces value
    single = case ps of
      [Ref i] -> Just i
      _ -> Nothing
    text = textExp env holes ps
    written = case dialect of
      Html -> name
      Svg -> Svg.attributeName name
    attr = AppE (AppE (VarE 'HP.attr) (AppE (ConE 'AttrName) (packE written))) <$> text
    bad = failAt env ln
    hole i = case IntMap.lookup i holes of
      Just h -> env.expression (position h 0) h.code
      Nothing -> bad ("no hole " <> T.pack (show i))

packE :: Text -> Exp
packE t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))
