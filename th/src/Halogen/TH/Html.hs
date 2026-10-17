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
module Halogen.TH.Html
  ( htmlExp
  )
where

import Data.Char (toLower)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.HTML.Core (AttrName (..), ClassName (..), ElemName (..))
import Halogen.HTML.Core qualified as HH
import Halogen.HTML.Elements qualified as HHE
import Halogen.HTML.Properties qualified as HP
import Halogen.TH.Html.Children (children, props)
import Halogen.TH.Html.Names
import Halogen.TH.Template
import Language.Haskell.TH (Exp (..), Lit (..), Q)
import Text.HTML.TagSoup (ParseOptions (..), Tag (..), parseOptions, parseTagsOptions)
import Text.Read (readMaybe)

-- | A node of the template.
data Node
  = Element Int Text [Item Attr] [Item Node]
  -- ^ Its line, name, attributes and children.
  | TextNode Text

-- | An attribute as written: its value may hold placeholders; an attribute
-- without a value has an empty one.
data Attr = Attr Text Text

-- | The quote as an expression: one element (@single@), or a list of nodes.
htmlExp :: Env -> Bool -> String -> Q Exp
htmlExp env single src = do
  (template, holes) <- orFail (extract (T.pack src))
  let tags = parseTagsOptions parseOptions {optTagPosition = True, optTagWarning = True} template
  items <- orFail (build holes tags)
  bs <- orFail (blocks holes items)
  if single
    then case bs of
      [Leaf n] -> nodeExp env holes n
      [] -> failAt env 0 "nothing in the quote"
      _ -> failAt env 0 "one element, please; for several, or for {% for %} or {% if %} at the top, hhs gives a list"
    else blocksExp env holes (nodeExp env holes) (AppE (VarE 'children)) bs
  where
    orFail = either (uncurry (failAt env)) pure

-- | The tag stream as a tree, strictly.
build :: Holes -> [Tag Text] -> Either Err [Item Node]
build holes = go 1 [] []
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
        | isVoid name ->
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
  ps -> zipWith (\p isLast -> case p of
                   Lit n -> One (Attr n (if isLast then value else ""))
                   Ref i -> item holes i)
                ps (map (== length ps) [1 ..])

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

nodeExp :: Env -> Holes -> Node -> Q Exp
nodeExp env holes = \case
  TextNode t -> pure (AppE (VarE 'HH.text) (packE t))
  Element ln name attrs kids -> do
    attrBlocks <- orFail (blocks holes attrs)
    kidBlocks <- orFail (blocks holes kids)
    propsE <- blocksExp env holes (attrExp env holes ln) (AppE (VarE 'props)) attrBlocks
    kidsE <- blocksExp env holes (nodeExp env holes) (AppE (VarE 'children)) kidBlocks
    case element name of
      Just (WithChildren n) -> pure (AppE (AppE (VarE n) propsE) kidsE)
      Just (NoChildren n)
        | null kids -> pure (AppE (VarE n) propsE)
        | otherwise -> failAt env ln ("<" <> name <> "> takes no children")
      Nothing -> pure (AppE (AppE (AppE (VarE 'HHE.element) (AppE (ConE 'ElemName) (packE name))) propsE) kidsE)
  where
    orFail = either (uncurry (failAt env)) pure

-- | One attribute as a property, handler or attribute.
attrExp :: Env -> Holes -> Int -> Attr -> Q Exp
attrExp env holes ln (Attr name value) = case event lower of
  Just handler -> case single of
    Just i -> AppE (VarE handler) <$> hole i
    Nothing -> bad (name <> " takes a handler in {{ }}")
  Nothing -> case property lower of
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
      _ -> bad (name <> " takes a number, or a Double in {{ }}")
    Just (TypedProp n) -> case single of
      Just i -> AppE (VarE n) <$> hole i
      Nothing -> attr
    Just StyleProp -> case single of
      Just i -> AppE (VarE 'HP.style) <$> hole i
      Nothing -> AppE (VarE 'HP.styleText) <$> text
    Just RefProp -> case single of
      Just i -> AppE (VarE 'HP.ref) <$> hole i
      Nothing -> bad "ref takes a RefLabel in {{ }}"
    Nothing -> attr
  where
    lower = T.map toLower name
    ps = pieces value
    single = case ps of
      [Ref i] -> Just i
      _ -> Nothing
    text = textExp env holes ps
    attr = AppE (AppE (VarE 'HP.attr) (AppE (ConE 'AttrName) (packE name))) <$> text
    bad = failAt env ln
    hole i = case IntMap.lookup i holes of
      Just h -> env.expression h.line h.code
      Nothing -> bad ("no hole " <> T.pack (show i))

packE :: Text -> Exp
packE t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))
