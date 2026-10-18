-- | Properties over random trees and sheets: written out as HTML or CSS and
-- quoted, they come back as the code that builds them. The code the quoter
-- makes is read back into the tree, since an 'Exp' cannot be run in a test.
module Test.Property (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Halogen.TH.Clay (cssExp)
import Halogen.TH.Haskell (parseExp, parsePat)
import Halogen.TH.Html (htmlExp)
import Halogen.TH.Template (Env (..))
import Language.Haskell.TH (Exp (..), Lit (..), nameBase, runQ)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  prop "a tree written as HTML is quoted as the code of that tree" $ \(Tree nodes) -> ioProperty $ do
    e <- runQ (htmlExp env False (T.unpack (foldMap html nodes)))
    pure (readNodes e === Right (normal nodes))
  prop "rules written as CSS are quoted as the code of those rules" $ \(Sheet rules) -> ioProperty $ do
    e <- runQ (cssExp env (T.unpack (T.unwords (map css rules))))
    pure (readRules e === Right rules)

-- | The quoters' environment, without a module: no holes are in play.
env :: Env
env =
  Env
    { expression = \_ t -> either (fail . show) pure (parseExp [] t)
    , pattern = \_ t -> either (fail . show) pure (parsePat [] t)
    , report = \_ m -> T.unpack m
    , startColumn = 1
    }

----------------------------------------------------------------------
-- HTML

-- | An element with attributes and children, or a text.
data Node
  = El Text [(Text, Text)] [Node]
  | Tx Text
  deriving (Eq, Show)

newtype Tree = Tree [Node]
  deriving (Show)

-- | Elements whose function in "Halogen.HTML" is named as the tag.
tags :: [Text]
tags = ["div", "span", "p", "ul", "li", "section", "em", "strong"]

instance Arbitrary Tree where
  arbitrary = Tree <$> resize 4 (listOf (node (3 :: Int)))
    where
      node depth = frequency [(2, Tx <$> phrase), (if depth > 0 then 3 else 0, element depth)]
      element depth = El <$> elements tags <*> attrs <*> resize 3 (listOf (node (depth - 1)))
      attrs = sublistOf ["class", "id", "title", "data-k"] >>= mapM (\k -> (,) k <$> (if k == "title" then phrase else word))
      word = T.pack <$> ((:) <$> elements ['a' .. 'z'] <*> resize 6 (listOf (elements (['a' .. 'z'] <> ['0' .. '9']))))
      phrase = T.unwords <$> resize 3 (listOf1 word)
  shrink (Tree nodes) = Tree <$> shrinkList shrinkNode nodes
    where
      shrinkNode = \case
        Tx _ -> []
        El t as kids -> kids <> [El t as kids' | kids' <- shrinkList shrinkNode kids] <> [El t as' kids | as' <- shrinkList (const []) as]

-- | Adjacent texts are one text node when read.
normal :: [Node] -> [Node]
normal = \case
  Tx a : Tx b : rest -> normal (Tx (a <> b) : rest)
  Tx a : rest -> Tx a : normal rest
  El t as kids : rest -> El t as (normal kids) : normal rest
  [] -> []

html :: Node -> Text
html = \case
  Tx t -> t
  El t as kids -> "<" <> t <> mconcat [" " <> k <> "=\"" <> v <> "\"" | (k, v) <- as] <> ">" <> foldMap html kids <> "</" <> t <> ">"

-- | The quoter's code for a list of nodes, as the nodes.
readNodes :: Exp -> Either String [Node]
readNodes = \case
  ListE es -> mapM readNode es
  AppE (VarE c) (ListE es) | nameBase c == "concat" -> concat <$> mapM readNodes es
  e -> Left ("not a list of nodes: " <> show e)
  where
    readNode = \case
      AppE (VarE t) s | nameBase t == "text" -> Tx <$> packed s
      AppE (AppE (VarE el) ps) kids | T.pack (nameBase el) `elem` tags -> El (T.pack (nameBase el)) <$> readProps ps <*> readNodes kids
      e -> Left ("not a node: " <> show e)
    readProps = \case
      ListE ps -> mapM readProp ps
      AppE (VarE c) (ListE es) | nameBase c == "concat" -> concat <$> mapM readProps es
      e -> Left ("not a list of properties: " <> show e)
    readProp = \case
      AppE (VarE f) (AppE (ConE c) s) | nameBase f == "class_", nameBase c == "ClassName" -> (,) "class" <$> packed s
      AppE (VarE f) s | nameBase f == "id" -> (,) "id" <$> packed s
      AppE (VarE f) s | nameBase f == "title" -> (,) "title" <$> packed s
      AppE (AppE (VarE f) (AppE (ConE c) n)) s | nameBase f == "attr", nameBase c == "AttrName" -> (,) <$> packed n <*> packed s
      e -> Left ("not a property: " <> show e)

-- | A text literal as the quoter writes it, @T.pack "…"@.
packed :: Exp -> Either String Text
packed = \case
  AppE (VarE p) (LitE (StringL s)) | nameBase p == "pack" -> Right (T.pack s)
  e -> Left ("not a text: " <> show e)

----------------------------------------------------------------------
-- CSS

data Rule
  = Decl Text Text
  | Nest Text [Rule]
  deriving (Eq, Show)

newtype Sheet = Sheet [Rule]
  deriving (Show)

instance Arbitrary Sheet where
  arbitrary = Sheet <$> resize 4 (listOf (rule (3 :: Int)))
    where
      rule depth = frequency [(3, Decl <$> elements properties <*> elements values), (if depth > 0 then 2 else 0, nest depth)]
      nest depth = Nest <$> elements selectors <*> resize 3 (listOf (rule (depth - 1)))
      selectors = [".a", ".b-c", "#id", "p", "ul li", "a.on"]
      properties = ["color", "margin", "display", "--gap", "border"]
      values = ["red", "4px 8px", "flex", "1px solid black", "calc(100% - 2px)"]
  shrink (Sheet rules) = Sheet <$> shrinkList shrinkRule rules
    where
      shrinkRule = \case
        Decl _ _ -> []
        Nest s rules' -> rules' <> [Nest s rules'' | rules'' <- shrinkList shrinkRule rules']

css :: Rule -> Text
css = \case
  Decl p v -> p <> ": " <> v <> ";"
  Nest s rules -> s <> " { " <> T.unwords (map css rules) <> " }"

-- | The quoter's code for rules, as the rules: one rule stands alone, several
-- are sequenced.
readRules :: Exp -> Either String [Rule]
readRules = \case
  AppE (VarE s) inner | nameBase s == "sequence_" -> readMany inner
  e -> pure <$> readRule e
  where
    readMany = \case
      ListE es -> mapM readRule es
      AppE (VarE c) (ListE es) | nameBase c == "concat" -> concat <$> mapM readMany es
      e -> Left ("not a list of rules: " <> show e)
    readRule = \case
      AppE (AppE (VarE q) sel) body | nameBase q == "?" -> Nest <$> selector sel <*> readRules body
      AppE (AppE (VarE k) key) value | nameBase k == "key" -> Decl <$> plain "Key" key <*> plain "Value" value
      e -> Left ("not a rule: " <> show e)
    selector = \case
      AppE (VarE el) s | nameBase el == "element" -> packed s
      e -> Left ("not a selector: " <> show e)
    plain con = \case
      AppE (ConE c) (AppE (ConE p) s) | nameBase c == con, nameBase p == "Plain" -> packed s
      e -> Left ("not a " <> con <> ": " <> show e)
