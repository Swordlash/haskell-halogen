-- | The Haskell inside a template: GHC's parser, translated to Template
-- Haskell, and the template's own grammar.
module Test.Haskell (spec) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Halogen.TH.Haskell (Failure (..), parseExp, parsePat)
import Halogen.TH.Template
import Language.Haskell.TH (Extension (..))
import Language.Haskell.TH qualified as TH
import Test.Hspec

exts :: [Extension]
exts = [OverloadedRecordDot, LambdaCase, MultiWayIf, BlockArguments, TypeApplications, OverloadedStrings]

pprinted :: Either Failure TH.Exp -> Either Failure String
pprinted = fmap TH.pprint

extracted :: Text -> (Text, Holes)
extracted = either (error . show) id . extract

spec :: Spec
spec = do
  describe "expressions" $ do
    it "record dot, application and operators" $
      parseExp exts "st.input.setup.panel && not (f 1)"
        `shouldBe` Right
          ( TH.UInfixE
              (TH.GetFieldE (TH.GetFieldE (TH.GetFieldE (TH.VarE (TH.mkName "st")) "input") "setup") "panel")
              (TH.VarE (TH.mkName "&&"))
              (TH.AppE (TH.VarE (TH.mkName "not")) (TH.ParensE (TH.AppE (TH.VarE (TH.mkName "f")) (TH.LitE (TH.IntegerL 1)))))
          )
    it "lambda case, comprehensions, if, let, do, records, sections" $ do
      pprinted (parseExp exts "\\case { Just x -> x; Nothing -> 0 }") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "[row x | x <- xs, cond x]") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "if a then b else c") `shouldBe` Right "if a then b else c"
      pprinted (parseExp exts "let { y = 1; f z = z + y } in f 2") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "do { x <- get; let { y = x }; pure (y, x) }") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "r { a = 1, b = c } . (.d)") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "(+ 1) . (2 -)") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "show @Int (x :: Int)") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "if | a -> 1 | otherwise -> 2") `shouldSatisfy` either (const False) (const True)
      pprinted (parseExp exts "case x of { Just y | y > 0, even y -> y; _ -> 0 }") `shouldSatisfy` either (const False) (const True)
    it "the extensions are the module's" $ do
      parseExp [] "a.b" `shouldBe` Right (TH.UInfixE (TH.VarE (TH.mkName "a")) (TH.VarE (TH.mkName ".")) (TH.VarE (TH.mkName "b")))
      parseExp exts "a.b" `shouldBe` Right (TH.GetFieldE (TH.VarE (TH.mkName "a")) "b")
    it "a parse error has its position" $
      parseExp exts "f (1 +\n  2" `shouldSatisfy` \case
        Left (Failure l c _) -> l == 2 && c > 0
        _ -> False
    it "what a template cannot hold is refused with a reason" $
      parseExp [Arrows] "proc x -> f -< x" `shouldBe` Left (Failure 0 0 "an arrow is not supported in a template: proc x -> f -< x")
    it "a parse error is GHC's message" $
      parseExp exts "(1 +" `shouldSatisfy` \case
        Left (Failure 1 _ msg) -> "parse error" `elem` words (show msg) || "error" `elem` words (show msg)
        _ -> False

  describe "patterns" $ do
    it "variables, tuples, constructors, literals, wildcards" $ do
      parsePat exts "(i, Just x, _, -1, 'c', \"s\")"
        `shouldBe` Right
          ( TH.TupP
              [ TH.VarP (TH.mkName "i")
              , TH.ConP (TH.mkName "Just") [] [TH.VarP (TH.mkName "x")]
              , TH.WildP
              , TH.LitP (TH.IntegerL (-1))
              , TH.LitP (TH.CharL 'c')
              , TH.LitP (TH.StringL "s")
              ]
          )
      parsePat exts "x : xs@(_ : _)" `shouldSatisfy` either (const False) (const True)
      parsePat exts "Person { name, age = a }" `shouldSatisfy` either (const False) (const True)

  describe "the template" $ do
    it "cuts the Haskell out" $ do
      let (t, holes) = extracted "a {{ f \"}}\" x }} b {% for x in xs %} c {# gone #} d"
      pieces t `shouldBe` [Lit "a ", Ref 0, Lit " b ", Ref 1, Lit " c  d"]
      IntMap.elems holes `shouldBe` [Hole ValueHole 1 "f \"}}\" x", Hole StmtHole 1 "for x in xs"]
    it "balances braces in a value" $ do
      let (_, holes) = extracted "{{ r {a = 1} }}{{ s{x=1}}}"
      map (.code) (IntMap.elems holes) `shouldBe` ["r {a = 1}", "s{x=1}"]
    it "passes braces and quotes in character literals" $ do
      let (_, holes) = extracted "{{ f '}' x' }}{{ g '{' }}{{ h '\\'' }}{{ k '\\x7d' }}{% if c == '%' %}{{ 'Just }}"
      map (.code) (IntMap.elems holes) `shouldBe` ["f '}' x'", "g '{'", "h '\\''", "k '\\x7d'", "if c == '%'", "'Just"]
    it "counts lines" $ do
      let (_, holes) = extracted "\n\n{{ a }}\n{% if\n b %}"
      map (.line) (IntMap.elems holes) `shouldBe` [3, 4]
    it "reports what is never closed" $ do
      extract "x {{ y" `shouldBe` Left (1, "{{ is never closed by }}")
      extract "\n{% y" `shouldBe` Left (2, "{% is never closed by %}")
    it "reads statements" $ do
      parseStmt "if a > b" `shouldBe` Right (If "a > b")
      parseStmt "for (i, x) in zip [1 ..] xs" `shouldBe` Right (For "(i, x)" "zip [1 ..] xs")
      parseStmt "let y = x + 1" `shouldBe` Right (Let "y" "x + 1")
      parseStmt "  if  a ==  \"x  y\" " `shouldBe` Right (If "a ==  \"x  y\"")
      parseStmt "for (inner, x)  in  [(\"a  b\", 1)]" `shouldBe` Right (For "(inner, x)" "[(\"a  b\", 1)]")
      parseStmt "let s = \"in  deed\"" `shouldBe` Right (Let "s" "\"in  deed\"")
      parseStmt "endfor" `shouldBe` Right End
      parseStmt "unless x" `shouldSatisfy` either (const True) (const False)
    it "groups blocks and refuses the unbalanced" $ do
      let holes = IntMap.fromList [(0, Hole StmtHole 1 "if a"), (1, Hole StmtHole 2 "else"), (2, Hole StmtHole 3 "end"), (3, Hole StmtHole 4 "for x in xs")]
          grouped = blocks holes :: [Item Char] -> Either Err [Block Char]
      grouped [One 'a', Ctl 0, One 'b', Ctl 1, One 'c', Ctl 2, One 'd']
        `shouldBe` Right [Leaf 'a', Cond [(0, [Leaf 'b'])] (Just [Leaf 'c']), Leaf 'd']
      grouped [Ctl 3, One 'x'] `shouldBe` Left (4, "{% for %} is never closed by {% end %}")
      grouped [Ctl 2] `shouldBe` Left (3, "{% end %} closes nothing")
      grouped [Ctl 0, Ctl 1, Ctl 1, Ctl 2] `shouldBe` Left (2, "{% else %} after an {% else %}")
