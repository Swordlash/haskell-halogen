-- | What the tutorial reads natively: the Markdown of a lesson's text
-- ("Learn.Markdown"), read at compile time, so that what it gets wrong
-- fails here rather than shows on a page, GHC's output
-- ("Learn.Diagnostics", "Learn.Types"), and the playground's addresses and
-- arguments ("Learn.Route", "Learn.Playground").
module Main (main) where

import Halogen.Router.Duplex qualified as Duplex
import Learn.Diagnostics (didNotCompile, withoutMarks)
import Learn.Lesson (parseDirectoryName)
import Learn.Markdown (Block (..), Inline (..), parseInlines, parseMarkdown)
import Learn.Playground (Handoff (..), Kept (..), Opening (..), opening, packageArgs, readLayers, showLayers, withoutPackages)
import Learn.Route (Route (..), route)
import Learn.Types (Binding (..), displayName, splitTypes)
import Protolude
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = hspec $ do
  markdown
  diagnostics
  types
  playground

playground :: Spec
playground = describe "Learn.Playground" $ do
  it "is at playground/, which is no lesson's address" $ do
    rightToMaybe (Duplex.parse route "/playground/") `shouldBe` Just Playground
    Duplex.print route Playground `shouldBe` "/playground/"
    rightToMaybe (Duplex.parse route "/02-counter/") `shouldBe` Just (LessonPage "02-counter")
    -- A lesson's directory, its slug, starts with its number.
    parseDirectoryName "playground" `shouldSatisfy` isLeft
  it "names each package of the layers checked once, but the base layer's" $
    packageArgs
      ["haskell-halogen-jsbits", "haskell-halogen-core"]
      [("hooks", ["haskell-halogen-hooks"]), ("lazy", ["haskell-halogen-lazy", "haskell-halogen-lazy-demo"]), ("odd", ["haskell-halogen-core", "haskell-halogen-hooks"])]
      ["lazy", "odd", "hooks"]
      `shouldBe` ["-package", "haskell-halogen-hooks", "-package", "haskell-halogen-lazy", "-package", "haskell-halogen-lazy-demo"]
  it "takes a lesson's code, its layers, and its arguments but for the -package flags its layers give" $ do
    let handoff = Handoff {code = "module Lesson where\n", layers = ["lazy"], args = ["-package", "haskell-halogen-lazy", "-fdefer-typed-holes", "-package", "haskell-halogen-lazy-demo"]}
    opening (Just handoff) (Kept Nothing Nothing Nothing) `shouldBe` Opening {code = Just "module Lesson where\n", layers = ["lazy"], extra = "-fdefer-typed-holes"}
    withoutPackages ["-package"] `shouldBe` ["-package"]
  -- What was kept is what the storage has: a write it refused leaves the
  -- playground's earlier code there, which a hand-off must win over.
  it "opens with a hand-off over what was kept, and with what was kept, if anything, without one" $ do
    let handoff = Handoff {code = "module Lesson where -- the lesson's\n", layers = [], args = []}
        kept = Kept {code = Just "module Lesson where -- kept before\n", layers = Just "store,hooks", args = Just "-Wall"}
    opening (Just handoff) kept `shouldBe` Opening {code = Just handoff.code, layers = [], extra = ""}
    opening Nothing kept `shouldBe` Opening {code = kept.code, layers = ["store", "hooks"], extra = "-Wall"}
    opening Nothing (Kept Nothing Nothing Nothing) `shouldBe` Opening {code = Nothing, layers = [], extra = ""}
  it "keeps the layers as it reads them" $ do
    readLayers (showLayers ["store", "hooks"]) `shouldBe` ["store", "hooks"]
    readLayers "" `shouldBe` []

diagnostics :: Spec
diagnostics = describe "Learn.Diagnostics" $ do
  it "reads an error GHC threw as a failed compile" $
    didNotCompile
      ( unlines
          [ "/tmp/Lesson.hs:7:18: error: [GHC-58481]"
          , "    parse error on input `type'"
          , "  |"
          , "7 | import Data.Row (type (.==))"
          , "  |                  ^^^^"
          ]
      )
      `shouldBe` True
  it "reads a panic as a failed compile" $
    didNotCompile "<interactive>: panic! (the 'impossible' happened)\n  GHC version 9.14.1:\n\tPlease report this as a GHC bug:  https://www.haskell.org/ghc/reportabug\n"
      `shouldBe` True
  it "does not read a warning, or the program's own failure, as one" $ do
    didNotCompile "/tmp/Lesson.hs:3:1: warning: [GHC-40910] [-Wunused-top-binds]\n    Defined but not used: `x'\n" `shouldBe` False
    didNotCompile "<interactive>: boom\nCallStack (from HasCallStack):\n  error, called at /tmp/Lesson.hs:12:5 in main:Lesson\n" `shouldBe` False
  -- What GHC 9.14 wrote for a Lesson with --learn-spans (at the
  -- playground's path), the marks broken over lines as GHC printed them, an
  -- element marked inside a marked one, and the lists GHC shortened.
  it "takes the playground's marks out of what GHC quotes of the code" $
    withoutMarks
      ( unlines
          [ "/tmp/Lesson.hs:14:17: error: [GHC-64725]"
          , "    * No such field: \"disabled\""
          , "    * In the expression: HP.disabled \"yes\""
          , "      In the first argument of `HH.p', namely `[HP.disabled \"yes\"]'"
          , "      In the first argument of `HH.div_', namely"
          , "        `[Learn__Spans.withProps"
          , "            [Learn__Spans.attr"
          , "               Nothing (Learn__Spans.AttrName \"data-learn-src\") \"14:11\"]"
          , "            (HH.p"
          , "               [HP.disabled \"yes\"]"
          , "               [Learn__Spans.withProps [...] (HH.b_ [...]),"
          , "                Learn__Spans.withProps [...] (HH.br_)])]'"
          ]
      )
      `shouldBe` unlines
        [ "/tmp/Lesson.hs:14:17: error: [GHC-64725]"
        , "    * No such field: \"disabled\""
        , "    * In the expression: HP.disabled \"yes\""
        , "      In the first argument of `HH.p', namely `[HP.disabled \"yes\"]'"
        , "      In the first argument of `HH.div_', namely"
        , "        `[HH.p"
        , "               [HP.disabled \"yes\"]"
        , "               [HH.b_ [...],"
        , "                HH.br_]]'"
        ]
  it "takes out a mark GHC shortened to ..., and leaves its own ... and .... as they were" $
    withoutMarks
      ( unlines
          [ "                render ()"
          , "                  = Learn__Spans.withProps"
          , "                      [Learn__Spans.attr"
          , "                         Nothing (Learn__Spans.AttrName \"data-learn-src\") \"18:7\"]"
          , "                      (HH.div [HP.id \"root\"] [Learn__Spans.withProps ... (p_ ...), ....])"
          , "                ...."
          ]
      )
      `shouldBe` unlines
        [ "                render ()"
        , "                  = HH.div [HP.id \"root\"] [p_ ..., ....]"
        , "                ...."
        ]
  it "takes out the mark at the head of a partially applied element's list, with its comma" $ do
    withoutMarks
      ( unlines
          [ "    * In the expression: HP.disabled \"yes\""
          , "      In the first argument of `HH.li', namely"
          , "        `[Learn__Spans.IProp"
          , "            (Learn__Spans.attr"
          , "               Nothing (Learn__Spans.AttrName \"data-learn-src\") \"41:24\"),"
          , "          HP.disabled \"yes\", HP.class_ (HH.ClassName \"c\")]'"
          , "      In the first argument of `map', namely"
          , "        `(HH.li"
          , "            [Learn__Spans.IProp"
          , "               (Learn__Spans.attr"
          , "                  Nothing (Learn__Spans.AttrName \"data-learn-src\") \"41:24\"),"
          , "             HP.disabled \"yes\", HP.class_ (HH.ClassName \"c\")])'"
          ]
      )
      `shouldBe` unlines
        [ "    * In the expression: HP.disabled \"yes\""
        , "      In the first argument of `HH.li', namely"
        , "        `[HP.disabled \"yes\", HP.class_ (HH.ClassName \"c\")]'"
        , "      In the first argument of `map', namely"
        , "        `(HH.li"
        , "            [HP.disabled \"yes\", HP.class_ (HH.ClassName \"c\")])'"
        ]
    withoutMarks "HH.li [Learn__Spans.IProp (...)] <$> xs" `shouldBe` "HH.li [] <$> xs"
    withoutMarks "Learn__Spans.IProp (x)" `shouldBe` "Learn__Spans.IProp (x)"
  it "matches the parenthesis past those in strings and characters, and leaves anything else as it was" $ do
    withoutMarks "In the expression: Learn__Spans.withProps [...] (HH.text \"a) (\\\" ]\" <> f ')') <> x"
      `shouldBe` "In the expression: HH.text \"a) (\\\" ]\" <> f ')' <> x"
    withoutMarks "In the expression: Learn__Spans.withProps [Learn__Spans.attr Nothing (Learn__Spans.AttrName \"data-learn-src\") \"3:5\"] HH.br_"
      `shouldBe` "In the expression: HH.br_"
    withoutMarks "Not in scope: Learn__Spans.withPropsy" `shouldBe` "Not in scope: Learn__Spans.withPropsy"
    withoutMarks "Learn__Spans.withProps [x] (y)" `shouldBe` "Learn__Spans.withProps [x] (y)"
    withoutMarks "Learn__Spans.withProps [...] (HH.p [" `shouldBe` "(HH.p ["

types :: Spec
types = describe "Learn.Types" $ do
  it "takes the bindings out of what the program wrote, and leaves the rest as it was" $
    splitTypes
      ( unlines
          [ "learn-type\t12\t1\trender\tState -> HH.HTML w Action"
          , "learn-type\t30\t3\t<+>\tInt -> Int -> Int"
          , "Hello"
          , ""
          , "learn-type: not one of them"
          ]
      )
      `shouldBe` (
                   [ Binding {line = 12, column = 1, name = "render", type_ = "State -> HH.HTML w Action"}
                   , Binding {line = 30, column = 3, name = "<+>", type_ = "Int -> Int -> Int"}
                   ]
                 , "Hello\n\nlearn-type: not one of them\n"
                 )
  it "keeps a line whose position is not a number" $
    splitTypes "learn-type\tx\t1\tf\tInt" `shouldBe` ([], "learn-type\tx\t1\tf\tInt")
  it "puts an operator in parentheses" $
    map displayName [Binding 1 1 "<+>" "Int", Binding 1 1 "_x" "Int", Binding 1 1 "x'" "Int"] `shouldBe` ["(<+>)", "_x", "x'"]

markdown :: Spec
markdown = describe "Learn.Markdown" $ do
  describe "links" $ do
    it "ends a link's text at the first ]" $
      parseInlines "[1, 2] and [the docs](u)"
        `shouldBe` Right [Plain "[1, 2] and ", Link "u" [Plain "the docs"]]
    it "keeps balanced parentheses in an address" $
      parseInlines "see [monads](https://en.wikipedia.org/wiki/Monad_(functional_programming)), then"
        `shouldBe` Right [Plain "see ", Link "https://en.wikipedia.org/wiki/Monad_(functional_programming)" [Plain "monads"], Plain ", then"]
    it "refuses an address that is not closed" $
      parseInlines "[a](b(c)" `shouldSatisfy` isLeft

  describe "emphasis" $ do
    it "reads ** before a space as text, not an empty emphasis" $
      parseInlines "a ** b" `shouldBe` Right [Plain "a ** b"]
    it "still reads bold and italic" $
      parseInlines "**b** and *i*" `shouldBe` Right [Strong [Plain "b"], Plain " and ", Emphasis [Plain "i"]]

  describe "lists" $ do
    it "starts an ordered list at its first number" $
      parseMarkdown "3. three\n4. four\n" `shouldBe` Right [Numbered 3 [[Plain "three"], [Plain "four"]]]
    it "keeps one list across a blank line between items" $
      parseMarkdown "1. one\n\n2. two\n\nAfter.\n"
        `shouldBe` Right [Numbered 1 [[Plain "one"], [Plain "two"]], Paragraph [Plain "After."]]
    it "refuses a nested item rather than read it as a paragraph" $ do
      parseMarkdown "- a\n  - b\n" `shouldSatisfy` isLeft
      parseMarkdown "- a\n\n  - b\n" `shouldSatisfy` isLeft
    it "continues an item on the lines indented under it" $
      parseMarkdown "- a\n  more\n- b\n" `shouldBe` Right [Bullets [[Plain "a more"], [Plain "b"]]]

  it "reads CRLF line ends as LF" $
    parseMarkdown "## Title\r\n\r\nSome `code`.\r\n- item\r\n"
      `shouldBe` Right [Heading 2 [Plain "Title"], Paragraph [Plain "Some ", Code "code", Plain "."], Bullets [[Plain "item"]]]
