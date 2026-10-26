-- | What the tutorial reads natively: the Markdown of a lesson's text
-- ("Learn.Markdown"), read at compile time, so that what it gets wrong
-- fails here rather than shows on a page, and GHC's output
-- ("Learn.Diagnostics", "Learn.Types").
module Main (main) where

import Learn.Diagnostics (didNotCompile, withoutMarks)
import Learn.Markdown (Block (..), Inline (..), parseInlines, parseMarkdown)
import Learn.Types (Binding (..), displayName, splitTypes)
import Protolude
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = hspec $ do
  markdown
  diagnostics
  types

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
  -- What GHC 9.14 wrote for a Lesson with --learn-spans, the mark broken
  -- over lines as GHC printed it.
  it "takes the playground's marks out of what GHC quotes of the code" $
    withoutMarks
      ( unlines
          [ "/tmp/Lesson.hs:17:17: error: [GHC-64725]"
          , "    * No such field: \"disabled\""
          , "    * In the expression: HP.disabled \"yes\""
          , "      In the first argument of `HH.p', namely"
          , "        `[Learn__Spans.attr"
          , "            (Learn__Spans.AttrName \"data-learn-src\") \"17:11\","
          , "          HP.disabled \"yes\"]'"
          , "      In the expression:"
          , "        HH.p"
          , "          [Learn__Spans.attr"
          , "             (Learn__Spans.AttrName \"data-learn-src\") \"17:11\","
          , "           HP.disabled \"yes\"]"
          , "          [HH.text \"p\"]"
          ]
      )
      `shouldBe` unlines
        [ "/tmp/Lesson.hs:17:17: error: [GHC-64725]"
        , "    * No such field: \"disabled\""
        , "    * In the expression: HP.disabled \"yes\""
        , "      In the first argument of `HH.p', namely"
        , "        `[HP.disabled \"yes\"]'"
        , "      In the expression:"
        , "        HH.p"
        , "          [HP.disabled \"yes\"]"
        , "          [HH.text \"p\"]"
        ]
  it "leaves an empty list of properties empty, and anything else as it was" $ do
    withoutMarks "In the expression: HH.p [Learn__Spans.attr (Learn__Spans.AttrName \"data-learn-src\") \"3:5\"] []"
      `shouldBe` "In the expression: HH.p [] []"
    withoutMarks "Not in scope: Learn__Spans.attrs" `shouldBe` "Not in scope: Learn__Spans.attrs"

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
