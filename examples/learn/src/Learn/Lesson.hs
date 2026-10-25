{-# LANGUAGE DeriveLift #-}

-- | A lesson: what one directory of @examples\/learn\/lessons\/@ holds, read at
-- compile time ("Learn.Lessons"). The format is described for authors in
-- @examples\/learn\/README.md@; in short, @NN-slug\/@ holds
--
-- * @lesson.md@: a header of @key: value@ lines up to the first blank line
--   (@title@, required; @layers@ and @args@, optional), then the text, in
--   the Markdown of "Learn.Markdown";
-- * @Start.hs@, what the editor opens with, and @Solution.hs@, the lesson's
--   answer: each the module @Lesson@;
-- * optionally @Main.hs@, the program around @Lesson@, in place of
--   'defaultMain'.
module Learn.Lesson
  ( Lesson (..)
  , parseLesson
  , parseDirectoryName
  , defaultMain
  , wrapper
  )
where

import Data.Char (isAsciiLower)
import Data.List (lookup)
import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Lift)
import Learn.Markdown (Block (..), parseMarkdown)
import Protolude

data Lesson = Lesson
  { slug :: Text
  -- ^ The directory's name, @01-hello@: the lesson's address, and the key
  -- its code is kept under in the browser.
  , number :: Int
  , title :: Text
  , text :: [Block]
  , start :: Text
  , solution :: Text
  , main :: Maybe Text
  -- ^ The lesson's own @Main.hs@, if it has one.
  , layers :: [Text]
  -- ^ The rootfs layers its programs need beside the base one, by name
  -- (@hooks@): their package databases are passed to GHC.
  , args :: [Text]
  -- ^ Further GHC arguments, such as @-package haskell-halogen-hooks@.
  }
  deriving stock (Eq, Show, Lift)

-- | The number and the slug a directory's name gives: @01-hello@ is lesson 1.
-- A slug is lower-case letters, digits and hyphens, so that it is an
-- address as it is.
parseDirectoryName :: Text -> Either Text Int
parseDirectoryName name = case T.span isDigit name of
  (digits, rest)
    | T.length digits == 2
    , Just word <- T.stripPrefix "-" rest
    , not (T.null word)
    , T.all (\c -> isAsciiLower c || isDigit c || c == '-') word ->
        maybe (Left "not a number") Right (readMaybe digits)
  _ -> Left (name <> ": a lesson's directory is named NN-slug, two digits, a hyphen and lower-case letters, digits or hyphens")

-- | A lesson from its directory's name and its files' contents: @lesson.md@,
-- @Start.hs@, @Solution.hs@ and @Main.hs@ if there is one.
parseLesson :: Text -> Text -> Text -> Text -> Maybe Text -> Either Text Lesson
parseLesson slug markdown start solution main = first ((slug <> ": ") <>) $ do
  number <- parseDirectoryName slug
  let (headerLines, body) = break (T.null . T.strip) (T.lines markdown)
  fields <- traverse field headerLines
  for_ fields $ \(key, _) ->
    unless (key `elem` ["title", "layers", "args"]) $ Left ("lesson.md: unknown header field " <> key <> " (title, layers, args)")
  title <- maybe (Left "lesson.md: the header has no title") Right (lookup "title" fields)
  text <- first ("lesson.md, after the header, " <>) (parseMarkdown (T.unlines body))
  -- The page's h1 is the title.
  when (any isTitle text) $ Left "lesson.md: the title is the header's; the text's headings are ## and ###"
  let words' key = maybe [] (T.words . T.replace "," " ") (lookup key fields)
  for_ [("Start.hs", start), ("Solution.hs", solution)] $ \(file, source) ->
    unless (any (\l -> "module Lesson" `T.isPrefixOf` l) (T.lines source)) $
      Left (file <> ": a lesson's code is the module Lesson")
  pure
    Lesson
      { slug
      , number
      , title
      , text
      , start
      , solution
      , main
      , layers = words' "layers"
      , args = words' "args"
      }
  where
    isTitle = \case
      Heading 1 _ -> True
      Quote inner -> any isTitle inner
      _ -> False

    field l = case T.breakOn ":" l of
      (key, value)
        | not (T.null value) -> Right (T.strip key, T.strip (T.drop 1 value))
      _ -> Left ("lesson.md: a header line is key: value, and this is not: " <> l <> " (the header ends at the first blank line)")

-- | The program a lesson's code runs in: its own @Main.hs@, or
-- 'defaultMain'.
wrapper :: Lesson -> Text
wrapper lesson = fromMaybe defaultMain lesson.main

-- | The @Main@ around a lesson that has none of its own: it mounts
-- @Lesson.component@, with no input, into the page's @#learn-result@,
-- in place of whatever the last run mounted there.
--
-- It is what any program that is run again on the same page does
-- ("Halogen.IO.Util"): end the last run's tree first, and keep how to end
-- this one for the next.
defaultMain :: Text
defaultMain =
  T.unlines
    [ "{-# LANGUAGE OverloadedRecordDot #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "module Main (main) where"
    , ""
    , "import Halogen (HalogenSocket (..))"
    , "import Halogen.IO.Util qualified as HA"
    , "import Halogen.VDom.DOM.Monad (runBrowserDOM)"
    , "import Halogen.VDom.Driver (runUI)"
    , "import Lesson (component)"
    , "import Web.DOM.ParentNode (QuerySelector (..))"
    , ""
    , "main :: IO ()"
    , "main = runBrowserDOM $ do"
    , "  HA.endPreviousRun"
    , "  result <- HA.selectElement (QuerySelector \"#learn-result\") >>= maybe (error \"the page has no #learn-result\") pure"
    , "  socket <- runUI component () result"
    , "  HA.keepForNextRun socket.dispose"
    ]
