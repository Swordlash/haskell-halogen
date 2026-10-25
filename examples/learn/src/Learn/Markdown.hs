{-# LANGUAGE DeriveLift #-}

-- | The Markdown a lesson's text is written in: a small subset, read at
-- compile time ("Learn.Lessons") into blocks the page renders
-- ("Learn.View"). What it reads, and nothing else, is listed in
-- @examples\/learn\/README.md@:
--
-- * headings, @#@ to @###@;
-- * paragraphs, lines that follow one another, ended by a blank line;
-- * fenced code blocks, between lines of three backticks, the first of
--   which may name the language (@haskell@);
-- * lists, each item a line starting with @- @ or @* @ (unordered) or a
--   number and @. @ (ordered), continued on lines indented under it;
-- * blockquotes, lines starting with @> @, whose content is read as blocks;
-- * inline, within any of those: @`code`@, @**bold**@, @*italic*@ or
--   @_italic_@, @[text](address)@, and a backslash before a character to
--   take it literally.
--
-- Anything else is a paragraph's text, as written. A construct that is
-- started and not ended (a code block without its closing fence, a link
-- without its address) is an error, which fails the build and names the
-- line, rather than a page that looks wrong.
module Learn.Markdown
  ( Block (..)
  , Inline (..)
  , parseMarkdown
  , parseInlines
  , plainText
  )
where

import Data.List (span)
import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Lift)
import Protolude

data Block
  = Heading Int [Inline]
  | Paragraph [Inline]
  | -- | A fenced code block: the language its fence names, if any, and the
    -- code.
    CodeBlock (Maybe Text) Text
  | Bullets [[Inline]]
  | Numbered [[Inline]]
  | Quote [Block]
  deriving stock (Eq, Show, Lift)

data Inline
  = Plain Text
  | Code Text
  | Strong [Inline]
  | Emphasis [Inline]
  | -- | A link: its address and its text.
    Link Text [Inline]
  deriving stock (Eq, Show, Lift)

-- | The blocks of a text, or why it cannot be read, with the line (from 1).
parseMarkdown :: Text -> Either Text [Block]
parseMarkdown = blocks . zip [1 ..] . T.lines

type Line = (Int, Text)

blocks :: [Line] -> Either Text [Block]
blocks = \case
  [] -> pure []
  ls@((n, l) : rest)
    | T.null (T.strip l) -> blocks rest
    | Just fence <- T.stripPrefix "```" (T.stripStart l) -> do
        let language = T.strip fence
            (code, after) = break (("```" `T.isPrefixOf`) . T.stripStart . snd) rest
        case after of
          [] -> Left (at n "a code block is not closed with ```")
          _ : more -> do
            let block = CodeBlock (if T.null language then Nothing else Just language) (T.intercalate "\n" (map snd code))
            (block :) <$> blocks more
    | Just (level, title) <- heading l -> do
        inlines <- inline n title
        (Heading level inlines :) <$> blocks rest
    | quoted l -> do
        let (quote, after) = span (quoted . snd) ls
        inner <- blocks [(m, unquote q) | (m, q) <- quote]
        (Quote inner :) <$> blocks after
    | Just _ <- bullet l -> items Bullets bullet ls
    | Just _ <- numbered l -> items Numbered numbered ls
    | otherwise -> do
        let (para, after) = break (ends . snd) ls
        inlines <- inline n (T.unwords (map (T.strip . snd) para))
        (Paragraph inlines :) <$> blocks after
  where
    -- What ends a paragraph: a blank line, or a line that starts another
    -- block.
    ends l =
      T.null (T.strip l)
        || "```" `T.isPrefixOf` T.stripStart l
        || isJust (heading l)
        || quoted l
        || isJust (bullet l)
        || isJust (numbered l)

    quoted l = ">" `T.isPrefixOf` l
    unquote l = maybe (T.drop 1 l) identity (T.stripPrefix "> " l)

    -- A list: its items, each the marker's line and the indented lines
    -- under it.
    items make marker ls = do
      let (found, after) = collect ls
      inlines <- traverse (\(m, item) -> inline m item) found
      (make inlines :) <$> blocks after
      where
        collect = \case
          (m, l) : more
            | Just firstLine <- marker l ->
                let (continued, after) = span (indented . snd) more
                    (others, rest) = collect after
                 in ((m, T.unwords (firstLine : map (T.strip . snd) continued)) : others, rest)
          more -> ([], more)
        indented l = not (T.null (T.strip l)) && " " `T.isPrefixOf` l && isNothing (marker (T.stripStart l))

heading :: Text -> Maybe (Int, Text)
heading l =
  let (hashes, rest) = T.span (== '#') l
      level = T.length hashes
   in if level >= 1 && level <= 3 && " " `T.isPrefixOf` rest then Just (level, T.strip rest) else Nothing

bullet :: Text -> Maybe Text
bullet l = T.strip <$> (T.stripPrefix "- " l <|> T.stripPrefix "* " l)

numbered :: Text -> Maybe Text
numbered l =
  let (digits, rest) = T.span isDigit l
   in if T.null digits then Nothing else T.strip <$> T.stripPrefix ". " rest

at :: Int -> Text -> Text
at n message = "line " <> show n <> ": " <> message

inline :: Int -> Text -> Either Text [Inline]
inline n = first (at n) . parseInlines

-- | The inline content of one block's text.
parseInlines :: Text -> Either Text [Inline]
parseInlines = fmap merge . go False
  where
    -- Whether the character before is a letter or a digit: an underscore
    -- after one is part of a name (handle_action), not emphasis.
    go :: Bool -> Text -> Either Text [Inline]
    go afterWord t = case T.uncons t of
      Nothing -> pure []
      Just ('\\', rest) | Just (c, more) <- T.uncons rest, not (isSpace c) -> (Plain (T.singleton c) :) <$> go False more
      Just ('`', rest) -> case T.breakOn "`" rest of
        (_, "") -> Left ("` is not closed: `" <> T.take 30 rest)
        (code, more) -> (Code code :) <$> go False (T.drop 1 more)
      Just ('*', rest)
        | Just inner <- T.stripPrefix "*" rest, startsWord inner -> delimited "**" Strong inner
        | startsWord rest -> delimited "*" Emphasis rest
      Just ('_', rest)
        | not afterWord && startsWord rest -> delimited "_" Emphasis rest
      Just ('[', rest) -> case T.breakOn "](" rest of
        (_, "") -> plain t
        (label, more) -> case T.breakOn ")" (T.drop 2 more) of
          (_, "") -> Left ("a link's address is not closed: [" <> T.take 30 rest)
          (address, after) -> do
            inner <- go False label
            (Link (T.strip address) inner :) <$> go False (T.drop 1 after)
      Just _ -> plain t

    -- Text up to the next character that may start something.
    plain t =
      let (txt, rest) = T.break (`elem` ['\\', '`', '*', '_', '[']) (T.drop 1 t)
          txt' = T.take 1 t <> txt
       in (Plain txt' :) <$> go (maybe False (isAlphaNum . snd) (T.unsnoc txt')) rest

    -- What may open emphasis: a marker followed by something other than a
    -- space, so that @a * b@ is a product and not emphasis.
    startsWord rest = maybe False (not . isSpace . fst) (T.uncons rest)

    delimited marker make rest = case T.breakOn marker rest of
      (_, "") -> Left (marker <> " is not closed: " <> marker <> T.take 30 rest)
      (inner, more) -> do
        content <- go False inner
        (make content :) <$> go False (T.drop (T.length marker) more)

    merge = \case
      Plain a : Plain b : more -> merge (Plain (a <> b) : more)
      Strong xs : more -> Strong (merge xs) : merge more
      Emphasis xs : more -> Emphasis (merge xs) : merge more
      Link a xs : more -> Link a (merge xs) : merge more
      x : more -> x : merge more
      [] -> []

-- | The text alone, as a title or a link's label is read aloud.
plainText :: [Inline] -> Text
plainText = foldMap $ \case
  Plain t -> t
  Code t -> t
  Strong xs -> plainText xs
  Emphasis xs -> plainText xs
  Link _ xs -> plainText xs
