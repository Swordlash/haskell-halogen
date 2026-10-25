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
--   number and @. @ (ordered), continued on lines indented under it; an
--   ordered list starts at its first item's number, and a blank line
--   between two items does not end a list. Lists do not nest: an indented
--   item is an error;
-- * blockquotes, lines starting with @> @, whose content is read as blocks;
-- * inline, within any of those: @`code`@, @**bold**@, @*italic*@ or
--   @_italic_@, @[text](address)@, and a backslash before a character to
--   take it literally. A link's text ends at the first @]@, and its address
--   at the @)@ that closes its @(@, so an address may hold parentheses that
--   are balanced (@https:\/\/en.wikipedia.org\/wiki\/Monad_(functional_programming)@).
--
-- Line ends may be CRLF. Anything else is a paragraph's text, as written.
-- A construct that is started and not ended (a code block without its
-- closing fence, a link without its address) is an error, which fails the
-- build and names the line, rather than a page that looks wrong.
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
  | -- | An ordered list: the number it starts at, and its items.
    Numbered Int [[Inline]]
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
parseMarkdown = blocks . zip [1 ..] . map (T.dropWhileEnd (== '\r')) . T.lines

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
    | Just (start, _) <- numberedFrom l -> items (Numbered start) numbered ls
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
    -- under it. Blank lines between two items are part of the list.
    items make marker ls = do
      (found, after) <- collect ls
      inlines <- traverse (\(m, item) -> inline m item) found
      (make inlines :) <$> blocks after
      where
        collect = \case
          (m, l) : more
            | Just firstLine <- marker l -> do
                let (continued, after) = span (indented . snd) more
                    next = dropWhile (T.null . T.strip . snd) after
                case next of
                  (k, l') : _
                    | " " `T.isPrefixOf` l'
                    , isJust (bullet (T.stripStart l')) || isJust (numbered (T.stripStart l')) ->
                        Left (at k "lists do not nest: an indented item is not read as a list inside the item above")
                  _ -> pure ()
                -- Past the blank lines only if an item of this list comes
                -- next.
                (others, rest) <- collect $ case next of
                  (_, l') : _ | isJust (marker l') -> next
                  _ -> after
                pure ((m, T.unwords (firstLine : map (T.strip . snd) continued)) : others, rest)
          more -> pure ([], more)
        indented l = not (T.null (T.strip l)) && " " `T.isPrefixOf` l && isNothing (marker (T.stripStart l))

heading :: Text -> Maybe (Int, Text)
heading l =
  let (hashes, rest) = T.span (== '#') l
      level = T.length hashes
   in if level >= 1 && level <= 3 && " " `T.isPrefixOf` rest then Just (level, T.strip rest) else Nothing

bullet :: Text -> Maybe Text
bullet l = T.strip <$> (T.stripPrefix "- " l <|> T.stripPrefix "* " l)

numbered :: Text -> Maybe Text
numbered = fmap snd . numberedFrom

-- | An ordered list's item: its number and its text.
numberedFrom :: Text -> Maybe (Int, Text)
numberedFrom l =
  let (digits, rest) = T.span isDigit l
   in if T.null digits then Nothing else (,) <$> readMaybe (toS digits :: [Char]) <*> (T.strip <$> T.stripPrefix ". " rest)

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
        | Just inner <- T.stripPrefix "*" rest, opens '*' inner -> delimited "**" Strong inner
        | opens '*' rest -> delimited "*" Emphasis rest
      Just ('_', rest)
        | not afterWord && opens '_' rest -> delimited "_" Emphasis rest
      -- A link's text is up to the first ], with no [ in it (that [ may
      -- start the link); a ( right after the ] starts its address.
      Just ('[', rest)
        | (label, more) <- T.break (`elem` ['[', ']']) rest
        , Just after <- T.stripPrefix "](" more -> case closing 0 after of
            Nothing -> Left ("a link's address is not closed: [" <> T.take 30 rest)
            Just (address, remaining) -> do
              inner <- go False label
              (Link (T.strip address) inner :) <$> go False remaining
      Just _ -> plain t

    -- An address, up to the ) that closes the link's (: the parentheses
    -- inside it must balance.
    closing :: Int -> Text -> Maybe (Text, Text)
    closing depth t = case T.uncons t of
      Nothing -> Nothing
      Just (')', more)
        | depth == 0 -> Just ("", more)
        | otherwise -> first (T.cons ')') <$> closing (depth - 1) more
      Just ('(', more) -> first (T.cons '(') <$> closing (depth + 1) more
      Just (c, more) -> first (T.cons c) <$> closing depth more

    -- Text up to the next character that may start something.
    plain t =
      let (txt, rest) = T.break (`elem` ['\\', '`', '*', '_', '[']) (T.drop 1 t)
          txt' = T.take 1 t <> txt
       in (Plain txt' :) <$> go (maybe False (isAlphaNum . snd) (T.unsnoc txt')) rest

    -- What may open emphasis: a marker followed by something other than a
    -- space, so that @a * b@ is a product and not emphasis, or than the
    -- marker again, so that @** x@ is text and not an empty emphasis.
    opens marker rest = maybe False (\(c, _) -> not (isSpace c) && c /= marker) (T.uncons rest)

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
