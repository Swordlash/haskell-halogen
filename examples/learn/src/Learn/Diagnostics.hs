-- | GHC's errors and warnings, read back from what it wrote to stderr, for
-- the page to list under the editor and mark in it.
--
-- GHC writes each one as a location line,
--
-- > /tmp/Lesson.hs:12:5: error: [GHC-83865]
-- > /tmp/Lesson.hs:12:5-9: warning: [GHC-40910] [-Wunused-top-binds]
-- > /tmp/Lesson.hs:(12,5)-(14,20): error: [GHC-39999]
--
-- followed by its message, indented, up to the next blank line or the next
-- location, and with the offending source quoted under a gutter of @|@,
-- which is left out here: the editor shows the source itself.
--
-- The page has GHC mark the elements of the reader's code with where they are
-- in it (the playground's @--learn-spans@), and what GHC quotes of that code
-- then has the mark in it; 'withoutMarks' takes it out before anything here
-- reads GHC's output.
module Learn.Diagnostics
  ( Diagnostic (..)
  , Severity (..)
  , parseDiagnostics
  , didNotCompile
  , withoutMarks
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text qualified as T
import Protolude

data Severity = Error | Warning
  deriving stock (Eq, Show)

data Diagnostic = Diagnostic
  { file :: Text
  -- ^ The file's name without its directory: @Lesson.hs@ or @Main.hs@.
  , line :: Int
  , column :: Int
  , endLine :: Int
  , endColumn :: Int
  -- ^ Just past the end, as GHC counts it (a span @12:5-9@ ends at column
  -- 10); the start again when GHC gave only a point.
  , severity :: Severity
  , message :: Text
  }
  deriving stock (Eq, Show)

-- | The editor's lint markers want this (see @js\/editor.js@).
instance ToJSON Diagnostic where
  toJSON d =
    object
      [ "line" .= d.line
      , "column" .= d.column
      , "endLine" .= d.endLine
      , "endColumn" .= d.endColumn
      , "severity" .= (case d.severity of Error -> "error"; Warning -> "warning" :: Text)
      , "message" .= d.message
      ]

-- | Every diagnostic in GHC's output, in order.
parseDiagnostics :: Text -> [Diagnostic]
parseDiagnostics = go . T.lines
  where
    go = \case
      [] -> []
      l : rest
        | Just (d, firstLine) <- location l ->
            let (body, after) = break (\x -> T.null (T.strip x) || isJust (location x)) rest
                message = T.strip (T.unlines (firstLine : unindent (filter (not . quoted) body)))
             in d {message} : go after
        | otherwise -> go rest

    -- The source GHC quotes: lines of a gutter, with or without a line
    -- number (and a space after it), then a bar.
    quoted l = case T.uncons (T.stripStart (T.dropWhile isDigit (T.stripStart l))) of
      Just ('|', _) -> True
      _ -> False

    unindent ls =
      let depth = minimum (maxBound : [T.length (T.takeWhile isSpace x) | x <- ls, not (T.null (T.strip x))])
       in map (T.drop depth) ls

-- | Whether what GHC wrote says the program did not compile: it reported
-- an error, or GHC itself failed (a panic). The page cannot tell this from
-- how the run ended alone: the playground fails in a way of its own when
-- GHC's @load@ reports errors, but an error GHC throws instead (one in a
-- module's header, such as an import it cannot parse) or a panic ends the
-- run as the program's own @main@ failing does, with the message on stderr.
-- A program's own failure is not written as a location line, and not as a
-- panic.
didNotCompile :: Text -> Bool
didNotCompile output =
  any ((== Error) . (.severity)) (parseDiagnostics output)
    || "Please report this as a GHC bug" `T.isInfixOf` output
    || "panic! (the 'impossible' happened)" `T.isInfixOf` output

-- | What GHC wrote, without the wrapper the playground puts around each
-- element of the reader's code (@learn\/Playground.hs@'s @lessonSpans@).
-- GHC knows the code it compiled, marks and all, and quotes it where a
-- message shows the code around an error:
--
-- > In the first argument of `HH.div_', namely
-- >   `[Learn__Spans.withProps
-- >       [Learn__Spans.attr
-- >          Nothing (Learn__Spans.AttrName "data-learn-src") "14:11"]
-- >       (HH.p
-- >          [HP.disabled "yes"]
-- >          [Learn__Spans.withProps [...] (HH.b_ [...])])]'
--
-- which reads here as the reader wrote it, @`[HH.p [HP.disabled "yes"]
-- [HH.b_ [...]]]'@ but for the lines GHC broke, which stay as they were.
-- A mark is @Learn__Spans.withProps@, its list (or what GHC shortens a list
-- deep in an expression to, @[...]@, or a whole argument, @...@), and the
-- expression it wraps, in parentheses; it goes with the space between its
-- parts, wherever GHC broke the lines, and the parentheses go with it, but
-- what is inside them stays (and has its own marks taken out). The closing
-- parenthesis is the one that matches, not one in a string literal or a
-- character; when GHC's message does not close it, the parenthesis stays.
--
-- An element applied to fewer arguments than it takes (@HH.li [...]@ in
-- @map (HH.li [...]) groups@) has its mark as the first of its literal
-- list of properties instead, @Learn__Spans.IProp (Learn__Spans.attr
-- ...)@, which goes with the comma after it and the space after that.
--
-- A name that only starts like a mark, or one not followed by what a mark
-- is made of, is left as it was.
withoutMarks :: Text -> Text
withoutMarks = removing "Learn__Spans.withProps" unwrap . removing "Learn__Spans.IProp" unlist
  where
    unwrap s = do
      afterSpace <- T.stripPrefix " " s <|> T.stripPrefix "\n" s
      afterList <- T.stripStart <$> markList (T.stripStart afterSpace)
      pure $ fromMaybe afterList $ do
        inside <- T.stripPrefix "(" afterList
        end <- closing inside
        pure (T.take end inside <> T.drop (end + 1) inside)
    markList s = case T.stripPrefix "[" s of
      Just inside -> do
        end <- closingBracket inside
        let properties = T.strip (T.take end inside)
        guard (properties == "..." || "Learn__Spans.attr" `T.isPrefixOf` properties)
        pure (T.drop (end + 1) inside)
      Nothing -> T.stripPrefix "..." s
    unlist s = do
      afterSpace <- T.stripPrefix " " s <|> T.stripPrefix "\n" s
      let start = T.stripStart afterSpace
      afterMark <- case T.stripPrefix "(" start of
        Just inside -> do
          end <- closing inside
          let attribute = T.strip (T.take end inside)
          guard (attribute == "..." || "Learn__Spans.attr" `T.isPrefixOf` attribute)
          pure (T.drop (end + 1) inside)
        Nothing -> T.stripPrefix "..." start
      pure (maybe afterMark T.stripStart (T.stripPrefix "," afterMark))

-- | The text with each mark that starts with this name taken out, as the
-- function says what is left once the name and the rest of one are gone
-- (given what follows the name), or that what follows is not the rest of
-- one, and the name stays.
removing :: Text -> (Text -> Maybe Text) -> Text -> Text
removing name unmark = go
  where
    go t = case T.breakOn name t of
      (before, rest)
        | T.null rest -> before
        | Just after <- unmark (T.drop (T.length name) rest) -> before <> go after
        | otherwise -> before <> name <> go (T.drop (T.length name) rest)

-- | Where in the text the parenthesis that closes one just before it is.
closing :: Text -> Maybe Int
closing = matching '(' ')'

-- | Where in the text the bracket that closes one just before it is.
closingBracket :: Text -> Maybe Int
closingBracket = matching '[' ']'

-- | The offset of the closing character that matches an opening one just
-- before the text, past the ones nested in it and those in a string
-- literal (with its escapes) or a character literal (@'('@).
matching :: Char -> Char -> Text -> Maybe Int
matching open close = go (0 :: Int) 0
  where
    go depth at s = case T.uncons s of
      Nothing -> Nothing
      Just (c, rest)
        | c == close -> if depth == 0 then Just at else go (depth - 1) (at + 1) rest
        | c == open -> go (depth + 1) (at + 1) rest
        | c == '"' -> let n = stringLength rest in go depth (at + 1 + n) (T.drop n rest)
        | c == '\'', Just (quoted, rest') <- T.uncons rest, quoted `elem` ['(', ')', '[', ']', '"'], "'" `T.isPrefixOf` rest' -> go depth (at + 3) (T.drop 2 rest)
        | otherwise -> go depth (at + 1) rest
    -- The characters of a string literal after its opening quote, its
    -- closing one included.
    stringLength s = case T.uncons s of
      Nothing -> 0
      Just ('"', _) -> 1
      Just ('\\', rest) -> maybe 1 ((2 +) . stringLength . snd) (T.uncons rest)
      Just (_, rest) -> 1 + stringLength rest

-- | A location line: the diagnostic it starts (its message still empty),
-- and what follows the severity on the line, without GHC's codes.
location :: Text -> Maybe (Diagnostic, Text)
location l = do
  (path, afterPath) <- breakOnSource l
  (span', afterSpan) <- spanOf afterPath
  rest <- T.stripPrefix ": " afterSpan
  (severity, afterSeverity) <-
    (Error,) <$> T.stripPrefix "error:" rest
      <|> (Warning,) <$> T.stripPrefix "warning:" rest
  let (sl, sc, el, ec) = span'
  pure
    ( Diagnostic {file = T.takeWhileEnd (/= '/') path, line = sl, column = sc, endLine = el, endColumn = ec, severity, message = ""}
    , T.strip (dropCodes afterSeverity)
    )
  where
    -- The path, up to the colon before the line number.
    breakOnSource t =
      let (path, rest) = T.breakOn ".hs:" t
       in if T.null rest then Nothing else Just (path <> ".hs", T.drop 4 rest)

    -- 12:5, 12:5-9 or (12,5)-(14,20).
    spanOf t
      | Just inner <- T.stripPrefix "(" t = do
          (sl, r1) <- number inner
          (sc, r2) <- number =<< T.stripPrefix "," r1
          (el, r3) <- number =<< T.stripPrefix ")-(" r2
          (ec, r4) <- number =<< T.stripPrefix "," r3
          r5 <- T.stripPrefix ")" r4
          pure ((sl, sc, el, ec + 1), r5)
      | otherwise = do
          (sl, r1) <- number t
          (sc, r2) <- number =<< T.stripPrefix ":" r1
          case T.stripPrefix "-" r2 of
            Just r3 -> do
              (ec, r4) <- number r3
              pure ((sl, sc, sl, ec + 1), r4)
            Nothing -> pure ((sl, sc, sl, sc), r2)

    number t =
      let (digits, rest) = T.span isDigit t
       in (,rest) <$> readMaybe (toS digits :: [Char])

    -- [GHC-83865] [-Wunused-top-binds]
    dropCodes t = case T.uncons (T.stripStart t) of
      Just ('[', more) -> dropCodes (T.drop 1 (T.dropWhile (/= ']') more))
      _ -> t
