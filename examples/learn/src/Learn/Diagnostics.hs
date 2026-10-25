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
module Learn.Diagnostics
  ( Diagnostic (..)
  , Severity (..)
  , parseDiagnostics
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
