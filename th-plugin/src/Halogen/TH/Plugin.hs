{-# LANGUAGE CPP #-}

-- | A compiler plugin that makes the code of @hh@, @hhs@ and @clay@
-- quotes with every error where it is. GHC runs a quasi-quoter in the
-- type checker and reports whatever goes wrong inside the quote at the
-- quote; this plugin does the quoter's work as the module is parsed
-- instead: each quote of the three quoters becomes the code its quoter
-- makes, with the Haskell of every hole parsed by GHC itself at its place
-- in the file. So a parse error in a hole is the parser's, there; a type
-- error in a hole is the type checker's, there; and what the template
-- itself gets wrong is reported at its line. The quoter never runs for a
-- quote the plugin has expanded, and the code is the same.
--
-- @ghc-options: -fplugin=Halogen.TH.Plugin@ in a cabal stanza, or
-- @{-\# OPTIONS_GHC -fplugin=Halogen.TH.Plugin \#-}@ in a module, with
-- @haskell-halogen-th-plugin@ among the dependencies. A language server
-- that builds on GHC shows the errors there too. The quoters are known by
-- their names, so another @hh@ in scope would be read as this one.
module Halogen.TH.Plugin (plugin) where

import Control.Exception (throwIO)
import Data.Generics (everywhere', everywhereM, extT, mkM, mkT)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.Bag (bagToList)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Hs (GhcPs, HsBind, HsBindLR (..), HsExpr (..), HsParsedModule (..), HsUntypedSplice (..), LHsExpr, LPat, Pat (..))
import GHC.Parser qualified as Parser
import GHC.Parser.Annotation (locA)
import GHC.Parser.Errors.Types (PsMessage (PsUnknownMessage))
import GHC.Parser.Lexer (P (..), ParseResult (..), getPsErrorMessages, initParserState)
import GHC.Parser.PostProcess (runPV, unECP)
import GHC.Plugins hiding (quote, quotes, (<>))
import GHC.Tc.Errors.Types (SpliceFailReason (RunSpliceFailure), THError (THSpliceFailed), TcRnMessage (TcRnTHError))
import GHC.ThToHs (convertToHsExpr)
import GHC.Types.Error (Diagnostic (..), MsgEnvelope, addMessage, defaultDiagnosticOpts, getMessages, mkPlainError, mkSimpleUnknownDiagnostic, noHints, unDecorated)
import GHC.Utils.Error (mkErrorMsgEnvelope)
import Halogen.TH.Check (Complaint (Complaint), Quoter, quoted, quoterName, quoterNamed)
import Halogen.TH.Haskell (Failure (..), namedExtensions, parseExpAt, parsePatAt)
import Halogen.TH.Template (Env (..))
import Language.Haskell.TH (runIO, runQ)
import Language.Haskell.TH.Syntax qualified as TH

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = \_ summary result -> liftIO (expandModule summary result)
    , pluginRecompile = purePlugin
    }

-- | What stops a quote from being expanded, as the parser's errors.
newtype Problems = Problems [MsgEnvelope PsMessage]

instance Show Problems where
  show (Problems es) = show (length es) <> " problems with a quote"

instance Exception Problems

-- | Every quote of the three quoters in the module replaced by its code;
-- the problems of the quotes that could not be join the parser's errors,
-- so the module stops here, with each at its place.
expandModule :: ModSummary -> ParsedResult -> IO ParsedResult
expandModule summary result = do
  problems <- newIORef []
  let L l m = hpm_module (parsedResultModule result)
  m' <- everywhereM (mkM (quote problems)) m
  found <- readIORef problems
  let messages = parsedResultMessages result
  pure
    result
      { parsedResultModule = (parsedResultModule result) {hpm_module = L l m'}
      , parsedResultMessages = messages {psErrors = foldr addMessage (psErrors messages) found}
      }
  where
    quote :: IORef [MsgEnvelope PsMessage] -> LHsExpr GhcPs -> IO (LHsExpr GhcPs)
    quote problems e@(L _ (HsUntypedSplice _ (HsQuasiQuote _ quoter body)))
      | Just q <- quoterNamed (T.pack (occNameString (rdrNameOcc name)))
      , at@(RealSrcSpan _ _) <- locA body = do
          made <- expandQuote (ms_hspp_opts summary) q name at (T.pack (unpackFS (unLoc body)))
          case made of
            Right e' -> pure e'
            Left errs -> e <$ modifyIORef' problems (errs <>)
      where
#if MIN_VERSION_ghc(9,14,0)
        name = unLoc quoter
#else
        name = quoter
#endif
    quote _ e = pure e

-- | The Haskell of a hole, as GHC parsed it at its place.
data Filling
  = AnExpression (LHsExpr GhcPs)
  | APattern (LPat GhcPs)

-- | The code a quote's quoter makes, with the Haskell of every hole as GHC
-- parses it at its place in the file, or the problems that stop it. The
-- quoter gets an environment whose holes are placeholders, named so that
-- no hole can contain them; the quoter's Template Haskell is converted to
-- GHC's syntax, and the holes take their placeholders' places. A quoter
-- failure (a template error, with a line) is placed on that line. What
-- the quoter's own parser would refuse in a hole (a splice, an arrow) is
-- refused here too, so a module builds the same with the plugin and
-- without it.
expandQuote :: DynFlags -> Quoter -> RdrName -> SrcSpan -> Text -> IO (Either [MsgEnvelope PsMessage] (LHsExpr GhcPs))
expandQuote dflags quoter name at body = do
  kept <- newIORef Map.empty
  let -- A placeholder's stem is not in the quote's text, so no hole holds
      -- it, nor any name that extends it.
      stem = until (\s -> not (T.pack s `T.isInfixOf` body)) (<> "x") "haskellHalogenThHole"
      keep filling = atomicModifyIORef' kept $ \m ->
        let placeholder = stem <> show (Map.size m)
        in (Map.insert placeholder filling m, TH.mkName placeholder)
      env =
        Env
          { expression = \origin src -> runIO (TH.VarE <$> (keep . AnExpression =<< parseHole expression (parseExpAt exts) origin src))
          , -- A variable stays a variable (a let of one is a function binding,
            -- as the quoter makes it); any other pattern hides behind an
            -- as-pattern, so a let of it stays a pattern binding.
            pattern = \origin src -> runIO $ do
              p <- parseHole Parser.parsePattern (parsePatAt exts) origin src
              placeholder <- keep (APattern p)
              pure $ case p of
                L _ (VarPat _ _) -> TH.VarP placeholder
                _ -> TH.AsP placeholder TH.WildP
          , report = \ln what -> runIO (throwIO (Problems [complaint (Complaint (fileLine ln) Nothing what)]))
          , startColumn = startCol
          }
  made <- try (runQ (quoted quoter env (T.unpack body)))
  case made of
    Left (Problems errs) -> pure (Left errs)
    Right code -> do
      fillings <- readIORef kept
      -- The quoter's name is referred to, so the import that brings it is used.
      let whole = TH.LetE [TH.ValD TH.WildP (TH.NormalB (TH.VarE (TH.mkName (rendered name)))) []] code
      pure $ case convert whole of
        Left failure -> Left [envelope at (vcat (unDecorated (diagnosticMessage (defaultDiagnosticOpts @TcRnMessage) (TcRnTHError (THSpliceFailed (RunSpliceFailure failure))))))]
        Right e -> Right (fillHoles fillings e)
  where
    (file, startLine, startCol) = case at of
      RealSrcSpan r _ -> (srcSpanFile r, srcSpanStartLine r, srcSpanStartCol r)
      UnhelpfulSpan _ -> (mkFastString "<quote>", 1, 1)
    fileLine ln = if ln > 0 then startLine + ln - 1 else startLine
    opts = initParserOpts dflags
    exts = namedExtensions (map show (EnumSet.toList (extensionFlags dflags)))

    -- Generated code, as GHC itself treats the result of a splice.
#if MIN_VERSION_ghc(9,14,0)
    convert = convertToHsExpr (extensionFlags dflags) (Generated OtherExpansion SkipPmc) at
#else
    convert = convertToHsExpr (Generated OtherExpansion SkipPmc) at
#endif

    expression = Parser.parseExpression >>= \ecp -> runPV (unECP ecp) :: P (LHsExpr GhcPs)

    -- GHC parses the hole at its place; then the quoter's parser must take
    -- it too, or the module would build here and not where the quoter runs.
    parseHole :: P a -> ((Int, Int) -> Text -> Either Failure b) -> (Int, Int) -> Text -> IO a
    parseHole p quoters (ln, col) src =
      case unP p (initParserState opts (stringToStringBuffer (T.unpack src)) (mkRealSrcLoc file (fileLine ln) col)) of
        PFailed s -> throwIO (Problems (bagToList (getMessages (getPsErrorMessages s))))
        POk _ a -> case quoters (fileLine ln, col) src of
          Left (Failure l c what)
            | l > 0 -> throwIO (Problems [complaint (Complaint l (Just c) what)])
            | otherwise -> throwIO (Problems [complaint (Complaint (fileLine ln) (Just col) what)])
          Right _ -> pure a

    -- The quoter's name as the module wrote it, qualified or not.
    rendered = \case
      Qual m occ -> moduleNameString m <> "." <> occNameString occ
      other -> occNameString (rdrNameOcc other)

    envelope sp doc =
      mkErrorMsgEnvelope sp neverQualify (PsUnknownMessage (mkSimpleUnknownDiagnostic (mkPlainError noHints doc)))

    -- A template error, from the column (or the first word of the line) to
    -- the line's end; the template's first line begins at the quote's column.
    complaint (Complaint ln col what) = envelope (place ln col) (text (T.unpack (quoterName quoter <> ": " <> what)))
    place ln col =
      let t = maybe "" T.stripEnd (listToMaybe (drop (ln - startLine) (T.lines body)))
          offset = if ln == startLine then startCol - 1 else 0
          start = fromMaybe (offset + T.length (T.takeWhile (== ' ') t) + 1) col
          end = max (start + 1) (offset + T.length t + 1)
      in mkSrcSpan (mkSrcLoc file ln start) (mkSrcLoc file ln end)

-- | The holes in the quoter's code take their placeholders' places. Where
-- a generated function is applied to a hole, the application takes the
-- hole's place too, so an instance missing "arising from a use of" that
-- function is reported at the hole.
fillHoles :: Map String Filling -> LHsExpr GhcPs -> LHsExpr GhcPs
fillHoles fillings = everywhere' (mkT expr `extT` pat `extT` bind)
  where
    filling = \case
      Unqual occ -> Map.lookup (occNameString occ) fillings
      _ -> Nothing
    expr :: LHsExpr GhcPs -> LHsExpr GhcPs
    expr = \case
      L _ (HsApp x (L _ f) (L _ (HsVar _ (L _ v))))
        | Just (AnExpression h@(L l _)) <- filling v -> L l (HsApp x (L l f) h)
      L _ (HsVar _ (L _ v))
        | Just (AnExpression h) <- filling v -> h
      e -> e
    pat :: LPat GhcPs -> LPat GhcPs
    pat = \case
      L _ (VarPat _ (L _ v))
        | Just (APattern h) <- filling v -> h
      L _ (AsPat _ (L _ v) _)
        | Just (APattern h) <- filling v -> h
      p -> p
    -- A let of a variable is a function binding of the placeholder.
    bind :: HsBind GhcPs -> HsBind GhcPs
    bind = \case
      b@FunBind {fun_id = L _ v}
        | Just (APattern (L _ (VarPat _ name))) <- filling v -> b {fun_id = name}
      b -> b
