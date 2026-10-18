{-# LANGUAGE CPP #-}

-- | A compiler plugin that puts the errors of @hh@, @hhs@ and @clay@
-- quotes where they are. GHC reports an error inside a quasi-quote at the
-- quote; this plugin reads every quote of those three quoters as the
-- module is parsed, runs the quoter's own checks on it
-- ("Halogen.TH.Check") and reports what they find at its line and column
-- in the file, among the parser's errors. A quote that passes is left to
-- its quoter.
--
-- @ghc-options: -fplugin=Halogen.TH.Plugin@ in a cabal stanza, or
-- @{-\# OPTIONS_GHC -fplugin=Halogen.TH.Plugin \#-}@ in a module, with
-- @haskell-halogen-th-plugin@ among the dependencies. A language server
-- that builds on GHC shows the errors there too. The quoters are known by
-- their names, so another @hh@ in scope would be read as this one.
module Halogen.TH.Plugin (plugin) where

import Data.Generics (everything, mkQ)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Hs (GhcPs, HsModule, HsParsedModule (..), HsUntypedSplice (..))
import GHC.Parser.Annotation (locA)
import GHC.Parser.Errors.Types (PsMessage (PsUnknownMessage))
import GHC.Plugins hiding (quotes, (<>))
import GHC.Types.Error (addMessage, mkPlainError, mkSimpleUnknownDiagnostic, noHints)
import GHC.Utils.Error (mkErrorMsgEnvelope)
import Halogen.TH.Check (Complaint (Complaint), Quoter, check, quoterName, quoterNamed)
import Halogen.TH.Haskell (namedExtensions)

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = \_ summary result -> liftIO (checked summary result)
    , pluginRecompile = purePlugin
    }

-- | A quasi-quote of one of the three quoters: which, where its text is,
-- and the text.
data Quote = Quote Quoter RealSrcSpan Text

-- | The module's quotes checked; what they get wrong joins the parser's
-- errors, so the module stops here, with every error at its place.
checked :: ModSummary -> ParsedResult -> IO ParsedResult
checked summary result = do
  complaints <- concat <$> mapM complaintsOf (quotes (unLoc (hpm_module (parsedResultModule result))))
  let messages = parsedResultMessages result
  pure result {parsedResultMessages = messages {psErrors = foldr addMessage (psErrors messages) complaints}}
  where
    exts = namedExtensions (map show (EnumSet.toList (extensionFlags (ms_hspp_opts summary))))

    complaintsOf (Quote quoter at body) = do
      found <- check exts quoter (startLine, srcSpanStartCol at) body
      pure [envelope c | Just c <- [found]]
      where
        file = srcSpanFile at
        startLine = srcSpanStartLine at
        envelope (Complaint ln col what) =
          mkErrorMsgEnvelope (place ln col) neverQualify $
            PsUnknownMessage (mkSimpleUnknownDiagnostic (mkPlainError noHints (text (T.unpack (quoterName quoter <> ": " <> what)))))
        -- From the column (or the first word of the line) to the line's
        -- end; the template's first line begins at the quote's column.
        place ln col =
          let t = maybe "" T.stripEnd (listToMaybe (drop (ln - startLine) (T.lines body)))
              offset = if ln == startLine then srcSpanStartCol at - 1 else 0
              start = fromMaybe (offset + T.length (T.takeWhile (== ' ') t) + 1) col
              end = max (start + 1) (offset + T.length t + 1)
          in mkSrcSpan (mkSrcLoc file ln start) (mkSrcLoc file ln end)

quotes :: HsModule GhcPs -> [Quote]
quotes = everything (<>) ([] `mkQ` quoted)
  where
    quoted :: HsUntypedSplice GhcPs -> [Quote]
    quoted = \case
      HsQuasiQuote _ quoter body
        | Just q <- quoterNamed (T.pack (occNameString (rdrNameOcc (quoterOf quoter))))
        , RealSrcSpan at _ <- locA body ->
            [Quote q at (T.pack (unpackFS (unLoc body)))]
      _ -> []
#if MIN_VERSION_ghc(9,14,0)
    quoterOf = unLoc
#else
    quoterOf = id
#endif
