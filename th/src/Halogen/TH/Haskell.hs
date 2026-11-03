{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | The Haskell inside a template, parsed by GHC's own parser with the
-- extensions of the module the template is in, and translated to Template
-- Haskell. The parser is the @ghc@ library of the compiler that compiles
-- the module, so a quote is read exactly as the module around it: the same
-- grammar, the same extensions, the same messages.
--
-- The translation covers what an expression in a template is: variables,
-- literals, application, operators (left for GHC to associate), sections,
-- tuples, lists and ranges, lambdas and @\\case@, @if@, @case@ with guards,
-- @let@ and @where@ with signatures, @do@ and comprehensions, records
-- (construction, update, dot access), type applications and annotations.
-- What is not a template's business (splices, quotes, arrows, linear
-- arrows, pattern synonyms, implicit parameters) is refused with a message.
module Halogen.TH.Haskell
  ( parseExp
  , parseExpAt
  , parsePat
  , parsePatAt
  , Failure (..)
  , namedExtensions

    -- * GHC's tree too
  , parseHsExpAt
  , parseHsPatAt
  )
where

import Data.ByteString qualified as BS
#if MIN_VERSION_ghc(9,14,0)
import Data.Either (partitionEithers)
#endif
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.Bag (bagToList)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.Parser qualified as Parser
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), ParserOpts, getPsErrorMessages, initParserState, mkParserOpts)
import GHC.Parser.PostProcess (runPV, unECP)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Error (Diagnostic (..), Messages, MsgEnvelope (..), NoDiagnosticOpts (..), getMessages, unDecorated)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (isDataOcc, occNameString)
import GHC.Types.Name.Reader (RdrName (..), isRdrTyVar, rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..), rationalFromFractionalLit)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcLoc, SrcSpan (..), mkRealSrcLoc, psRealSpan, realSrcSpanStart, srcLocCol, srcLocLine, unLoc)
import GHC.Types.Var (Specificity (..))
import GHC.Utils.Error (emptyDiagOpts)
import GHC.Utils.Outputable (Outputable, defaultSDocContext, ppr, renderWithContext)
import Language.Haskell.Syntax.Basic (FieldLabelString (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as THS

-- | Where and why parsing failed: the line and column, counted on from
-- where the text was said to start (0 0: not a position), and the message.
data Failure = Failure Int Int Text
  deriving (Eq, Show)

parseExp :: [TH.Extension] -> Text -> Either Failure TH.Exp
parseExp exts = parseExpAt exts (1, 1)

parsePat :: [TH.Extension] -> Text -> Either Failure TH.Pat
parsePat exts = parsePatAt exts (1, 1)

-- | Parse text that starts at a line and column of its file, so a
-- failure is placed in the file.
parseExpAt :: [TH.Extension] -> (Int, Int) -> Text -> Either Failure TH.Exp
parseExpAt exts origin src = run exts origin expression src >>= translation . lexpr

parsePatAt :: [TH.Extension] -> (Int, Int) -> Text -> Either Failure TH.Pat
parsePatAt exts origin src = run exts origin Parser.parsePattern src >>= translation . lpat

-- | An expression parsed with the caller's parser options from a place in
-- a file, for a caller that wants GHC's tree as well as what the quoter
-- would make of it: the parser's errors, as GHC reports them, or the tree
-- and the translation's verdict, the failure the quoter would report for
-- it (a splice, an arrow: what a template does not take) or none. The
-- compiler plugin of haskell-halogen-th-plugin puts the tree in the
-- module, so it parses each hole once and still refuses what the quoter
-- would.
parseHsExpAt :: ParserOpts -> RealSrcLoc -> Text -> Either (Messages PsMessage) (LHsExpr GhcPs, Maybe Failure)
parseHsExpAt opts at src = (\e -> (e, verdict (lexpr e))) <$> parseWith opts at expression src

-- | A pattern, as 'parseHsExpAt' parses an expression.
parseHsPatAt :: ParserOpts -> RealSrcLoc -> Text -> Either (Messages PsMessage) (LPat GhcPs, Maybe Failure)
parseHsPatAt opts at src = (\p -> (p, verdict (lpat p))) <$> parseWith opts at Parser.parsePattern src

expression :: P (LHsExpr GhcPs)
expression = Parser.parseExpression >>= \ecp -> runPV (unECP ecp)

translation :: Either Text a -> Either Failure a
translation = either (Left . Failure 0 0) Right

verdict :: Either Text a -> Maybe Failure
verdict = either (Just . Failure 0 0) (const Nothing)

-- | The parser run on the text from a place, to its result or to the
-- parser's state at the failure, which has the errors and the last token.
parseState :: ParserOpts -> RealSrcLoc -> P a -> Text -> Either PState a
parseState opts at p src = case unP p (initParserState opts (stringToStringBuffer (T.unpack src)) at) of
  POk _ a -> Right a
  PFailed s -> Left s

parseWith :: ParserOpts -> RealSrcLoc -> P a -> Text -> Either (Messages PsMessage) a
parseWith opts at p src = either (Left . getPsErrorMessages) Right (parseState opts at p src)

run :: [TH.Extension] -> (Int, Int) -> P a -> Text -> Either Failure a
run exts (line, column) p src = case parseState opts (mkRealSrcLoc (mkFastString "<template>") line column) p src of
  Right a -> Right a
  Left s ->
    -- The failure is where GHC's message puts it (a lexical error is inside
    -- its token), else at the last token read.
    let l = case [sp | MsgEnvelope {errMsgSpan = RealSrcSpan sp _} <- bagToList (getMessages s.errors)] of
          sp : _ -> realSrcSpanStart sp
          [] -> realSrcSpanStart (psRealSpan s.last_loc)
     in Left (Failure (srcLocLine l) (srcLocCol l) (T.pack (messages s.errors)))
  where
    opts = parserOpts (extensions exts)

-- | The parser's options for a template: its extensions, and no warnings,
-- Haddock comments or token stream kept.
parserOpts :: EnumSet.EnumSet TH.Extension -> ParserOpts
#if MIN_VERSION_ghc(9,14,0)
parserOpts exts = mkParserOpts exts emptyDiagOpts False False False False
#else
-- Before 9.14 the options also listed the extensions GHC supports, for the
-- message about an unknown one in a LANGUAGE pragma, which a template has
-- none of.
parserOpts exts = mkParserOpts exts emptyDiagOpts [] False False False False
#endif

-- | Extensions by their names, for a caller that has GHC's own: the
-- names are the same.
namedExtensions :: [String] -> [TH.Extension]
namedExtensions names = filter ((`Set.member` wanted) . show) [minBound .. maxBound]
  where
    wanted = Set.fromList names

-- | The module's extensions as the parser takes them: Template Haskell's
-- type is the compiler's own.
extensions :: [TH.Extension] -> EnumSet.EnumSet TH.Extension
extensions = EnumSet.fromList

render :: (Outputable a) => a -> String
render = renderWithContext defaultSDocContext . ppr

-- | The parser's messages, as GHC would print them, without the position
-- (the caller places it in the template).
messages :: Messages PsMessage -> String
messages msgs =
  intercalate
    "; "
    [ render d
    | envelope <- bagToList (getMessages msgs)
    , d <- unDecorated (diagnosticMessage NoDiagnosticOpts envelope.errMsgDiagnostic)
    ]

unsupported :: (Outputable a) => Text -> a -> Either Text b
unsupported what x = Left (what <> " is not supported in a template: " <> T.pack (render x))

-- Names ----------------------------------------------------------------------

rdrString :: RdrName -> String
rdrString = \case
  Unqual occ -> occNameString occ
  Qual m occ -> moduleNameString m <> "." <> occNameString occ
  Orig _ occ -> occNameString occ
  Exact n -> occNameString (nameOccName n)

varName :: RdrName -> TH.Name
varName = TH.mkName . rdrString

-- | A variable or constructor as an expression, with the built-in
-- constructors by their own names.
nameExp :: RdrName -> TH.Exp
nameExp n = case rdrString n of
  "()" -> TH.ConE '()
  "[]" -> TH.ConE '[]
  ":" -> TH.ConE '(:)
  s
    | Just arity <- tupleArity s -> TH.ConE (TH.tupleDataName arity)
    | isDataOcc (rdrNameOcc n) -> TH.ConE (TH.mkName s)
    | otherwise -> TH.VarE (TH.mkName s)

tupleArity :: String -> Maybe Int
tupleArity s = case s of
  '(' : rest | not (null rest), last rest == ')', all (== ',') (init rest) -> Just (length rest)
  _ -> Nothing

-- Expressions ----------------------------------------------------------------

lexpr :: LHsExpr GhcPs -> Either Text TH.Exp
lexpr = expr . unLoc

expr :: HsExpr GhcPs -> Either Text TH.Exp
expr = \case
  HsVar _ (L _ n) -> Right (nameExp n)
  HsOverLabel _ fs -> Right (TH.LabelE (unpackFS fs))
  HsIPVar _ (HsIPName fs) -> Right (TH.ImplicitParamVarE (unpackFS fs))
  HsOverLit _ (OverLit _ v) -> Right (TH.LitE (overLit v))
  HsLit _ l -> TH.LitE <$> lit l
  HsLam _ variant mg -> lambda variant mg
  HsApp _ f a -> TH.AppE <$> lexpr f <*> lexpr a
  HsAppType _ e (HsWC _ t) -> TH.AppTypeE <$> lexpr e <*> ltype t
  OpApp _ a op b -> TH.UInfixE <$> lexpr a <*> lexpr op <*> lexpr b
  NegApp _ e _ -> TH.AppE (TH.VarE 'negate) <$> lexpr e
  HsPar _ e -> TH.ParensE <$> lexpr e
  SectionL _ a op -> (\a' op' -> TH.InfixE (Just a') op' Nothing) <$> lexpr a <*> lexpr op
  SectionR _ op b -> (\op' b' -> TH.InfixE Nothing op' (Just b')) <$> lexpr op <*> lexpr b
  ExplicitTuple _ args boxity -> (if boxity == Boxed then TH.TupE else TH.UnboxedTupE) <$> mapM tupArg args
  ExplicitSum _ alt arity e -> (\e' -> TH.UnboxedSumE e' alt arity) <$> lexpr e
  HsCase _ e mg -> TH.CaseE <$> lexpr e <*> matches mg
  HsIf _ c t e -> TH.CondE <$> lexpr c <*> lexpr t <*> lexpr e
  HsMultiIf _ alts -> TH.MultiIfE <$> mapM guarded (toList alts)
  HsLet _ binds e -> TH.LetE <$> localBinds binds <*> lexpr e
  HsDo _ flavour (L _ stmts) -> case flavour of
    DoExpr m -> TH.DoE (modName <$> m) <$> mapM lstmt stmts
    MDoExpr m -> TH.MDoE (modName <$> m) <$> mapM lstmt stmts
    ListComp -> TH.CompE <$> mapM lstmt stmts
    MonadComp -> TH.CompE <$> mapM lstmt stmts
    GhciStmtCtxt -> Left "a GHCi statement"
  ExplicitList _ es -> TH.ListE <$> mapM lexpr es
  RecordCon _ (L _ con) (HsRecFields _ flds dotdot) -> case dotdot of
    Just _ -> Left "a record wildcard (..) is not supported in a template"
    Nothing -> TH.RecConE (varName con) <$> mapM recField flds
  RecordUpd _ e flds -> case flds of
    RegularRecUpdFields _ fs -> TH.RecUpdE <$> lexpr e <*> mapM updField fs
    OverloadedRecUpdFields {} -> Left "a record update through a dotted field is not supported in a template"
  HsGetField _ e (L _ f) -> TH.GetFieldE <$> lexpr e <*> dotField f
  HsProjection _ fs -> TH.ProjectionE <$> mapM dotField fs
  ExprWithTySig _ e (HsWC _ (L _ sig)) -> TH.SigE <$> lexpr e <*> sigType sig
  ArithSeq _ _ info ->
    TH.ArithSeqE <$> case info of
      From a -> TH.FromR <$> lexpr a
      FromThen a b -> TH.FromThenR <$> lexpr a <*> lexpr b
      FromTo a b -> TH.FromToR <$> lexpr a <*> lexpr b
      FromThenTo a b c -> TH.FromThenToR <$> lexpr a <*> lexpr b <*> lexpr c
  HsStatic _ e -> TH.StaticE <$> lexpr e
  HsPragE _ _ e -> lexpr e
  HsEmbTy _ (HsWC _ t) -> TH.TypeE <$> ltype t
  e@HsTypedBracket {} -> unsupported "a quote" e
  e@HsUntypedBracket {} -> unsupported "a quote" e
  e@HsTypedSplice {} -> unsupported "a splice" e
  e@HsUntypedSplice {} -> unsupported "a splice" e
  e@HsProc {} -> unsupported "an arrow" e
  e@HsForAll {} -> unsupported "a type as an expression" e
  e@HsQual {} -> unsupported "a type as an expression" e
  e@HsFunArr {} -> unsupported "a type as an expression" e
  -- What is left is a hole, @_@: HsHole since GHC 9.14, HsUnboundVar
  -- before.
  e -> unsupported "a hole" e
  where
    modName = THS.ModName . moduleNameString

tupArg :: HsTupArg GhcPs -> Either Text (Maybe TH.Exp)
tupArg = \case
  Present _ e -> Just <$> lexpr e
  Missing _ -> Right Nothing

dotField :: DotFieldOcc GhcPs -> Either Text String
dotField = \case
  DotFieldOcc _ (L _ (FieldLabelString fs)) -> Right (unpackFS fs)

recField :: LHsRecField GhcPs (LHsExpr GhcPs) -> Either Text TH.FieldExp
recField (L _ (HsFieldBind _ (L _ f) rhs _)) = (,) <$> fieldName f <*> lexpr rhs

updField :: LHsRecUpdField GhcPs GhcPs -> Either Text TH.FieldExp
updField (L _ (HsFieldBind _ (L _ f) rhs _)) = (,) <$> fieldName f <*> lexpr rhs

fieldName :: FieldOcc GhcPs -> Either Text TH.Name
fieldName = \case
  FieldOcc _ (L _ n) -> Right (varName n)

lambda :: HsLamVariant -> MatchGroup GhcPs (LHsExpr GhcPs) -> Either Text TH.Exp
lambda variant mg = case variant of
  LamSingle -> do
    ms <- matchList mg
    case ms of
      [Match {m_pats = L _ ps, m_grhss = GRHSs _ rhss (EmptyLocalBinds _)}]
        | [L _ (GRHS _ [] body)] <- toList rhss -> TH.LamE <$> mapM lpat ps <*> lexpr body
      _ -> Left "a lambda with guards or a where"
  LamCase -> TH.LamCaseE <$> matches mg
  LamCases -> TH.LamCasesE <$> clauses mg

matchList :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either Text [Match GhcPs (LHsExpr GhcPs)]
matchList = \case
  MG _ (L _ alts) -> Right (map unLoc alts)

-- | Alternatives of one pattern each: a case, a @\\case@.
matches :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either Text [TH.Match]
matches mg = matchList mg >>= mapM one
  where
    one = \case
      Match {m_pats = L _ [p], m_grhss = rhs} -> do
        (body, decs) <- grhss rhs
        p' <- lpat p
        Right (TH.Match p' body decs)
      m -> unsupported "an alternative with several patterns" m

-- | Alternatives of any number of patterns: a function, a @\\cases@.
clauses :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either Text [TH.Clause]
clauses mg = matchList mg >>= mapM one
  where
    one = \case
      Match {m_pats = L _ ps, m_grhss = rhs} -> do
        (body, decs) <- grhss rhs
        ps' <- mapM lpat ps
        Right (TH.Clause ps' body decs)

grhss :: GRHSs GhcPs (LHsExpr GhcPs) -> Either Text (TH.Body, [TH.Dec])
grhss = \case
  GRHSs _ rhss binds -> do
    decs <- localBinds binds
    body <- case toList rhss of
      [L _ (GRHS _ [] e)] -> TH.NormalB <$> lexpr e
      alts -> TH.GuardedB <$> mapM guarded alts
    Right (body, decs)

guarded :: LGRHS GhcPs (LHsExpr GhcPs) -> Either Text (TH.Guard, TH.Exp)
guarded (L _ g) = case g of
  GRHS _ guards e -> (,) <$> guardOf guards <*> lexpr e
  where
    guardOf = \case
      [L _ (BodyStmt _ c _ _)] -> TH.NormalG <$> lexpr c
      stmts -> TH.PatG <$> mapM lstmt stmts

localBinds :: HsLocalBinds GhcPs -> Either Text [TH.Dec]
localBinds = \case
  EmptyLocalBinds _ -> Right []
  HsValBinds _ (ValBinds _ binds sigs) -> (<>) <$> (concat <$> mapM sig sigs) <*> mapM bind binds
  HsValBinds _ (XValBindsLR _) -> Left "renamed bindings"
  b@HsIPBinds {} -> unsupported "an implicit parameter binding" b
  where
    bind (L _ b) = case b of
      FunBind {fun_id = L _ n, fun_matches = mg} -> TH.FunD (varName n) <$> clauses mg
      PatBind {pat_lhs = p, pat_rhs = rhs} -> do
        (body, decs) <- grhss rhs
        p' <- lpat p
        Right (TH.ValD p' body decs)
      VarBind {} -> unsupported "this binding" b
      PatSynBind {} -> unsupported "a pattern synonym" b
    sig (L _ s) = case s of
      TypeSig _ names (HsWC _ (L _ t)) -> do
        t' <- sigType t
        Right [TH.SigD (varName n) t' | L _ n <- names]
      _ -> unsupported "a signature that is not a type signature" s

lstmt :: LStmt GhcPs (LHsExpr GhcPs) -> Either Text TH.Stmt
lstmt (L _ s) = case s of
  BindStmt _ p e -> TH.BindS <$> lpat p <*> lexpr e
  BodyStmt _ e _ _ -> TH.NoBindS <$> lexpr e
  LetStmt _ binds -> TH.LetS <$> localBinds binds
  LastStmt _ e _ _ -> TH.NoBindS <$> lexpr e
  ParStmt _ blocks _ _ -> TH.ParS <$> mapM block (toList blocks)
  RecStmt {recS_stmts = L _ stmts} -> TH.RecS <$> mapM lstmt stmts
  TransStmt {} -> unsupported "a transform comprehension" s
  where
    block = \case
      ParStmtBlock _ stmts _ _ -> mapM lstmt stmts
      XParStmtBlock {} -> Left "a parallel block of no known form"

-- Literals --------------------------------------------------------------------

lit :: HsLit GhcPs -> Either Text TH.Lit
lit = \case
  HsChar _ c -> Right (TH.CharL c)
  HsCharPrim _ c -> Right (TH.CharPrimL c)
  HsString _ fs -> Right (TH.StringL (unpackFS fs))
  HsMultilineString _ fs -> Right (TH.StringL (unpackFS fs))
  HsStringPrim _ bs -> Right (TH.StringPrimL (BS.unpack bs))
  HsInt _ il -> Right (TH.IntegerL il.il_value)
  HsIntPrim _ i -> Right (TH.IntPrimL i)
  HsWordPrim _ i -> Right (TH.WordPrimL i)
  HsFloatPrim _ fl -> Right (TH.FloatPrimL (rationalFromFractionalLit fl))
  HsDoublePrim _ fl -> Right (TH.DoublePrimL (rationalFromFractionalLit fl))
  l -> unsupported "a sized primitive literal" l

overLit :: OverLitVal -> TH.Lit
overLit = \case
  HsIntegral il -> TH.IntegerL il.il_value
  HsFractional fl -> TH.RationalL (rationalFromFractionalLit fl)
  HsIsString _ fs -> TH.StringL (unpackFS fs)

negated :: TH.Lit -> TH.Lit
negated = \case
  TH.IntegerL n -> TH.IntegerL (negate n)
  TH.RationalL r -> TH.RationalL (negate r)
  l -> l

-- Patterns -------------------------------------------------------------------

lpat :: LPat GhcPs -> Either Text TH.Pat
lpat = pat . unLoc

pat :: Pat GhcPs -> Either Text TH.Pat
pat = \case
  WildPat _ -> Right TH.WildP
  VarPat _ (L _ n) -> Right (TH.VarP (varName n))
  LazyPat _ p -> TH.TildeP <$> lpat p
  AsPat _ (L _ n) p -> TH.AsP (varName n) <$> lpat p
  ParPat _ p -> TH.ParensP <$> lpat p
  BangPat _ p -> TH.BangP <$> lpat p
  ListPat _ ps -> TH.ListP <$> mapM lpat ps
  TuplePat _ ps boxity -> (if boxity == Boxed then TH.TupP else TH.UnboxedTupP) <$> mapM lpat ps
  SumPat _ p alt arity -> (\p' -> TH.UnboxedSumP p' alt arity) <$> lpat p
  ConPat {pat_con = L _ con, pat_args = details} -> conPat con details
  ViewPat _ e p -> TH.ViewP <$> lexpr e <*> lpat p
  LitPat _ l -> TH.LitP <$> lit l
  NPat _ (L _ (OverLit _ v)) neg _ -> Right (TH.LitP (maybe id (const negated) neg (overLit v)))
  SigPat _ p (HsPS _ t) -> TH.SigP <$> lpat p <*> ltype t
  EmbTyPat _ (HsTP _ t) -> TH.TypeP <$> ltype t
  InvisPat _ (HsTP _ t) -> TH.InvisP <$> ltype t
  p@SplicePat {} -> unsupported "a splice" p
  p@NPlusKPat {} -> unsupported "an n+k pattern" p
  p@OrPat {} -> unsupported "an or-pattern" p

-- | A constructor and its arguments.
conPat :: RdrName -> HsConPatDetails GhcPs -> Either Text TH.Pat
#if MIN_VERSION_ghc(9,14,0)
conPat con = \case
  PrefixCon args -> do
    let (tys, ps) = partitionEithers (map typeArg args)
    TH.ConP (varName con) <$> mapM ltype tys <*> mapM lpat ps
  RecCon r -> recPat con r
  InfixCon a b -> infixPat con a b
  where
    -- Since 9.14 a constructor's type arguments are among its patterns.
    typeArg lp = case unLoc lp of
      InvisPat _ (HsTP _ t) -> Left t
      _ -> Right lp
#else
conPat con = \case
  PrefixCon tys ps -> TH.ConP (varName con) <$> mapM (\(HsConPatTyArg _ (HsTP _ t)) -> ltype t) tys <*> mapM lpat ps
  RecCon r -> recPat con r
  InfixCon a b -> infixPat con a b
#endif

recPat :: RdrName -> HsRecFields GhcPs (LPat GhcPs) -> Either Text TH.Pat
recPat con (HsRecFields _ flds dotdot) = case dotdot of
  Just _ -> Left "a record wildcard (..) is not supported in a template"
  Nothing -> TH.RecP (varName con) <$> mapM field flds
  where
    field (L _ (HsFieldBind _ (L _ f) p _)) = (,) <$> fieldName f <*> lpat p

infixPat :: RdrName -> LPat GhcPs -> LPat GhcPs -> Either Text TH.Pat
infixPat con a b = (\a' b' -> TH.UInfixP a' (varName con) b') <$> lpat a <*> lpat b

-- Types ----------------------------------------------------------------------

ltype :: LHsType GhcPs -> Either Text TH.Type
ltype = type_ . unLoc

type_ :: HsType GhcPs -> Either Text TH.Type
type_ = \case
  HsForAllTy _ tele body -> do
    b <- ltype body
    case tele of
      HsForAllInvis _ bndrs -> (\bs -> TH.ForallT bs [] b) <$> mapM (binder specificity) bndrs
      HsForAllVis _ bndrs -> (`TH.ForallVisT` b) <$> mapM (binder (const ())) bndrs
  HsQualTy _ (L _ ctx) body -> TH.ForallT [] <$> mapM ltype ctx <*> ltype body
  HsTyVar _ promoted (L _ n) -> Right $ case promoted of
    IsPromoted -> TH.PromotedT (varName n)
    NotPromoted
      | isRdrTyVar n -> TH.VarT (varName n)
      | otherwise -> TH.ConT (varName n)
  HsAppTy _ a b -> TH.AppT <$> ltype a <*> ltype b
  HsAppKindTy _ a k -> TH.AppKindT <$> ltype a <*> ltype k
  HsFunTy _ mult a b
    | unrestricted mult -> (\a' b' -> TH.AppT (TH.AppT TH.ArrowT a') b') <$> ltype a <*> ltype b
    | otherwise -> Left "a linear arrow is not supported in a template"
  HsListTy _ a -> TH.AppT TH.ListT <$> ltype a
  HsTupleTy _ sort ts -> do
    ts' <- mapM ltype ts
    let con = case sort of
          HsUnboxedTuple -> TH.UnboxedTupleT
          HsBoxedOrConstraintTuple -> TH.TupleT
    Right (foldl TH.AppT (con (length ts')) ts')
  HsSumTy _ ts -> do
    ts' <- mapM ltype ts
    Right (foldl TH.AppT (TH.UnboxedSumT (length ts')) ts')
  HsOpTy _ promoted a (L _ op) b -> (\a' b' -> (if promoted == IsPromoted then TH.PromotedInfixT else TH.InfixT) a' (varName op) b') <$> ltype a <*> ltype b
  HsParTy _ t -> TH.ParensT <$> ltype t
  HsIParamTy _ (L _ (HsIPName fs)) t -> TH.ImplicitParamT (unpackFS fs) <$> ltype t
  HsStarTy _ _ -> Right TH.StarT
  HsKindSig _ t k -> TH.SigT <$> ltype t <*> ltype k
  HsExplicitListTy _ _ ts -> foldr (\t acc -> TH.AppT (TH.AppT TH.PromotedConsT t) acc) TH.PromotedNilT <$> mapM ltype ts
  HsExplicitTupleTy _ _ ts -> do
    ts' <- mapM ltype ts
    Right (foldl TH.AppT (TH.PromotedTupleT (length ts')) ts')
  HsTyLit _ l ->
    TH.LitT <$> case l of
      HsNumTy _ n -> Right (TH.NumTyLit n)
      HsStrTy _ fs -> Right (TH.StrTyLit (unpackFS fs))
      HsCharTy _ c -> Right (TH.CharTyLit c)
  HsWildCardTy _ -> Right TH.WildCardT
  t@HsSpliceTy {} -> unsupported "a splice" t
  t@HsDocTy {} -> unsupported "a documented type" t
  -- The rest: XHsType, and before GHC 9.14 HsBangTy and HsRecTy.
  t -> unsupported "this type" t

-- | An arrow written @->@, with no multiplicity.
#if MIN_VERSION_ghc(9,14,0)
unrestricted :: HsMultAnn GhcPs -> Bool
unrestricted = \case
  HsUnannotated _ -> True
  _ -> False
#else
unrestricted :: HsArrow GhcPs -> Bool
unrestricted = \case
  HsUnrestrictedArrow _ -> True
  _ -> False
#endif

binder :: (flag -> flag') -> LHsTyVarBndr flag GhcPs -> Either Text (TH.TyVarBndr flag')
binder flagOf (L _ b) = case b of
  HsTvb _ flag var kind -> do
    n <- case var of
      HsBndrVar _ (L _ n) -> Right (varName n)
      HsBndrWildCard _ -> Left "a wildcard binder is not supported in a template"
    case kind of
      HsBndrNoKind _ -> Right (TH.PlainTV n (flagOf flag))
      HsBndrKind _ k -> TH.KindedTV n (flagOf flag) <$> ltype k

specificity :: Specificity -> THS.Specificity
specificity = \case
  SpecifiedSpec -> THS.SpecifiedSpec
  InferredSpec -> THS.InferredSpec

-- | A signature's type, with its own forall if written.
sigType :: HsSigType GhcPs -> Either Text TH.Type
sigType = \case
  HsSig _ bndrs body -> case bndrs of
    HsOuterImplicit _ -> ltype body
    HsOuterExplicit _ bs -> TH.ForallT <$> mapM (binder specificity) bs <*> pure [] <*> ltype body
