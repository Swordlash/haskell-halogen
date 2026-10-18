{-# LANGUAGE TemplateHaskellQuotes #-}

-- | The Haskell inside a template, parsed by GHC's own parser (through
-- ghc-lib-parser, so the same on every GHC) with the extensions of the
-- module the template is in, and translated to Template Haskell.
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
  )
where

import Data.ByteString qualified as BS
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (intercalate)
import GHC.Data.Bag (bagToList)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type qualified as Ext
import GHC.Parser qualified as Parser
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Types.Error (Diagnostic (..), Messages, MsgEnvelope (..), NoDiagnosticOpts (..), getMessages, unDecorated)
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), initParserState, mkParserOpts)
import GHC.Parser.PostProcess (runPV, unECP)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (isDataOcc, occNameString)
import GHC.Types.Name.Reader (RdrName (..), isRdrTyVar, rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..), rationalFromFractionalLit)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..), mkRealSrcLoc, psRealSpan, realSrcSpanStart, srcLocCol, srcLocLine, unLoc)
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
parseExpAt exts origin src = do
  e <- run exts origin (Parser.parseExpression >>= \ecp -> (runPV (unECP ecp) :: P (LHsExpr GhcPs))) src
  translation (expr (unLoc e))

parsePatAt :: [TH.Extension] -> (Int, Int) -> Text -> Either Failure TH.Pat
parsePatAt exts origin src = do
  p <- run exts origin Parser.parsePattern src
  translation (pat (unLoc p))

translation :: Either Text a -> Either Failure a
translation = either (Left . Failure 0 0) Right

run :: [TH.Extension] -> (Int, Int) -> P a -> Text -> Either Failure a
run exts (line, column) p src = case unP p st of
  POk _ a -> Right a
  PFailed s ->
    -- The failure is where GHC's message puts it (a lexical error is inside
    -- its token), else at the last token read.
    let l = case [sp | MsgEnvelope {errMsgSpan = RealSrcSpan sp _} <- bagToList (getMessages s.errors)] of
          sp : _ -> realSrcSpanStart sp
          [] -> realSrcSpanStart (psRealSpan s.last_loc)
    in Left (Failure (srcLocLine l) (srcLocCol l) (T.pack (messages s.errors)))
  where
    opts = mkParserOpts (extensions exts) emptyDiagOpts False False False False
    st = initParserState opts (stringToStringBuffer (T.unpack src)) (mkRealSrcLoc (mkFastString "<template>") line column)

-- | The module's extensions, by name: Template Haskell and ghc-lib-parser
-- each have the type.
extensions :: [TH.Extension] -> EnumSet.EnumSet Ext.Extension
extensions = EnumSet.fromList . mapMaybe ((`Map.lookup` byName) . show)
  where
    byName = Map.fromList [(show e, e) | e <- [minBound .. maxBound]]

render :: (Outputable a) => a -> String
render = renderWithContext defaultSDocContext . ppr

-- | The parser's messages, as GHC would print them, without the position
-- (the caller places it in the template).
messages :: Messages PsMessage -> String
messages msgs =
  intercalate "; "
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
  HsMultiIf _ alts -> TH.MultiIfE <$> mapM guarded (NE.toList alts)
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
  ArithSeq _ _ info -> TH.ArithSeqE <$> case info of
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
  e@HsHole {} -> unsupported "a hole" e
  e@HsForAll {} -> unsupported "a type as an expression" e
  e@HsQual {} -> unsupported "a type as an expression" e
  e@HsFunArr {} -> unsupported "a type as an expression" e
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
      [Match {m_pats = L _ ps, m_grhss = GRHSs _ (L _ (GRHS _ [] body) :| []) (EmptyLocalBinds _)}] ->
        TH.LamE <$> mapM lpat ps <*> lexpr body
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
    body <- case NE.toList rhss of
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
  ParStmt _ blocks _ _ -> TH.ParS <$> mapM block (NE.toList blocks)
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
  ConPat {pat_con = L _ con, pat_args = details} -> case details of
    PrefixCon args -> do
      let (tys, ps) = partitionEithers (map typeArg args)
      TH.ConP (varName con) <$> mapM ltype tys <*> mapM lpat ps
    RecCon (HsRecFields _ flds dotdot) -> case dotdot of
      Just _ -> Left "a record wildcard (..) is not supported in a template"
      Nothing -> TH.RecP (varName con) <$> mapM field flds
    InfixCon a b -> (\a' b' -> TH.UInfixP a' (varName con) b') <$> lpat a <*> lpat b
  ViewPat _ e p -> TH.ViewP <$> lexpr e <*> lpat p
  LitPat _ l -> TH.LitP <$> lit l
  NPat _ (L _ (OverLit _ v)) neg _ -> Right (TH.LitP (maybe id (const negated) neg (overLit v)))
  SigPat _ p (HsPS _ t) -> TH.SigP <$> lpat p <*> ltype t
  EmbTyPat _ (HsTP _ t) -> TH.TypeP <$> ltype t
  InvisPat _ (HsTP _ t) -> TH.InvisP <$> ltype t
  p@SplicePat {} -> unsupported "a splice" p
  p@NPlusKPat {} -> unsupported "an n+k pattern" p
  p@OrPat {} -> unsupported "an or-pattern" p
  where
    typeArg lp = case unLoc lp of
      InvisPat _ (HsTP _ t) -> Left t
      _ -> Right lp
    field (L _ (HsFieldBind _ (L _ f) p _)) = (,) <$> fieldName f <*> lpat p

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
  HsFunTy _ mult a b -> case mult of
    HsUnannotated _ -> (\a' b' -> TH.AppT (TH.AppT TH.ArrowT a') b') <$> ltype a <*> ltype b
    _ -> Left "a linear arrow is not supported in a template"
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
  HsTyLit _ l -> TH.LitT <$> case l of
    HsNumTy _ n -> Right (TH.NumTyLit n)
    HsStrTy _ fs -> Right (TH.StrTyLit (unpackFS fs))
    HsCharTy _ c -> Right (TH.CharTyLit c)
  HsWildCardTy _ -> Right TH.WildCardT
  t@HsSpliceTy {} -> unsupported "a splice" t
  t@HsDocTy {} -> unsupported "a documented type" t
  t@XHsType {} -> unsupported "this type" t

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
