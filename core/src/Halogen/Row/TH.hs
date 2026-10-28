{-# LANGUAGE TemplateHaskell #-}

-- | Halogen.Row.TH writes row types of row-types as sorted lists of
-- labels, at compile time.
--
-- row-types builds rows with the type families '.+' and '.=='. A type
-- synonym whose row is built with them is expanded at every place it is
-- used, and the type checker reduces the type families again there. The
-- element rows of "DOM.HTML.Indexed" are chains of about a dozen groups of
-- attributes. Each @HH.div@ paid for reducing its chain: about 70 ms of type
-- checking for an element without properties. A row written as a list,
-- @R '[ "class" :-> Text, ... ]@, has no type family to reduce, so the same
-- element type-checks in about a millisecond. 'flatRows' writes those lists
-- from the definitions with the type families, so the definitions stay as
-- readable as before.
module Halogen.Row.TH (flatRows, flatRow) where

import Control.Monad.State.Strict (StateT, evalStateT, get, lift, modify')
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Row (Empty, type (.+), type (.==))
import Data.Row.Internal (LT ((:->)), Row (R))
import Language.Haskell.TH
import Prelude

-- | flatRows defines type synonyms of rows, each as the sorted list of
-- labels its definition reduces to.
--
-- The argument is a declaration quote of type synonyms without parameters,
-- each written as a row usually is: labels with '.==', rows joined with
-- '.+', 'Empty', and type synonyms that take a row and add labels to it,
-- such as @Interactive r@:
--
-- > $( flatRows
-- >      [d|
-- >        type HTMLdiv = Interactive ("onScroll" .== Event)
-- >        type HTMLspan = Interactive Empty
-- >        |]
-- >  )
--
-- For each synonym, flatRows looks up the type synonyms its definition
-- uses and collects every label with its type. It refuses a label that
-- appears twice, as '.+' does. It sorts the labels by character code, which
-- is the order of 'GHC.TypeLits.CmpSymbol' and the order '.+' produces, so
-- the list is the same type as the definition. Each synonym keeps its name.
--
-- One splice defines all the rows of a module, because a splice is
-- expensive on the JavaScript backend. There GHC 9.12 spent about 3 s and
-- 500 MB of allocation on each splice and kept much of that memory until
-- the end of the module: with one splice per row, "DOM.HTML.Indexed" (108
-- rows) grew GHC past 10 GB. With one splice, it compiles in about 13 s.
--
-- A type synonym used in a definition is defined in another module, or in
-- the same module above the splice.
flatRows :: Q [Dec] -> Q [Dec]
flatRows quoted = do
  decs <- quoted
  evalStateT (traverse flatten decs) Map.empty
  where
    flatten = \case
      TySynD name [] row -> TySynD (mkName (nameBase name)) [] <$> flatType row
      dec -> lift (fail ("flatRows: expected a type synonym of a row, not " <> pprint dec))

-- | flatRow is one row a row-types type expression builds, written as the
-- sorted list of labels, as 'flatRows' writes it:
--
-- > type HTMLdiv = $(flatRow [t|Interactive ("onScroll" .== Event)|])
--
-- 'flatRows' is cheaper for many rows: see there.
flatRow :: Q Type -> Q Type
flatRow quoted = evalStateT (flatType =<< lift quoted) Map.empty

-- | Synonyms is the type synonyms looked up so far, by name: their
-- parameters and their definition. A definition is looked up once per
-- splice, however many rows use it.
type Synonyms = Map.Map Name ([Name], Type)

flatType :: Type -> StateT Synonyms Q Type
flatType row = do
  labels <- rowLabels row
  case [l | (l, n) <- Map.toList (Map.fromListWith (+) [(l, 1 :: Int) | (l, _) <- labels]), n > 1] of
    [] -> pure ()
    twice -> lift (fail ("flatRow: the label " <> show twice <> " appears more than once in " <> pprint row))
  pure (AppT (PromotedT 'R) (foldr cons PromotedNilT (sortOn fst labels)))
  where
    cons (l, ty) rest =
      AppT (AppT PromotedConsT (AppT (AppT (PromotedT '(:->)) (LitT (StrTyLit l))) ty)) rest

-- | rowLabels is every label of a row type expression, with its type.
--
-- A type synonym is expanded by substituting its arguments for its
-- parameters throughout its definition, field types included: in @type
-- Nested r = "nested" .== Rec r .+ r@ the parameter is both a row to join
-- and part of a field type. A row already written as a sorted list,
-- @R '[ "class" :-> Text, ... ]@, contributes its labels as they are: that
-- is what every element row of "DOM.HTML.Indexed" expands to, so a row
-- built on @HTMLdiv@ reads it this way.
rowLabels :: Type -> StateT Synonyms Q [(String, Type)]
rowLabels = \case
  ParensT t -> rowLabels t
  SigT t _ -> rowLabels t
  InfixT a op b -> operator op a b
  ConT n | n == ''Empty -> pure []
  t -> case unapply t [] of
    (ConT op, [a, b]) | op == ''(.+) || op == ''(.==) -> operator op a b
    (PromotedT r, [list]) | r == 'R -> listLabels list
    (ConT name, args) -> synonym name args
    _ -> unreadable t
  where
    operator op a b
      | op == ''(.+) = (<>) <$> rowLabels a <*> rowLabels b
      | op == ''(.==) = case unparen a of
          LitT (StrTyLit l) -> pure [(l, b)]
          _ -> unreadable a
      | otherwise = unreadable (InfixT a op b)
    listLabels list = case unapply list [] of
      (PromotedNilT, []) -> pure []
      (PromotedConsT, [field, rest]) -> (:) <$> fieldLabel field <*> listLabels rest
      _ -> unreadable list
    fieldLabel field = case unapply field [] of
      (PromotedT arrow, [l, ty]) | arrow == '(:->), LitT (StrTyLit name) <- unparen l -> pure (name, ty)
      _ -> case unparen field of
        PromotedInfixT l arrow ty | arrow == '(:->), LitT (StrTyLit name) <- unparen l -> pure (name, ty)
        InfixT l arrow ty | arrow == '(:->), LitT (StrTyLit name) <- unparen l -> pure (name, ty)
        _ -> unreadable field
    synonym name args = do
      known <- get
      definition <- case Map.lookup name known of
        Just d -> pure (Just d)
        Nothing ->
          lift (reify name) >>= \case
            TyConI (TySynD _ params body) -> do
              let d = (map paramName params, body)
              modify' (Map.insert name d)
              pure (Just d)
            _ -> pure Nothing
      case definition of
        Just (params, body)
          | length params == length args ->
              rowLabels (substitute (Map.fromList (zip params args)) body)
        _ -> unreadable (foldl' AppT (ConT name) args)
    unapply (AppT f x) args = unapply f (x : args)
    unapply (AppKindT f _) args = unapply f args
    unapply (ParensT f) args = unapply f args
    unapply (SigT f _) args = unapply f args
    unapply f args = (f, args)
    unparen (ParensT t) = unparen t
    unparen (SigT t _) = unparen t
    unparen t = t
    paramName = \case
      PlainTV n _ -> n
      KindedTV n _ _ -> n
    unreadable t =
      lift
        ( fail
            ( "flatRow: cannot read "
                <> pprint t
                <> " as a row: expected .==, .+, Empty, a row R '[ ... ] or a type synonym of a row"
            )
        )

-- | substitute replaces the type variables a substitution names with their
-- types, everywhere in a type. The arguments of a type synonym mention only
-- the variables of the place it is used, so nothing is captured; a variable
-- a @forall@ inside the definition binds is left alone.
substitute :: Map.Map Name Type -> Type -> Type
substitute s = \case
  VarT v | Just t <- Map.lookup v s -> t
  AppT f x -> AppT (substitute s f) (substitute s x)
  AppKindT t k -> AppKindT (substitute s t) (substitute s k)
  SigT t k -> SigT (substitute s t) (substitute s k)
  ParensT t -> ParensT (substitute s t)
  InfixT a n b -> InfixT (substitute s a) n (substitute s b)
  UInfixT a n b -> UInfixT (substitute s a) n (substitute s b)
  PromotedInfixT a n b -> PromotedInfixT (substitute s a) n (substitute s b)
  PromotedUInfixT a n b -> PromotedUInfixT (substitute s a) n (substitute s b)
  ForallT binders context t ->
    let inner = foldr (Map.delete . binderName) s binders
     in ForallT binders (map (substitute inner) context) (substitute inner t)
  ForallVisT binders t ->
    let inner = foldr (Map.delete . binderName) s binders
     in ForallVisT binders (substitute inner t)
  ImplicitParamT n t -> ImplicitParamT n (substitute s t)
  t -> t
  where
    binderName = \case
      PlainTV n _ -> n
      KindedTV n _ _ -> n
