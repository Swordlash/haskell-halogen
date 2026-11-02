{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Compile ordinary jsbits into the WASM JSFFI module. The same files are
-- linked by the JavaScript backend through Cabal's @js-sources@ field.
module Halogen.JSBits
  ( browserJS
  , wasmJS
  , Safety (..)
  , module Halogen.JSBits.Value
  )
where

import Control.Monad (forM)
#if defined(javascript_HOST_ARCH) && !defined(HALOGEN_WASI)
import Control.Exception (throwIO)
import Control.Monad (unless)
import GHC.JS.Prim (fromJSString)
#endif
import Data.List (intercalate)
import Halogen.JSBits.Value
import Language.Haskell.TH hiding (Safety (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | How a binding calls its JavaScript function.
data Safety
  = -- | A synchronous call, with what the function throws caught. A
    -- binding whose type ends in 'IO' throws a JavaScript exception as a
    -- 'JSError' the caller can catch. On WebAssembly an exception out of a
    -- synchronous import otherwise ends the program, which no Haskell
    -- handler can stop. The catching costs a small object and one more
    -- crossing per call. A binding of a pure type is not guarded: its
    -- function must not throw.
    Unsafe
  | -- | A synchronous call with nothing caught, for a function that cannot
    -- throw for the values it is given and is called so often that the
    -- catching would show: the DOM operations a render makes. An exception
    -- out of it ends the program on WebAssembly.
    Unchecked
  | -- | A call that awaits the Promise the function returns.
    Safe
  deriving (Eq, Show)

-- | Each binding names a Haskell function, a JavaScript function, its
-- 'Safety' and its type. Callbacks and representation conversions stay
-- backend-specific.
--
-- Source files are tracked as compilation dependencies and embedded once per
-- splice. A NOINLINE unit CAF initializes a splice-specific Symbol-keyed API once. Keeping
-- JSVal out of the CAF avoids passing an updated thunk to WASM marshalling.
-- Functions are not installed as named globals and no script loading is needed.
wasmJS :: [FilePath] -> [(String, String, Safety, Q Type)] -> Q [Dec]
wasmJS files bindings = do
  loc <- location
  let sourceFile = loc_filename loc
      key = show ("halogen.jsbits:" <> loc_package loc <> ":" <> loc_module loc <> ":" <> show (loc_start loc))
      namespace = "globalThis[Symbol.for(" <> key <> ")]"
  sources <- forM files $ \file -> do
    path <- runIO $ findSource (takeDirectory sourceFile) file
    addDependentFile path
    runIO $ readFile path
  load <- newName "loadJSBits"
  api <- newName "jsbitsAPI"
  let names = [name | (_, name, _, _) <- bindings]
      body = namespace <> " = (() => {\n" <> intercalate "\n;\n" sources <> "\nreturn {" <> intercalate "," names <> "};})();"
      valueType = TupleT 0
      loader = ForeignD (ImportF JavaScript TH.Unsafe body load (AppT (ConT ''IO) valueType))
      cache =
        [ SigD api valueType
        , ValD (VarP api) (NormalB (AppE (VarE 'unsafePerformIO) (VarE load))) []
        , PragmaD (InlineP api NoInline FunLike AllPhases)
        ]
  declarations <- forM bindings $ \(haskell, javascript, safety, quotedType) -> do
    ty <- quotedType
    raw <- newName (haskell <> "Raw")
    arguments <- mapM (const (newName "argument")) [1 .. arity ty]
    let call = namespace <> "." <> javascript <> "(" <> intercalate "," ["$" <> show n | n <- [1 .. length arguments]] <> ")"
        function = mkName haskell
        applied = foldl AppE (VarE raw) (map VarE arguments)
        loaded e = InfixE (Just (VarE api)) (VarE 'seq) (Just e)
    case guardedResult safety ty of
      Nothing ->
        let expression = if safety == Safe then "await " <> call else call
         in pure
              [ ForeignD (ImportF JavaScript (ffiSafety safety) expression raw ty)
              , SigD function ty
              , FunD function [Clause (map VarP arguments) (NormalB (loaded applied)) []]
              ]
      Just result -> do
        (values, check) <- guarded "$1.value" haskell result
        checked <- check (pure applied)
        pure $
          [ ForeignD (ImportF JavaScript TH.Unsafe (caught call) raw (withResult (ConT ''JSVal) ty))
          , SigD function ty
          , FunD function [Clause (map VarP arguments) (NormalB (loaded checked)) []]
          ]
            <> values
  pure (loader : cache <> concat declarations)

-- The arguments a binding's type takes.
arity :: Type -> Int
arity (AppT (AppT ArrowT _) result) = 1 + arity result
arity (SigT ty _) = arity ty
arity _ = 0

-- The type with its IO result replaced.
withResult :: Type -> Type -> Type
withResult new = \case
  AppT (AppT ArrowT argument) result -> AppT (AppT ArrowT argument) (withResult new result)
  _ -> AppT (ConT ''IO) new

ffiSafety :: Safety -> TH.Safety
ffiSafety = \case
  Safe -> TH.Safe
  _ -> TH.Unsafe

-- The result of a binding whose JavaScript exception is caught: an 'Unsafe'
-- one whose type ends in @IO r@.
guardedResult :: Safety -> Type -> Maybe Type
guardedResult safety ty
  | safety /= Unsafe = Nothing
  | otherwise = go ty
  where
    go = \case
      AppT (AppT ArrowT _) rest -> go rest
      SigT t _ -> go t
      AppT (ConT io) result | io == ''IO -> Just result
      _ -> Nothing

-- A call, in JavaScript, made into an outcome object: @{ ok: true, value }@
-- with what it returned, or @{ ok: false, error }@ with what it threw.
caught :: String -> String
caught call = "(() => { try { return { ok: true, value: " <> call <> " }; } catch (e) { return { ok: false, error: e }; } })()"

-- What turns an outcome into the binding's result: 'checkThrown', which
-- throws a caught exception, and for a result other than @()@ an import
-- (of the JavaScript given) that reads the value with the FFI's conversion
-- for its type. The outcome is made and read with nothing in between, so no
-- other Haskell thread can see it.
guarded :: String -> String -> Type -> Q ([Dec], Q Exp -> Q Exp)
guarded readValue haskell result
  | isUnit result = pure ([], \call -> [|$call >>= checkThrown|])
  | otherwise = do
      value <- newName (haskell <> "Value")
      r <- newName "outcome"
      pure
        ( [ForeignD (ImportF JavaScript TH.Unsafe readValue value (AppT (AppT ArrowT (ConT ''JSVal)) (AppT (ConT ''IO) result)))]
        , \call -> [|$call >>= \ $(varP r) -> checkThrown $(varE r) >> $(varE value) $(varE r)|]
        )
  where
    isUnit = \case
      TupleT 0 -> True
      ConT u -> u == ''()
      _ -> False

-- | One list of bindings for every backend: on WebAssembly it is 'wasmJS'; on
-- the JavaScript backend, where Cabal links the same files through
-- @js-sources@, each binding imports its JavaScript function by name. A safe
-- binding there is @interruptible@: it awaits the function's Promise and
-- throws its rejection as an 'IOError'. It must return @IO ()@, and the module
-- splicing it needs @InterruptibleFFI@. A binding whose work is awaited later,
-- or whose result or rejection is wanted, returns a
-- "Halogen.JSBits.Promise" 'Halogen.JSBits.Promise.Promise' from an 'Unsafe'
-- binding instead.
--
-- Natively, with no engine to call, each binding is 'inert': it does nothing
-- and returns nothing in particular.
browserJS :: [FilePath] -> [(String, String, Safety, Q Type)] -> Q [Dec]
#if defined(javascript_HOST_ARCH) && !defined(HALOGEN_WASI)
browserJS _ bindings = concat <$> forM bindings importByName
  where
    importByName (haskell, javascript, safety, quotedType) = do
      ty <- quotedType
      let function = mkName haskell
      case (safety, guardedResult safety ty) of
        (Unsafe, Just result) -> do
          raw <- newName (haskell <> "Raw")
          arguments <- mapM (const (newName "argument")) [1 .. arity ty]
          let names = ["a" <> show n | n <- [1 .. length arguments]]
              wrapper = "((" <> intercalate ", " names <> ") => { return " <> caught (javascript <> "(" <> intercalate ", " names <> ")") <> "; })"
          (values, check) <- guarded "((r) => { return r.value; })" haskell result
          body <- check (pure (foldl AppE (VarE raw) (map VarE arguments)))
          pure $
            [ ForeignD (ImportF JavaScript TH.Unsafe wrapper raw (withResult (ConT ''JSVal) ty))
            , SigD function ty
            , FunD function [Clause (map VarP arguments) (NormalB body) []]
            ]
              <> values
        _ | safety /= Safe -> pure [ForeignD (ImportF JavaScript TH.Unsafe javascript function ty)]
        _ -> do
          unless (returnsUnit ty) $
            fail ("browserJS: the safe binding " <> haskell <> " must return IO () on the JavaScript backend")
          raw <- newName (haskell <> "Raw")
          arguments <- mapM (const (newName "argument")) [1 .. arity ty]
          let names = ["a" <> show n | n <- [1 .. length arguments]]
              wrapper = "((" <> intercalate ", " (names <> ["done"]) <> ") => { " <> javascript <> "(" <> intercalate ", " names
                <> ").then(() => done(null), error => done(String(error?.message ?? error))); })"
          outcome <- newName "outcome"
          pure
            [ ForeignD (ImportF JavaScript TH.Interruptible wrapper raw (withResult (ConT ''JSVal) ty))
            , SigD function ty
            , FunD function
                [ Clause (map VarP arguments)
                    (NormalB (InfixE (Just (foldl AppE (VarE raw) (map VarE arguments))) (VarE '(>>=))
                      (Just (LamE [VarP outcome]
                        (AppE (AppE (VarE 'unless) (AppE (VarE 'isNull) (VarE outcome)))
                          (AppE (VarE 'throwIO) (AppE (VarE 'userError) (AppE (VarE 'fromJSString) (VarE outcome)))))))))
                    [] ]
            ]
    returnsUnit = \case
      AppT (AppT ArrowT _) result -> returnsUnit result
      AppT (ConT io) (TupleT 0) -> io == ''IO
      _ -> False
#elif defined(wasm32_HOST_ARCH) && !defined(HALOGEN_WASI)
browserJS = wasmJS
#else
browserJS _ bindings = concat <$> forM bindings stub
  where
    stub (haskell, _, _, quotedType) = do
      ty <- quotedType
      let function = mkName haskell
      pure [SigD function ty, ValD (VarP function) (NormalB (VarE 'inert)) []]
#endif

-- Locate sources relative to the package, even when Cabal compiles from a
-- project root or the package is unpacked from an sdist elsewhere.
findSource :: FilePath -> FilePath -> IO FilePath
findSource directory file = do
  absolute <- makeAbsolute directory
  search absolute
  where
    search directory' = do
      let candidate = directory' </> file
      exists <- doesFileExist candidate
      if exists
        then pure candidate
        else
          if takeDirectory directory' == directory'
            then ioError (userError ("wasmJS: cannot find " <> file))
            else search (takeDirectory directory')
