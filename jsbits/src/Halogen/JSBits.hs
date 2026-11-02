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
import Data.List (intercalate)
import Halogen.JSBits.Inline (importJS)
import Halogen.JSBits.Promise (Promise, promise)
import Halogen.JSBits.Value
import Language.Haskell.TH hiding (Safety (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | How a binding calls its JavaScript function. Both are synchronous: a
-- function that starts asynchronous work returns a promise, which a binding
-- whose type ends in @IO ('Promise' a)@ gives as a "Halogen.JSBits.Promise"
-- 'Promise', listened to at once, for the program to wait for.
data Safety
  = -- | What the function throws is caught. A binding whose type ends in
    -- 'IO' throws a JavaScript exception as a 'JSError' the caller can
    -- catch. On WebAssembly an exception out of a synchronous import
    -- otherwise ends the program, which no Haskell handler can stop. The
    -- catching costs a small object and one more crossing per call. A
    -- binding of a pure type is not guarded: its function must not throw.
    Unsafe
  | -- | Nothing is caught, for a function that cannot throw for the values
    -- it is given and is called so often that the catching would show: the
    -- DOM operations a render makes. An exception out of it ends the program
    -- on WebAssembly.
    Unchecked
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
      loaded e = InfixE (Just (VarE api)) (VarE 'seq) (Just e)
  declarations <- forM bindings (binding ((namespace <> ".") <>) loaded)
  pure (loader : cache <> concat declarations)

-- The declarations of one binding: its imports and its function. @named@
-- gives the expression that names the JavaScript function, @loaded@ wraps
-- the function's body (on WebAssembly, in the forcing of the splice's
-- namespace).
--
-- An 'Unsafe' binding whose type ends in @IO r@ calls the function inside a
-- @try@, which makes an outcome object, @{ ok: true, value }@ or
-- @{ ok: false, error }@; 'checkThrown' throws what it caught, and a second
-- import reads the value with the FFI's conversion for @r@. The outcome is
-- made and read with nothing in between, so no other thread can see it. A
-- result of type @'Promise' a@ crosses as a 'JSVal' and is made a 'Promise'
-- with 'promise'.
binding :: (String -> String) -> (Exp -> Exp) -> (String, String, Safety, Q Type) -> Q [Dec]
binding named loaded (haskell, javascript, safety, quotedType) = do
  ty <- quotedType
  raw <- newName (haskell <> "Raw")
  arguments <- mapM (const (newName "argument")) [1 .. arity ty]
  let call = named javascript <> "(" <> intercalate ", " ["$" <> show n | n <- [1 .. length arguments]] <> ")"
      function = mkName haskell
      applied = foldl AppE (VarE raw) (map VarE arguments)
      declare imports body = pure (imports <> [SigD function ty, FunD function [Clause (map VarP arguments) (NormalB (loaded body)) []]])
  case ioResult ty of
    Nothing -> declare [importJS raw call ty] applied
    Just result -> do
      let (ffiResult, made) = case promiseOf result of
            Just _ -> (ConT ''JSVal, \e -> [|$e >>= promise|])
            Nothing -> (result, id)
      if safety == Unchecked
        then do
          body <- made (pure applied)
          declare [importJS raw call (withResult ffiResult ty)] body
        else do
          let outcome = importJS raw (caught call) (withResult (ConT ''JSVal) ty)
          if isUnit ffiResult
            then do
              body <- made [|$(pure applied) >>= checkThrown|]
              declare [outcome] body
            else do
              value <- newName (haskell <> "Value")
              r <- newName "outcome"
              body <- made [|$(pure applied) >>= \ $(varP r) -> checkThrown $(varE r) >> $(varE value) $(varE r)|]
              declare [outcome, importJS value "$1.value" (AppT (AppT ArrowT (ConT ''JSVal)) (AppT (ConT ''IO) ffiResult))] body
  where
    isUnit = \case
      TupleT 0 -> True
      ConT u -> u == ''()
      _ -> False

-- The arguments a binding's type takes.
arity :: Type -> Int
arity (AppT (AppT ArrowT _) result) = 1 + arity result
arity (SigT ty _) = arity ty
arity _ = 0

-- The type with its IO result replaced.
withResult :: Type -> Type -> Type
withResult new = \case
  AppT (AppT ArrowT argument) result -> AppT (AppT ArrowT argument) (withResult new result)
  SigT t _ -> withResult new t
  _ -> AppT (ConT ''IO) new

-- The @r@ of a type that ends in @IO r@.
ioResult :: Type -> Maybe Type
ioResult = \case
  AppT (AppT ArrowT _) rest -> ioResult rest
  SigT t _ -> ioResult t
  AppT (ConT io) result | io == ''IO -> Just result
  _ -> Nothing

-- The @a@ of @'Promise' a@.
promiseOf :: Type -> Maybe Type
promiseOf = \case
  AppT (ConT p) a | p == ''Promise -> Just a
  ParensT t -> promiseOf t
  _ -> Nothing

-- A call, in JavaScript, made into an outcome object: @{ ok: true, value }@
-- with what it returned, or @{ ok: false, error }@ with what it threw.
caught :: String -> String
caught call = "(() => { try { return { ok: true, value: " <> call <> " }; } catch (e) { return { ok: false, error: e }; } })()"

-- | One list of bindings for every backend: on WebAssembly it is 'wasmJS'; on
-- the JavaScript backend, where Cabal links the same files through
-- @js-sources@, each binding calls its JavaScript function by name.
--
-- Natively, with no engine to call, each binding is 'inert': it does nothing
-- and returns nothing in particular (a 'Promise' that has been rejected).
browserJS :: [FilePath] -> [(String, String, Safety, Q Type)] -> Q [Dec]
#if defined(javascript_HOST_ARCH) && !defined(HALOGEN_WASI)
browserJS _ bindings = concat <$> forM bindings (binding id id)
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
