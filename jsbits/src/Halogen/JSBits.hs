{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Compile ordinary jsbits into the WASM JSFFI module. The same files are
-- linked by the JavaScript backend through Cabal's @js-sources@ field.
module Halogen.JSBits (browserJS, wasmJS, Safety (..)) where

import Control.Monad (forM)
#if defined(javascript_HOST_ARCH)
import Control.Exception (throwIO)
import Control.Monad (unless)
import GHC.JS.Prim (JSVal, fromJSString, isNull)
#endif
import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | Each binding names a Haskell function, a JavaScript function, its FFI
-- safety and its type. Safe imports await a Promise; unsafe ones return on
-- the spot. Callbacks and representation conversions stay backend-specific.
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
      loader = ForeignD (ImportF JavaScript Unsafe body load (AppT (ConT ''IO) valueType))
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
        expression = if safety == Safe then "await " <> call else call
        foreignType = ty
        function = mkName haskell
    pure
      [ ForeignD (ImportF JavaScript safety expression raw foreignType)
      , SigD function ty
      , FunD function [Clause (map VarP arguments) (NormalB (InfixE (Just (VarE api)) (VarE 'seq) (Just (foldl AppE (VarE raw) (map VarE arguments))))) []]
      ]
  pure (loader : cache <> concat declarations)
  where
    arity :: Type -> Int
    arity (AppT (AppT ArrowT _) result) = 1 + arity result
    arity (SigT ty _) = arity ty
    arity _ = 0

-- | One list of bindings for both browser backends: on WebAssembly it is
-- 'wasmJS'; on the JavaScript backend, where Cabal links the same files
-- through @js-sources@, each binding imports its JavaScript function by name.
-- A safe binding there is @interruptible@: it awaits the function's Promise
-- and throws its rejection as an 'IOError'. It must return @IO ()@, and the
-- module splicing it needs @InterruptibleFFI@.
browserJS :: [FilePath] -> [(String, String, Safety, Q Type)] -> Q [Dec]
#if defined(javascript_HOST_ARCH)
browserJS _ bindings = concat <$> forM bindings importByName
  where
    importByName (haskell, javascript, safety, quotedType) = do
      ty <- quotedType
      let function = mkName haskell
      if safety /= Safe
        then pure [ForeignD (ImportF JavaScript safety javascript function ty)]
        else do
          unless (returnsUnit ty) $
            fail ("browserJS: the safe binding " <> haskell <> " must return IO () on the JavaScript backend")
          raw <- newName (haskell <> "Raw")
          arguments <- mapM (const (newName "argument")) [1 .. arity ty]
          let names = ["a" <> show n | n <- [1 .. length arguments]]
              wrapper = "((" <> intercalate ", " (names <> ["done"]) <> ") => { " <> javascript <> "(" <> intercalate ", " names
                <> ").then(() => done(null), error => done(String(error?.message ?? error))); })"
          outcome <- newName "outcome"
          pure
            [ ForeignD (ImportF JavaScript Interruptible wrapper raw (withResult (ConT ''JSVal) ty))
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
    -- The type with its IO result replaced.
    withResult new = \case
      AppT (AppT ArrowT argument) result -> AppT (AppT ArrowT argument) (withResult new result)
      _ -> AppT (ConT ''IO) new
    arity :: Type -> Int
    arity (AppT (AppT ArrowT _) result) = 1 + arity result
    arity _ = 0
#else
browserJS = wasmJS
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
