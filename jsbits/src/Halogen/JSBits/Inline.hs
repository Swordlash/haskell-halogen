{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Imports of one JavaScript expression each, with no @.js@ file: what
-- "Halogen.JSBits.Fields" writes its fields with, and 'inlineJS', the same
-- for a constructor, a static method or a call with an options object. It
-- sits below "Halogen.JSBits.Promise", whose few bindings it writes, so that
-- 'Halogen.JSBits.browserJS' can wrap the promises a binding returns.
module Halogen.JSBits.Inline
  ( inlineJS
  , importJS
  , Backend (..)
  , backend
  , splitArrows
  , arrow
  )
where

import Data.List (intercalate)
import Halogen.JSBits.Value (Inert (..))
import Language.Haskell.TH

-- | Which code to write, decided by the architecture this package is
-- compiled for. The code for every backend is compiled on every one, so a
-- native build type-checks the generator for the browsers too.
data Backend = OnWasm | OnJavaScript | OnNative
  deriving (Eq)

backend :: Backend
#if defined(javascript_HOST_ARCH) && !defined(HALOGEN_WASI)
backend = OnJavaScript
#elif defined(wasm32_HOST_ARCH) && !defined(HALOGEN_WASI)
backend = OnWasm
#else
backend = OnNative
#endif

-- | A function of one JavaScript expression over its arguments, @$1@,
-- @$2@ and so on, for what is not a member of an object: a constructor, a
-- static method, a call with an options object.
--
-- > $(inlineJS "js_new_url" "(() => { try { return new URL($1, $2); } catch { return null; } })()"
-- >     [t|JSText -> JSText -> IO JSVal|])
--
-- The type is the import's own, as 'Halogen.JSBits.browserJS' takes it
-- ('JSText' for a string), and natively the function is 'inert'. It is a
-- synchronous import with nothing caught: an expression that can throw must
-- catch what it throws itself (a @try@), or it ends the program on
-- WebAssembly.
inlineJS :: String -> String -> Q Type -> Q [Dec]
inlineJS function expression quotedType = do
  ty <- quotedType
  let name = mkName function
  pure $
    if backend == OnNative
      then [SigD name ty, ValD (VarP name) (NormalB (VarE 'inert)) []]
      else [importJS name expression ty]

-- A synchronous import of one JavaScript expression over @$1@, @$2@, ...:
-- on WebAssembly the expression itself, which the linker makes the body of
-- a function of those parameters; on the JavaScript backend that function,
-- written out.
importJS :: Name -> String -> Type -> Dec
importJS name expression ty = ForeignD (ImportF JavaScript Unsafe source name ty)
  where
    source = case backend of
      OnJavaScript -> "((" <> intercalate ", " parameters <> ") => { " <> body <> " })"
      OnWasm -> expression
      OnNative -> expression
    parameters = ["$" <> show n | n <- [1 .. length (fst (splitArrows ty))]]
    body
      | returnsUnit (snd (splitArrows ty)) = expression <> ";"
      | otherwise = "return " <> expression <> ";"
    returnsUnit = \case
      AppT (ConT io) (TupleT 0) -> io == ''IO
      AppT (ConT io) (ConT u) -> io == ''IO && u == ''()
      _ -> False

splitArrows :: Type -> ([Type], Type)
splitArrows = \case
  AppT (AppT ArrowT a) rest -> let (as, r) = splitArrows rest in (a : as, r)
  ParensT t -> splitArrows t
  t -> ([], t)

arrow :: Type -> Type -> Type
arrow a = AppT (AppT ArrowT a)
