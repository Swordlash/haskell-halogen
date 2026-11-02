{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fields of JavaScript objects, as record fields: @w.navigator.onLine@,
-- @doc.title_set "Notes"@, @crypto.randomUUID@. 'jsFields' writes a
-- 'HasField' instance for each member of a type, so that
-- @OverloadedRecordDot@ reads it; 'jsFieldFunctions' writes the same members
-- as plain functions, for a module meant to be imported qualified
-- (@History.go h 1@), as purescript-web has them.
--
-- The JavaScript is one expression in each foreign import, so a type needs
-- no @.js@ file of its own. On WebAssembly an import is
-- @foreign import javascript unsafe "$1.navigator"@; on the JavaScript
-- backend it is the same expression in an arrow function; natively, with no
-- engine to call, each field is 'inert', as 'Halogen.JSBits.browserJS'
-- stubs are.
--
-- > newtype Navigator = Navigator JSVal
-- >   deriving newtype (Inert)
-- >
-- > $( jsFields ''Navigator
-- >      [ Live "onLine" [t|IO Bool|]
-- >      , Fixed "clipboard" [t|Clipboard|]
-- >      ]
-- >  )
--
-- = The kinds of member
--
-- A member is one of five things, and its constructor says which:
--
-- ['Fixed'] A value that does not change while the object lives, read
--   purely: @w.navigator :: Navigator@, @e.key :: Text@. Objects that a
--   page keeps for its whole life (the navigator, the history) are fixed,
--   and so is what an event says about itself, so fields chain:
--   @w.navigator.clipboard@.
--
-- ['Live'] A value that changes, read as an 'IO' action each time:
--   @w.navigator.onLine :: IO Bool@, @doc.title :: IO Text@.
--
-- ['Writable'] A 'Live' value the page can also set. It has a second
--   field, whose name is the member's with @_set@ after it, holding the
--   assignment: @doc.title_set :: Text -> IO ()@. No Web API names a member
--   with @_set@ in it, so the two cannot clash. A value without the @_set@
--   field is read-only.
--
-- ['Method'] A method, as a field holding the call: @history.go :: Int ->
--   IO ()@, @crypto.randomUUID :: IO Text@.
--
-- ['Awaited'] A method that returns a Promise, as a field holding the call
--   and the wait for the Promise: @blob.text :: IO Text@. A rejection is
--   thrown as a 'Halogen.JSBits.Promise.JSError', and so is an exception
--   the method throws before it returns its Promise.
--
-- A member whose JavaScript name is a Haskell keyword gets an underscore
-- after its field name: @File@'s @type@ is @f.type_@.
--
-- = Types
--
-- A member's type is built from 'Bool', 'Int', 'Double', 'Text', 'JSVal',
-- a newtype over a 'JSVal' (an object, such as @Navigator@), and 'Maybe'
-- around 'Text', a 'JSVal' or such a newtype, which reads JavaScript's
-- @null@ and @undefined@ as 'Nothing'. An 'IO' result can also be @()@, and
-- an 'Awaited' one a 'Data.ByteString.ByteString' (a @Uint8Array@ copied
-- out). As for any foreign import, the constructor of each newtype in the
-- type must be in scope where the splice is.
--
-- Natively a member's type needs an 'Inert' instance; a newtype over a
-- 'JSVal' derives it. 'Natively' gives a member a native answer of its own
-- instead: core reads an event made from JSON that way.
--
-- = Exceptions
--
-- A 'Fixed', 'Live', 'Writable' or 'Method' member is a synchronous import,
-- and on WebAssembly a JavaScript exception out of a synchronous import ends
-- the program: it cannot be caught as a Haskell exception there. So a call
-- that can throw (@showModal@ on a dialog that is already open, a selector
-- that does not parse) must be made only when it cannot, or written in
-- jsbits with its own @try@. An 'Awaited' member catches what its method
-- throws.
--
-- = Where instances go
--
-- An instance belongs in the module that defines the newtype, or in the
-- module of a type the field returns, so that it is not an orphan.
module Halogen.JSBits.Fields
  ( Member (..)
  , jsFields
  , jsFieldFunctions
  , jsCast
  , inlineJS
  )
where

import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Coerce (coerce)
import Data.List (intercalate, isSuffixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Records (HasField (..))
import Halogen.JSBits.Promise (Promise (..), await)
import Halogen.JSBits.Value (Inert (..), JSText, JSVal, fromJSBytes, fromJSText, isNull, jsNull, jsValText, toJSText)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)

-- | One member of a JavaScript object: its JavaScript name and the type of
-- its field. See the module's description for what each kind means.
data Member
  = -- | A value that does not change, read purely.
    Fixed String (Q Type)
  | -- | A value that changes, read in 'IO': the type is @IO a@.
    Live String (Q Type)
  | -- | A value that changes and that the page can set: the type is @IO a@,
    -- and a second field, @name_set :: a -> IO ()@, sets it.
    Writable String (Q Type)
  | -- | A method: the type is @a -> b -> ... -> IO r@, or @IO r@ for none.
    Method String (Q Type)
  | -- | A method that returns a Promise, which the field awaits: the type
    -- is @a -> ... -> IO r@, with @r@ what the Promise resolves with.
    Awaited String (Q Type)
  | -- | The member, with this expression as its field natively instead of
    -- 'inert'. It has the field's type (for a 'Writable' member, the
    -- read's). The browsers ignore it.
    Natively (Q Exp) Member

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

-- | A 'HasField' instance for each member of the type, which must be a
-- newtype over a 'JSVal' (or over another such newtype, as an event type is
-- over @Event@), with no type parameters.
jsFields :: Name -> [Member] -> Q [Dec]
jsFields receiver members = concat <$> mapM (member receiver Nothing) members

-- | Each field of the type, as a function of the same name, taking the
-- object first: @go :: History -> Int -> IO ()@. A @_set@ field becomes
-- @setName@: @title_set@ is @setTitle@. The fields are the 'HasField'
-- instances in scope for the type, so splice this after 'jsFields' (or in
-- a module that imports the instances); the second argument lists fields
-- the module writes functions for itself.
jsFieldFunctions :: Name -> [String] -> Q [Dec]
jsFieldFunctions receiver handWritten = do
  instances <- reifyInstances ''HasField [VarT (mkName "field"), ConT receiver, VarT (mkName "value")]
  let fields = sortOn fst [(field, ty) | InstanceD _ _ h _ <- instances, (_, [LitT (StrTyLit field), _, ty]) <- [spine h], field `notElem` handWritten]
  fmap concat . forM fields $ \(field, ty) -> do
    let name = mkName (functionName field)
    addModFinalizer $ putDoc (DeclDoc name) ("The @" <> field <> "@ field of '" <> nameBase receiver <> "', as a function.")
    pure [SigD name (ConT receiver `arrow` ty), ValD (VarP name) (NormalB (AppTypeE (VarE 'getField) (LitT (StrTyLit field)))) []]
  where
    spine = go []
      where
        go args = \case
          AppT f x -> go (x : args) f
          AppKindT t _ -> go args t
          SigT t _ -> go args t
          ParensT t -> go args t
          t -> (t, args)
    functionName field
      | "_set" `isSuffixOf` field, (c : cs) <- take (length field - 4) field = "set" <> (toUpper c : cs)
      | otherwise = field

-- | A checked cast, named by the first argument, from one type to another
-- that JavaScript knows as a class of the same name: @$(jsCast "fromEvent"
-- ''Event ''KeyboardEvent)@ writes @fromEvent :: Event -> Maybe
-- KeyboardEvent@, which asks @instanceof KeyboardEvent@. An engine with no
-- such class (Node has no @KeyboardEvent@) gives 'Nothing', and so does
-- every cast natively.
jsCast :: String -> Name -> Name -> Q [Dec]
jsCast function from to = do
  let name = mkName function
      signature = SigD name (ConT from `arrow` AppT (ConT ''Maybe) (ConT to))
      klass = nameBase to
  addModFinalizer $
    putDoc (DeclDoc name) ("The value as a '" <> klass <> "', if JavaScript says it is one (@instanceof " <> klass <> "@). Natively 'Nothing'.")
  if backend == OnNative
    then pure [signature, ValD (VarP name) (NormalB (VarE 'inert)) []]
    else do
      test <- newName ("js_is_" <> klass <> "_" <> function)
      body <- [|\value -> if $(varE test) value then Just (coerce value) else Nothing|]
      pure
        [ importJS test ("(typeof " <> klass <> " === \"function\" && $1 instanceof " <> klass <> ")") (ConT from `arrow` ConT ''Bool)
        , signature
        , ValD (VarP name) (NormalB body) []
        ]

-- | A function of one JavaScript expression over its arguments, @$1@,
-- @$2@ and so on, for what is not a member of an object: a constructor, a
-- static method, a call with an options object.
--
-- > $(inlineJS "js_new_url" "(() => { try { return new URL($1, $2); } catch { return null; } })()"
-- >     [t|JSText -> JSText -> IO JSVal|])
--
-- The type is the import's own, as 'Halogen.JSBits.browserJS' takes it
-- ('JSText' for a string), and natively the function is 'inert'. It is a
-- synchronous import: what can throw must catch, as above.
inlineJS :: String -> String -> Q Type -> Q [Dec]
inlineJS function expression quotedType = do
  ty <- quotedType
  let name = mkName function
  pure $
    if backend == OnNative
      then [SigD name ty, ValD (VarP name) (NormalB (VarE 'inert)) []]
      else [importJS name expression ty]

--------------------------------------------------------------------------------
-- Members
--------------------------------------------------------------------------------

-- The declarations for one member: its instances, and in a browser the
-- imports they call.
member :: Name -> Maybe (Q Exp) -> Member -> Q [Dec]
member receiver native = \case
  Natively e m -> member receiver (Just (fromMaybe e native)) m
  Fixed js quoted -> do
    ty <- quoted
    shape <- valueShape js ty
    when (shape `elem` [Unit, Bytes]) $ failFor js "a fixed member cannot be () or a ByteString; read it as Live"
    get <- browser native $ do
      raw <- rawName "get" js
      pure ([importJS raw (nullish shape ("$1." <> js)) (self `arrow` ffiType shape ty)], [|\o -> $(pureResult shape) ($(varE raw) o)|])
    pure (field js ty get)
  Live js quoted -> do
    ty <- quoted
    (shape, result) <- ioResult js ty
    get <- browser native (readLive js shape result)
    pure (field js ty get)
  Writable js quoted -> do
    ty <- quoted
    (shape, result) <- ioResult js ty
    get <- browser native (readLive js shape result)
    argument <- argumentShape js result
    set <- browser Nothing $ do
      raw <- rawName "set" js
      v <- newName "v"
      pure
        ( [importJS raw ("$1." <> js <> " = $2") (self `arrow` (ffiType argument result `arrow` AppT (ConT ''IO) (TupleT 0)))]
        , [|\o -> $(lamE [varP v] [|$(varE raw) o $(toForeign argument (varE v))|])|]
        )
    pure (field js ty get <> field (js <> "_set") (result `arrow` AppT (ConT ''IO) (TupleT 0)) set)
  Method js quoted -> do
    ty <- quoted
    let (arguments, io) = splitArrows ty
    (shape, result) <- ioResult js io
    shapes <- mapM (argumentShape js) arguments
    call <- browser native $ do
      raw <- rawName "call" js
      vs <- mapM (const (newName "a")) arguments
      o <- newName "o"
      let ffi = foldr arrow (AppT (ConT ''IO) (ffiType shape result)) (self : zipWith ffiType shapes arguments)
          applied = foldl appE (varE raw) (varE o : zipWith toForeign shapes (map varE vs))
      pure ([importJS raw (nullish shape (callOf js (length arguments))) ffi], lamE (varP o : map varP vs) (ioFrom shape applied))
    pure (field js ty call)
  Awaited js quoted -> do
    ty <- quoted
    let (arguments, io) = splitArrows ty
    (shape, result) <- ioResult js io
    shapes <- mapM (argumentShape js) arguments
    call <- browser native $ do
      raw <- rawName "call" js
      (conversions, settled) <- awaitedResult shape result
      vs <- mapM (const (newName "a")) arguments
      o <- newName "o"
      let ffi = foldr arrow (AppT (ConT ''IO) (ConT ''JSVal)) (self : zipWith ffiType shapes arguments)
          applied = foldl appE (varE raw) (varE o : zipWith toForeign shapes (map varE vs))
          awaitedCall
            | isMaybe shape = "(async () => (await " <> callOf js (length arguments) <> ") ?? null)()"
            | otherwise = "(async () => " <> callOf js (length arguments) <> ")()"
      pure
        ( importJS raw awaitedCall ffi : conversions
        , lamE (varP o : map varP vs) [|$applied >>= await . Promise >>= either throwIO $settled|]
        )
    pure (field js ty call)
  where
    self = ConT receiver
    typeName = nameBase receiver
    rawName kind js = newName ("js_" <> kind <> "_" <> typeName <> "_" <> js)
    readLive js shape result = do
      raw <- rawName "get" js
      pure ([importJS raw (nullish shape ("$1." <> js)) (self `arrow` AppT (ConT ''IO) (ffiType shape result))], [|\o -> $(ioFrom shape [|$(varE raw) o|])|])
    field js ty (decls, impl) = decls <> [fieldInstance (fieldName js) receiver ty impl]

-- In a browser, the imports and the field's expression the generator
-- writes; natively no imports, and the native expression or 'inert'.
browser :: Maybe (Q Exp) -> Q ([Dec], Q Exp) -> Q ([Dec], Exp)
browser native generate
  | backend == OnNative = ([],) <$> fromMaybe [|inert|] native
  | otherwise = do
      (decls, impl) <- generate
      (decls,) <$> impl

fieldInstance :: String -> Name -> Type -> Exp -> Dec
fieldInstance name receiver ty impl =
  InstanceD Nothing [] (foldl AppT (ConT ''HasField) [LitT (StrTyLit name), ConT receiver, ty]) [ValD (VarP 'getField) (NormalB impl) []]

-- The field's name: the member's, with an underscore after a Haskell
-- keyword, which a record field cannot be.
fieldName :: String -> String
fieldName js
  | js `elem` keywords = js <> "_"
  | otherwise = js
  where
    keywords = ["case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where"]

--------------------------------------------------------------------------------
-- Values crossing
--------------------------------------------------------------------------------

-- How a value of a member's type crosses the FFI.
data Shape
  = -- | A 'JSVal', or a newtype over one: as it is.
    Plain
  | -- | 'Bool', 'Int' or 'Double', which the FFI converts.
    Primitive
  | -- | 'Text', as a 'JSText'.
    AsText
  | -- | @Maybe Text@, as a 'JSVal' that may be @null@.
    MaybeText
  | -- | 'Maybe' a 'JSVal' or a newtype over one, as a 'JSVal' that may be
    -- @null@.
    MaybeValue
  | -- | A 'ByteString', as a @Uint8Array@.
    Bytes
  | -- | @()@, a result only.
    Unit
  deriving (Eq)

isMaybe :: Shape -> Bool
isMaybe s = s == MaybeText || s == MaybeValue

valueShape :: String -> Type -> Q Shape
valueShape js ty = maybe (failFor js ("cannot pass a value of type " <> pprint ty)) pure (shapeOf ty)
  where
    shapeOf = \case
      TupleT 0 -> Just Unit
      ConT n
        | n == ''() -> Just Unit
        | n == ''Text -> Just AsText
        | n == ''ByteString -> Just Bytes
        | n `elem` [''Bool, ''Int, ''Double] -> Just Primitive
        | otherwise -> Just Plain
      AppT (ConT m) a | m == ''Maybe -> case shapeOf a of
        Just AsText -> Just MaybeText
        Just Plain -> Just MaybeValue
        _ -> Nothing
      _ -> Nothing

-- The member's IO result: its shape and its type.
ioResult :: String -> Type -> Q (Shape, Type)
ioResult js = \case
  AppT (ConT io) result | io == ''IO -> (,result) <$> valueShape js result
  ty -> failFor js ("the type must end in IO, not " <> pprint ty)

argumentShape :: String -> Type -> Q Shape
argumentShape js ty = do
  shape <- valueShape js ty
  when (shape `elem` [Unit, Bytes]) $ failFor js ("cannot take an argument of type " <> pprint ty)
  pure shape

ffiType :: Shape -> Type -> Type
ffiType shape ty = case shape of
  AsText -> ConT ''JSText
  MaybeText -> ConT ''JSVal
  MaybeValue -> ConT ''JSVal
  Bytes -> ConT ''JSVal
  _ -> ty

-- The JavaScript of a read that may give @null@ or @undefined@, so that
-- 'isNull' alone decides.
nullish :: Shape -> String -> String
nullish shape js
  | isMaybe shape = "(" <> js <> " ?? null)"
  | otherwise = js

callOf :: String -> Int -> String
callOf js n = "$1." <> js <> "(" <> intercalate ", " ["$" <> show i | i <- [2 .. n + 1]] <> ")"

-- From what the import gives to the field's value, purely.
pureResult :: Shape -> Q Exp
pureResult = \case
  AsText -> [|fromJSText|]
  MaybeText -> [|\v -> if isNull v then Nothing else Just (jsValText v)|]
  MaybeValue -> [|\v -> if isNull v then Nothing else Just (coerce v)|]
  _ -> [|id|]

-- The import's call, as the field's IO action.
ioFrom :: Shape -> Q Exp -> Q Exp
ioFrom shape call = case shape of
  Unit -> call
  Primitive -> call
  Plain -> call
  Bytes -> [|$call >>= fromJSBytes|]
  _ -> [|fmap $(pureResult shape) $call|]

-- From what a Promise resolved with to the field's value, in IO, and the
-- imports that takes: a number or a boolean is read by an import of its
-- own, since the FFI converts only what an import returns.
awaitedResult :: Shape -> Type -> Q ([Dec], Q Exp)
awaitedResult shape ty = case shape of
  Unit -> pure ([], [|\_ -> pure ()|])
  Bytes -> pure ([], [|fromJSBytes|])
  AsText -> pure ([], [|pure . jsValText|])
  Plain -> pure ([], [|pure . coerce|])
  Primitive -> do
    raw <- newName ("js_resolved_" <> concat [nameBase n | ConT n <- [ty]])
    pure ([importJS raw "$1" (ConT ''JSVal `arrow` ty)], [|pure . $(varE raw)|])
  _ -> pure ([], [|pure . $(pureResult shape)|])

-- From the field's argument to what the import takes.
toForeign :: Shape -> Q Exp -> Q Exp
toForeign shape v = case shape of
  AsText -> [|toJSText $v|]
  MaybeText -> [|maybe jsNull toJSText $v|]
  MaybeValue -> [|maybe jsNull coerce $v|]
  _ -> v

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

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

failFor :: String -> String -> Q a
failFor js message = fail ("jsFields: the member " <> js <> ": " <> message)
