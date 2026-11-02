{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
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
-- >      [ Live "onLine" ''Bool
-- >      , Fixed "clipboard" ''Clipboard
-- >      ]
-- >  )
--
-- = The kinds of member
--
-- A member is one of four things, and its constructor says which:
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
--   IO ()@, @crypto.randomUUID :: IO Text@. A method that returns a Promise
--   gives it typed with what it resolves with, for the caller to wait for:
--   @blob.text :: IO (Promise Text)@, then
--   @'Halogen.JSBits.Promise.wait' =<< blob.text@.
--
-- A member whose JavaScript name is a Haskell keyword gets an underscore
-- after its field name: @File@'s @type@ is @f.type_@.
--
-- = Types
--
-- A member's type is built from 'Bool', 'Int', 'Double', 'Text', 'JSVal',
-- a newtype over a 'JSVal' (an object, such as @Navigator@), and 'Maybe'
-- around 'Text', a 'JSVal' or such a newtype, which reads JavaScript's
-- @null@ and @undefined@ as 'Nothing'. An 'IO' result can also be @()@, a
-- 'Data.ByteString.ByteString' (a @Uint8Array@ copied out), or a
-- @'Halogen.JSBits.Promise.Promise' a@. As for any foreign import, the constructor of each newtype in the
-- type must be in scope where the splice is.
--
-- Natively a member's type needs an 'Inert' instance; a newtype over a
-- 'JSVal' derives it. 'Natively' gives a member a native answer of its own
-- instead: core reads an event made from JSON that way.
--
-- = Exceptions
--
-- A JavaScript exception out of a 'Live', 'Writable' or 'Method' member is
-- thrown in Haskell as a 'Halogen.JSBits.Promise.JSError', with the
-- exception's name and message, so the caller catches it as any 'IO'
-- exception (@showModal@ on a dialog that is already open throws an
-- @InvalidStateError@, a selector that does not parse a @SyntaxError@).
-- Without that, a JavaScript exception out of a synchronous import would end
-- the program on WebAssembly, where it cannot be caught as a Haskell
-- exception. A 'Fixed' member is read purely and is not guarded: it is for
-- values whose read does not throw. A Promise's rejection comes back from
-- 'Halogen.JSBits.Promise.waitCatch' as a 'Left'.
--
-- = Where instances go
--
-- An instance belongs in the module that defines the newtype, or in the
-- module of a type the field returns, so that it is not an orphan.
module Halogen.JSBits.Fields
  ( Member (..)
  , ToType (..)
  , jsFields
  , jsFieldFunctions
  , jsCast
  , inlineJS
  )
where

import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Coerce (coerce)
import Data.List (intercalate, isSuffixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Records (HasField (..))
import Halogen.JSBits.Inline (Backend (..), arrow, backend, importJS, inlineJS, splitArrows)
import Halogen.JSBits.Promise (Promise, promise)
import Halogen.JSBits.Value (Inert (..), JSText, JSVal, checkThrown, fromJSBytes, fromJSText, isNull, jsNull, jsValText, toJSText)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)

-- | One member of a JavaScript object: its JavaScript name and the type of
-- its field. See the module's description for what each kind means.
--
-- The type is given as a 'Name' when it is one (@''Bool@, @''Navigator@)
-- or quoted (@[t|Maybe Element|]@).
data Member where
  -- | A value that does not change, read purely: the field has the type.
  Fixed :: (ToType t) => String -> t -> Member
  -- | A value that changes, read in 'IO': the field is @IO a@ for the type
  -- @a@ given.
  Live :: (ToType t) => String -> t -> Member
  -- | A value that changes and that the page can set: the field is @IO a@,
  -- and a second field, @name_set :: a -> IO ()@, sets it.
  Writable :: (ToType t) => String -> t -> Member
  -- | A method: the type is @a -> b -> ... -> IO r@, or @IO r@ for none.
  Method :: (ToType t) => String -> t -> Member
  -- | The member, with this expression as its field natively instead of
  -- 'inert'. It has the field's type (for a 'Writable' member, the
  -- read's). The browsers ignore it.
  Natively :: Q Exp -> Member -> Member

-- | A member's type, as a 'Name' (@''Bool@) or quoted (@[t|Maybe Text|]@).
class ToType t where
  toType :: t -> Q Type

instance ToType Name where toType = conT

-- A quote is polymorphic in its monad (@Quote m => m Type@), so the
-- instance takes any @m a@ and fixes both.
instance (m ~ Q, a ~ Type) => ToType (m a) where toType = id

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

--------------------------------------------------------------------------------
-- Members
--------------------------------------------------------------------------------

-- The declarations for one member: its instances, and in a browser the
-- imports they call.
member :: Name -> Maybe (Q Exp) -> Member -> Q [Dec]
member receiver native = \case
  Natively e m -> member receiver (Just (fromMaybe e native)) m
  Fixed js quoted -> do
    ty <- toType quoted
    shape <- valueShape js ty
    when (shape `elem` [Unit, Bytes, PromiseOf]) $ failFor js "a fixed member cannot be (), a ByteString or a Promise; read it as Live"
    get <- browser native $ do
      raw <- rawName "get" js
      pure ([importJS raw (nullish shape ("$1." <> js)) (self `arrow` ffiType shape ty)], [|\o -> $(pureResult shape) ($(varE raw) o)|])
    pure (field js ty get)
  Live js quoted -> do
    result <- notIO js =<< toType quoted
    shape <- valueShape js result
    let ty = AppT (ConT ''IO) result
    get <- browser native (readLive js shape result)
    pure (field js ty get)
  Writable js quoted -> do
    result <- notIO js =<< toType quoted
    shape <- valueShape js result
    let ty = AppT (ConT ''IO) result
    get <- browser native (readLive js shape result)
    argument <- argumentShape js result
    set <- browser Nothing $ do
      (raw, decls, guard) <- guarded "set" js ("$1." <> js <> " = $2") Unit (TupleT 0) [self, ffiType argument result]
      v <- newName "v"
      pure (decls, [|\o -> $(lamE [varP v] (guard [|$(varE raw) o $(toForeign argument (varE v))|]))|])
    pure (field js ty get <> field (js <> "_set") (result `arrow` AppT (ConT ''IO) (TupleT 0)) set)
  Method js quoted -> do
    ty <- toType quoted
    let (arguments, io) = splitArrows ty
    (shape, result) <- ioResult js io
    shapes <- mapM (argumentShape js) arguments
    call <- browser native $ do
      (raw, decls, guard) <- guarded "call" js (callOf js (length arguments)) shape result (self : zipWith ffiType shapes arguments)
      vs <- mapM (const (newName "a")) arguments
      o <- newName "o"
      let applied = foldl appE (varE raw) (varE o : zipWith toForeign shapes (map varE vs))
      pure (decls, lamE (varP o : map varP vs) (guard applied))
    pure (field js ty call)
  where
    self = ConT receiver
    typeName = nameBase receiver
    rawName kind js = newName ("js_" <> kind <> "_" <> typeName <> "_" <> js)
    readLive js shape result = do
      (raw, decls, guard) <- guarded "get" js ("$1." <> js) shape result [self]
      pure (decls, [|\o -> $(guard [|$(varE raw) o|])|])
    -- An IO member's call, made so that a JavaScript exception out of it is
    -- thrown in Haskell as a 'JSError'. The import catches the exception
    -- and returns an outcome object, @{ok, value}@ or @{ok: false, error}@;
    -- 'checkThrown' throws its error, and a second import reads its value
    -- with the FFI's conversion for the member's type. The outcome is made
    -- and read with nothing in between, so no other Haskell thread can see
    -- or take it.
    guarded kind js expression shape result ffiArguments = do
      raw <- rawName kind js
      let caught = "(() => { try { return { ok: true, value: " <> expression <> " }; } catch (e) { return { ok: false, error: e }; } })()"
          outcome = importJS raw caught (foldr arrow (AppT (ConT ''IO) (ConT ''JSVal)) ffiArguments)
      if shape == Unit
        then pure (raw, [outcome], \call -> [|$call >>= checkThrown|])
        else do
          value <- rawName (kind <> "_value") js
          let readValue = importJS value (nullish shape "$1.value") (ConT ''JSVal `arrow` AppT (ConT ''IO) (ffiType shape result))
          r <- newName "r"
          pure (raw, [outcome, readValue], \call -> [|$call >>= \ $(varP r) -> checkThrown $(varE r) >> $(ioFrom shape [|$(varE value) $(varE r)|])|])
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
  | -- | A @'Promise' a@, a result only: the JavaScript promise as a
    -- 'JSVal', made a 'Promise' (listened to) with 'promise'.
    PromiseOf
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
      AppT (ConT p) _ | p == ''Promise -> Just PromiseOf
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
  when (shape `elem` [Unit, Bytes, PromiseOf]) $ failFor js ("cannot take an argument of type " <> pprint ty <> " (a Promise goes as its rawPromise, a JSVal)")
  pure shape

ffiType :: Shape -> Type -> Type
ffiType shape ty = case shape of
  AsText -> ConT ''JSText
  MaybeText -> ConT ''JSVal
  MaybeValue -> ConT ''JSVal
  Bytes -> ConT ''JSVal
  PromiseOf -> ConT ''JSVal
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
  PromiseOf -> [|$call >>= promise|]
  _ -> [|fmap $(pureResult shape) $call|]

-- From the field's argument to what the import takes.
toForeign :: Shape -> Q Exp -> Q Exp
toForeign shape v = case shape of
  AsText -> [|toJSText $v|]
  MaybeText -> [|maybe jsNull toJSText $v|]
  MaybeValue -> [|maybe jsNull coerce $v|]
  _ -> v

-- A live or writable member's type is the value's: the IO is the
-- generator's.
notIO :: String -> Type -> Q Type
notIO js = \case
  AppT (ConT io) _ | io == ''IO -> failFor js "give the value's type, without IO: a live member is read in IO already"
  ty -> pure ty

failFor :: String -> String -> Q a
failFor js message = fail ("jsFields: the member " <> js <> ": " <> message)
