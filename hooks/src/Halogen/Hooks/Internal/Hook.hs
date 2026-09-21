-- | The hook program itself: an indexed monad whose two indices say which
-- hooks the program is going to use.
--
-- A hook program is re-run on every render, and the interpreter walks a store
-- of cells in step with it, so the hooks a program uses have to come in the
-- same order every time — React's "rules of hooks". Here that is not a rule
-- but a type: @i@ is the list of hooks the program consumes and @o@ what is
-- left for whatever runs after it, so a @useState@ in one branch of an @if@
-- and nothing in the other simply does not type-check.
--
-- Because the indices thread left to right, the list reads in the order the
-- hooks are written, and a composite hook is a parameterised type synonym:
--
-- @
-- type UseCounter hooks = UseState Int : UseEffect Int : hooks
--
-- useCounter
--   :: Int
--   -> Hook scope q slots output m (UseCounter hooks) hooks (Int, StateId Int)
-- useCounter start = Hooks.do
--   (count, countId) <- Hooks.useState start
--   Hooks.useTickEffect count $ Hooks.pure Nothing
--   Hooks.pure (count, countId)
-- @
--
-- The @Hooks.do@ is @QualifiedDo@ over the operators this module exports; an
-- indexed monad has no 'Monad' instance to give @do@ otherwise.
module Halogen.Hooks.Internal.Hook
  ( Hook (..)
  , HookFn

    -- * The hooks
  , useState
  , useLifecycleEffect
  , useTickEffect
  , useTickEffectBy
  , useMemo
  , useMemoBy
  , useRef
  , useQuery

    -- * @QualifiedDo@ support
  , pure
  , return
  , (>>=)
  , (>>)
  , fmap
  , (<$>)
  , (<*>)
  , void
  )
where

import Data.IORef (IORef)
import Data.Row (Row)
import Halogen.Hooks.Internal.HookM (HookAction, HookHTML, HookM)
import Halogen.Hooks.Internal.Types (HookK (..), StateId)
import Protolude hiding (fmap, pure, return, void, (<$>), (<*>), (>>), (>>=))

type Hook
  :: Type
  -> (Type -> Type)
  -> Row Type
  -> Type
  -> (Type -> Type)
  -> [HookK]
  -> [HookK]
  -> Type
  -> Type

-- | A program that, given the hooks @i@, uses some of them and leaves @o@,
-- producing an @a@. A whole component is a @Hook scope q slots output m hooks '[]
-- (HookHTML slots output m)@: it consumes exactly the hooks it declares.
--
-- @q@ is the component's query algebra, present so that 'useQuery' can be
-- checked against it. The PureScript original passes a token around instead
-- and coerces the handler back into place when a query arrives.
data Hook scope q slots output m i o a where
  HPure :: a -> Hook scope q slots output m i i a
  HBind :: Hook scope q slots output m i j a -> (a -> Hook scope q slots output m j o b) -> Hook scope q slots output m i o b
  HState :: s -> Hook scope q slots output m (UseState s : i) i (s, StateId scope s)
  HEffect
    :: (deps -> deps -> Bool)
    -> deps
    -> HookM scope slots output m (Maybe (HookAction scope slots output m))
    -> Hook scope q slots output m (UseEffect deps : i) i ()
  HMemo :: (deps -> deps -> Bool) -> deps -> (deps -> a) -> Hook scope q slots output m (UseMemo deps a : i) i a
  HRef :: a -> Hook scope q slots output m (UseRef a : i) i (a, IORef a)
  HQuery :: (forall a. q a -> HookM scope slots output m (Maybe a)) -> Hook scope q slots output m (UseQuery : i) i ()

-- | What a hooks component is written as: one function from input to HTML,
-- consuming exactly the hooks it declares.
type HookFn scope q input slots output m hooks =
  input -> Hook scope q slots output m hooks '[] (HookHTML scope slots output m)

-- | A piece of state owned by this hook, and a handle for changing it.
--
-- The value is the one this render sees; the handle is stable across renders,
-- and writing through it schedules another one.
useState :: forall s scope q slots output m i. s -> Hook scope q slots output m (UseState s : i) i (s, StateId scope s)
useState = HState

-- | An effect run once, after the component's first render. If it returns a
-- program, that program runs when the component is finalized.
--
-- This is 'useTickEffect' on dependencies that cannot change.
useLifecycleEffect
  :: forall scope q slots output m i
   . HookM scope slots output m (Maybe (HookAction scope slots output m))
  -> Hook scope q slots output m (UseEffect () : i) i ()
useLifecycleEffect = HEffect (\_ _ -> True) ()

-- | An effect run after the first render, and again after any render in which
-- the dependencies changed. The cleanup it returns runs before the next run of
-- the effect, and when the component is finalized.
--
-- The dependencies are an ordinary value compared with '=='. The PureScript
-- original compares an array of coerced values captured by @Hooks.captures@,
-- which is the same idea with the types erased.
useTickEffect
  :: forall deps scope q slots output m i
   . (Eq deps)
  => deps
  -> HookM scope slots output m (Maybe (HookAction scope slots output m))
  -> Hook scope q slots output m (UseEffect deps : i) i ()
useTickEffect = useTickEffectBy (==)

-- | 'useTickEffect' with the comparison spelled out, for dependencies that
-- have no 'Eq' instance or an expensive one: a function, a large structure a
-- cheap summary stands in for, a value only worth comparing by one field.
useTickEffectBy
  :: forall deps scope q slots output m i
   . (deps -> deps -> Bool)
  -- ^ whether the dependencies are unchanged since the last run
  -> deps
  -> HookM scope slots output m (Maybe (HookAction scope slots output m))
  -> Hook scope q slots output m (UseEffect deps : i) i ()
useTickEffectBy = HEffect

-- | A value computed from its dependencies, recomputed only when they change.
useMemo :: forall deps a scope q slots output m i. (Eq deps) => deps -> (deps -> a) -> Hook scope q slots output m (UseMemo deps a : i) i a
useMemo = useMemoBy (==)

-- | 'useMemo' with the comparison spelled out. See 'useTickEffectBy'.
useMemoBy
  :: forall deps a scope q slots output m i
   . (deps -> deps -> Bool)
  -- ^ whether the dependencies are unchanged since the last computation
  -> deps
  -> (deps -> a)
  -> Hook scope q slots output m (UseMemo deps a : i) i a
useMemoBy = HMemo

-- | A mutable reference that survives renders, together with the value it held
-- at the time of this render.
--
-- Writing to the reference does /not/ schedule a render; that is the
-- difference between this and 'useState'.
useRef :: forall a scope q slots output m i. a -> Hook scope q slots output m (UseRef a : i) i (a, IORef a)
useRef = HRef

-- | Handle queries sent to this component by its parent.
--
-- The handler is replaced on every render, so it sees the current values of
-- everything above it. Only the last 'useQuery' in a program takes effect.
useQuery
  :: forall scope q slots output m i
   . (forall a. q a -> HookM scope slots output m (Maybe a))
  -> Hook scope q slots output m (UseQuery : i) i ()
useQuery = HQuery

-- | A program that uses no hooks.
pure :: forall a scope q slots output m i. a -> Hook scope q slots output m i i a
pure = HPure

-- | 'pure', for @QualifiedDo@ that ends in @return@.
return :: forall a scope q slots output m i. a -> Hook scope q slots output m i i a
return = HPure

infixl 1 >>=, >>

-- | Run one hook program after another, the second using what the first left.
(>>=) :: forall a b scope q slots output m i j o. Hook scope q slots output m i j a -> (a -> Hook scope q slots output m j o b) -> Hook scope q slots output m i o b
(>>=) = HBind

-- | '>>=' discarding the result.
(>>) :: forall a b scope q slots output m i j o. Hook scope q slots output m i j a -> Hook scope q slots output m j o b -> Hook scope q slots output m i o b
ma >> mb = HBind ma (const mb)

fmap :: forall a b scope q slots output m i o. (a -> b) -> Hook scope q slots output m i o a -> Hook scope q slots output m i o b
fmap f ma = HBind ma (HPure . f)

infixl 4 <$>, <*>

(<$>) :: forall a b scope q slots output m i o. (a -> b) -> Hook scope q slots output m i o a -> Hook scope q slots output m i o b
(<$>) = fmap

(<*>) :: forall a b scope q slots output m i j o. Hook scope q slots output m i j (a -> b) -> Hook scope q slots output m j o a -> Hook scope q slots output m i o b
mf <*> ma = HBind mf (\f -> fmap f ma)

void :: forall a scope q slots output m i o. Hook scope q slots output m i o a -> Hook scope q slots output m i o ()
void = fmap (const ())
