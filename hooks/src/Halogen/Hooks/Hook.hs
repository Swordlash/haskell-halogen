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
--   -> Hook q slots output m (UseCounter hooks) hooks (Int, StateId Int)
-- useCounter start = Hooks.do
--   (count, countId) <- Hooks.useState start
--   Hooks.useTickEffect count $ Hooks.pure Nothing
--   Hooks.pure (count, countId)
-- @
--
-- The @Hooks.do@ is @QualifiedDo@ over the operators this module exports; an
-- indexed monad has no 'Monad' instance to give @do@ otherwise.
module Halogen.Hooks.Hook
  ( Hook (..)

    -- * The hooks
  , useState
  , useLifecycleEffect
  , useTickEffect
  , useMemo
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
import Halogen.Hooks.HookM (HookAction, HookM)
import Halogen.Hooks.Types (HookK (..), StateId)
import Protolude hiding (fmap, pure, return, void, (<$>), (<*>), (>>), (>>=))

type Hook
  :: (Type -> Type)
  -> Row Type
  -> Type
  -> (Type -> Type)
  -> [HookK]
  -> [HookK]
  -> Type
  -> Type

-- | A program that, given the hooks @i@, uses some of them and leaves @o@,
-- producing an @a@. A whole component is a @Hook q slots output m hooks '[]
-- (HookHTML slots output m)@: it consumes exactly the hooks it declares.
--
-- @q@ is the component's query algebra, present so that 'useQuery' can be
-- checked against it. The PureScript original passes a token around instead
-- and coerces the handler back into place when a query arrives.
data Hook q slots output m i o a where
  HPure :: a -> Hook q slots output m i i a
  HBind :: Hook q slots output m i j a -> (a -> Hook q slots output m j o b) -> Hook q slots output m i o b
  HState :: s -> Hook q slots output m (UseState s : i) i (s, StateId s)
  HEffect
    :: (Eq deps)
    => deps
    -> HookM slots output m (Maybe (HookAction slots output m))
    -> Hook q slots output m (UseEffect deps : i) i ()
  HMemo :: (Eq deps) => deps -> (deps -> a) -> Hook q slots output m (UseMemo deps a : i) i a
  HRef :: a -> Hook q slots output m (UseRef a : i) i (a, IORef a)
  HQuery :: (forall a. q a -> HookM slots output m (Maybe a)) -> Hook q slots output m (UseQuery : i) i ()

-- | A piece of state owned by this hook, and a handle for changing it.
--
-- The value is the one this render sees; the handle is stable across renders,
-- and writing through it schedules another one.
useState :: forall s q slots output m i. s -> Hook q slots output m (UseState s : i) i (s, StateId s)
useState = HState

-- | An effect run once, after the component's first render. If it returns a
-- program, that program runs when the component is finalized.
--
-- This is 'useTickEffect' on dependencies that cannot change.
useLifecycleEffect
  :: forall q slots output m i
   . HookM slots output m (Maybe (HookAction slots output m))
  -> Hook q slots output m (UseEffect () : i) i ()
useLifecycleEffect = HEffect ()

-- | An effect run after the first render, and again after any render in which
-- the dependencies changed. The cleanup it returns runs before the next run of
-- the effect, and when the component is finalized.
--
-- The dependencies are an ordinary value compared with '=='. The PureScript
-- original compares an array of coerced values captured by @Hooks.captures@,
-- which is the same idea with the types erased.
useTickEffect
  :: forall deps q slots output m i
   . (Eq deps)
  => deps
  -> HookM slots output m (Maybe (HookAction slots output m))
  -> Hook q slots output m (UseEffect deps : i) i ()
useTickEffect = HEffect

-- | A value computed from its dependencies, recomputed only when they change.
useMemo :: forall deps a q slots output m i. (Eq deps) => deps -> (deps -> a) -> Hook q slots output m (UseMemo deps a : i) i a
useMemo = HMemo

-- | A mutable reference that survives renders, together with the value it held
-- at the time of this render.
--
-- Writing to the reference does /not/ schedule a render; that is the
-- difference between this and 'useState'.
useRef :: forall a q slots output m i. a -> Hook q slots output m (UseRef a : i) i (a, IORef a)
useRef = HRef

-- | Handle queries sent to this component by its parent.
--
-- The handler is replaced on every render, so it sees the current values of
-- everything above it. Only the last 'useQuery' in a program takes effect.
useQuery
  :: forall q slots output m i
   . (forall a. q a -> HookM slots output m (Maybe a))
  -> Hook q slots output m (UseQuery : i) i ()
useQuery = HQuery

-- | A program that uses no hooks.
pure :: forall a q slots output m i. a -> Hook q slots output m i i a
pure = HPure

-- | 'pure', for @QualifiedDo@ that ends in @return@.
return :: forall a q slots output m i. a -> Hook q slots output m i i a
return = HPure

infixl 1 >>=, >>

-- | Run one hook program after another, the second using what the first left.
(>>=) :: forall a b q slots output m i j o. Hook q slots output m i j a -> (a -> Hook q slots output m j o b) -> Hook q slots output m i o b
(>>=) = HBind

-- | '>>=' discarding the result.
(>>) :: forall a b q slots output m i j o. Hook q slots output m i j a -> Hook q slots output m j o b -> Hook q slots output m i o b
ma >> mb = HBind ma (const mb)

fmap :: forall a b q slots output m i o. (a -> b) -> Hook q slots output m i o a -> Hook q slots output m i o b
fmap f ma = HBind ma (HPure . f)

infixl 4 <$>, <*>

(<$>) :: forall a b q slots output m i o. (a -> b) -> Hook q slots output m i o a -> Hook q slots output m i o b
(<$>) = fmap

(<*>) :: forall a b q slots output m i j o. Hook q slots output m i j (a -> b) -> Hook q slots output m j o a -> Hook q slots output m i o b
mf <*> ma = HBind mf (\f -> fmap f ma)

void :: forall a q slots output m i o. Hook q slots output m i o a -> Hook q slots output m i o ()
void = fmap (const ())
