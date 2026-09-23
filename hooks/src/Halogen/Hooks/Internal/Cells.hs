-- | What a hook program leaves behind between renders.
--
-- One cell per hook, in the order the hooks were used, indexed by the very
-- same list that indexes the program. The interpreter therefore walks the two
-- together and reads each cell back at the type it was written: a state cell
-- of a @UseState s@ hook is an @IORef s@ and nothing else, so no coercion is
-- needed to read it. The PureScript original keeps arrays of coerced values
-- and an index per array, and pays for it with @unsafeCoerce@ at every read.
module Halogen.Hooks.Internal.Cells
  ( Cells (..)
  , EffectCell (..)
  , cleanups
  )
where

import Data.IORef (IORef, readIORef)
import Data.Row (Row)
import Halogen.Hooks.Internal.HookM (HookAction)
import Halogen.Hooks.Internal.Types (HookK (..))
import Protolude

-- | What an effect hook remembers: the dependencies it last ran on, and the
-- cleanup it left.
data EffectCell scope deps slots output m = EffectCell
  { deps :: deps
  , cleanup :: Maybe (HookAction scope slots output m)
  }

type Cells :: Type -> Row Type -> Type -> (Type -> Type) -> [HookK] -> Type

-- | The cells of a hook program, in use order.
data Cells scope slots output m i where
  CNil :: Cells scope slots output m '[]
  CState :: IORef s -> Cells scope slots output m i -> Cells scope slots output m (UseState s : i)
  CEffect :: IORef (EffectCell scope deps slots output m) -> Cells scope slots output m i -> Cells scope slots output m (UseEffect deps : i)
  CMemo :: IORef (deps, a) -> Cells scope slots output m i -> Cells scope slots output m (UseMemo deps a : i)
  CRef :: IORef a -> Cells scope slots output m i -> Cells scope slots output m (UseRef a : i)
  -- | A query hook keeps nothing here: its handler is replaced on every render
  -- and so lives with the rest of the per-render state.
  CQuery :: Cells scope slots output m i -> Cells scope slots output m (UseQuery : i)

-- | Every cleanup the effect hooks have outstanding, in use order. Run when
-- the component is finalized.
cleanups :: forall scope slots output m i. Cells scope slots output m i -> IO [HookAction scope slots output m]
cleanups = \case
  CNil -> pure []
  CState _ rest -> cleanups rest
  CEffect ref rest -> do
    cell <- readIORef ref
    rest' <- cleanups rest
    pure $ maybe rest' (: rest') cell.cleanup
  CMemo _ rest -> cleanups rest
  CRef _ rest -> cleanups rest
  CQuery rest -> cleanups rest
