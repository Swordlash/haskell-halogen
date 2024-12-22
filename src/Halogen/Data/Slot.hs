module Halogen.Data.Slot where

import Data.Map.Strict qualified as M
import Data.Row
import Data.Set qualified as S
import GHC.TypeLits (sameSymbol)
import HPrelude

data Slot (query :: Type -> Type) (output :: Type) (slotType :: Type)

-- some element of the `slots` row type, ordered by label and slot Ord instance
data SlotElem (slots :: Row Type) (slot :: (Type -> Type) -> Type -> Type) where
  SlotElem
    :: (HasType sym (Slot query output s) slots, KnownSymbol sym, Ord s)
    => Proxy sym
    -> s
    -> slot query output
    -> SlotElem slots slot

instance Eq (SlotElem slots slot) where
  SlotElem p s _ == SlotElem p' s' _ =
    Just True == do
      Refl <- sameSymbol p p'
      pure $ s == s'

instance Ord (SlotElem slots slot) where
  SlotElem p s _ `compare` SlotElem p' s' _ =
    case sameSymbol p p' of
      Just Refl -> s `compare` s'
      Nothing -> symbolVal p `compare` symbolVal p'

newtype SlotStorage slots slot = SlotStorage (Set (SlotElem slots slot))

lookup
  :: ( HasType sym (Slot query output s) slots
     , KnownSymbol sym
     , Ord s
     )
  => Proxy sym
  -> s
  -> SlotStorage slots slot
  -> Maybe (slot query output)
lookup symb key (SlotStorage s) = do
  SlotElem sym' key' slot <- S.lookupGE (SlotElem symb key (fix identity)) s
  Refl <- sameSymbol symb sym'
  guard (key == key')
  pure slot

empty :: SlotStorage slots slot
empty = SlotStorage S.empty

pop
  :: ( HasType sym (Slot query output s) slots
     , KnownSymbol sym
     , Ord s
     )
  => Proxy sym
  -> s
  -> SlotStorage slots slot
  -> Maybe (slot query output, SlotStorage slots slot)
pop symb key stor@(SlotStorage s) = do
  slot <- lookup symb key stor
  pure (slot, SlotStorage $ S.delete (SlotElem symb key slot) s)

insert
  :: ( HasType sym (Slot query output s) slots
     , KnownSymbol sym
     , Ord s
     )
  => Proxy sym
  -> s
  -> slot query output
  -> SlotStorage slots slot
  -> SlotStorage slots slot
insert symb key slot = coerce (S.insert (SlotElem symb key slot))

slots
  :: ( HasType sym (Slot query output s) slots
     , KnownSymbol sym
     , Ord s
     )
  => Proxy sym
  -> SlotStorage slots slot
  -> Map s (slot query output)
slots symb (SlotStorage s) =
  M.fromAscList $ mapMaybe flt $ S.toAscList s
  where
    flt (SlotElem symb' key' slot) = do
      Refl <- sameSymbol symb symb'
      pure (key', slot)

foreachSlot
  :: (Applicative m)
  => SlotStorage slots slot
  -> (forall query output. slot query output -> m ())
  -> m ()
foreachSlot (SlotStorage s) act =
  for_ @_ @_ @_ @() s $ \(SlotElem _ _ v) -> act v -- no idea why I need to type apply
