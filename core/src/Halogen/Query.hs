-- | Functions and types used to describe the `HalogenF` algebra used in a
-- | component's `eval` function.
module Halogen.Query
  ( Tell
  , mkTell
  , tell
  , tellAll
  , Request
  , mkRequest
  , request
  , requestAll
  , getHTMLElementRef
  , newRefLabel
  , module Halogen.Query.Input
  , module Halogen.Query.HalogenM
  , module Halogen.Query.HalogenQ
  )
where

import Control.Monad.UUID (MonadUUID (..))
import Data.Row as Row
import Data.UUID.Types qualified as UUID
import HPrelude
import Halogen.Data.Slot (Slot)
import Halogen.Query.HalogenM (ForkId, HalogenF (..), HalogenM (..), SubscriptionId, fork, getRef, join, kill, query, queryAll, raise, subscribe, subscribe', unsubscribe)
import Halogen.Query.HalogenQ (HalogenQ (..))
import Halogen.Query.Input (RefLabel (..))
import Web.DOM.Internal.Types

-- | Type synonym for a "tell-style" query - queries that only cause effects,
-- | but that cannot receive a return value.
-- |
-- | In a query algebra, a tell constructor carries the algebra's type variable
-- | as its last argument. For example:
-- |
-- | ``` purescript
-- | data Query a
-- |   = SomeTell a
-- |   | SomeOtherTell String a
-- |   | NotATell (Boolean -> a)
-- | ```
-- |
-- | Both `SomeTell` and `SomeOtherTell` carry a plain `a` as a value, whereas
-- | `NotATell` has `a` as the result of a function so is considered to be a
-- | "request" ([see below](#Request)).
type Tell f = () -> f ()

-- | Takes a data constructor of query algebra `f` and creates a tell query.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = Tick a
-- |
-- | sendTick :: forall o. H.HalogenSocket Query o IO -> IO (Maybe ())
-- | sendTick app = app.query (H.mkTell Tick)
-- | ```
mkTell :: forall f. Tell f -> f ()
mkTell act = act ()

tell
  :: forall label
  ->forall state action output m slots query output' slot
   . (HasType label (Slot query output' slot) slots, Functor m)
  => (KnownSymbol label)
  => (Ord slot)
  => slot
  -> Tell query
  -> HalogenM state action slots output m ()
tell label slot req = void $ query label slot (req ())

tellAll
  :: forall label
  ->forall state action output m slots query output' slot
   . (HasType label (Slot query output' slot) slots, Functor m)
  => (KnownSymbol label)
  => (Ord slot)
  => Tell query
  -> HalogenM state action slots output m ()
tellAll label req = void $ queryAll label (req ())

-- | Type synonym for an "request-style" query - queries that can cause effects
-- | as well as fetching some information from a component.
-- |
-- | In a query algebra, a request constructor carries the algebra's type
-- | variable as the return value of a function as its last argument. For
-- | example:
-- |
-- | ``` purescript
-- | data Query a = SomeRequest (Boolean -> a)
-- | ```
type Request f a = (a -> a) -> f a

-- | Takes a data constructor of query algebra `f` and creates a request query.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = GetTickCount (Int -> a)
-- |
-- | getTickCount :: forall o. H.HalogenSocket Query o IO -> IO (Maybe Int)
-- | getTickCount app = app.query (H.mkRequest GetTickCount)
-- | ```
mkRequest :: forall f a. Request f a -> f a
mkRequest req = req identity

request
  :: forall label
  ->forall state action output m slots query output' slot a
   . (HasType label (Slot query output' slot) slots, Functor m)
  => (KnownSymbol label)
  => (Ord slot)
  => slot
  -> Request query a
  -> HalogenM state action slots output m (Maybe a)
request slot label req = query slot label (req identity)

requestAll
  :: forall label
  ->forall state action output m slots query output' slot a
   . (HasType label (Slot query output' slot) slots, Functor m)
  => (KnownSymbol label)
  => (Ord slot)
  => Request query a
  -> HalogenM state action slots output m (Map slot a)
requestAll label req = queryAll label (req identity)

-- | Retrieves a `HTMLElement` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value (or
-- | it is not an `HTMLElement`) for the request will return `Nothing`.
getHTMLElementRef
  :: forall state action slots output m
   . (Functor m)
  => RefLabel
  -> HalogenM state action slots output m (Maybe HTMLElement)
getHTMLElementRef = map (fromElement =<<) . getRef

-- | A ref label no other can share, named after what it labels.
--
-- Refs are looked up by label in the component that rendered them, and that
-- includes HTML a component renders on its parent's behalf: a tab bar or a
-- list shows markup it was handed, and a ref in that markup lands among the
-- tab bar's own. A fixed label can therefore collide with one it has never
-- seen. Generate one in @initialState@ and keep it in the state.
newRefLabel :: (MonadUUID m) => Text -> m RefLabel
newRefLabel name = RefLabel . ((name <> "-") <>) . UUID.toText <$> generateV4
