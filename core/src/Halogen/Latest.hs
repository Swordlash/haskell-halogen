-- | The latest answer wins.
--
-- A component asks for something and takes the answer when it comes. Two
-- asks of one kind can be out at once: a reload during a load, or a route
-- changed and changed back. Their answers can come in the other order, and
-- then the older answer stays on the screen.
--
-- This module gives each ask a ticket. The rule: an answer is taken as soon
-- as it comes, unless an answer to a later ask was taken already. So an
-- earlier ask's answer shows until the later one's comes, and never after.
-- A failure is an answer like any other: take it with a ticket too.
--
-- = An example
--
-- This component loads a page when its input changes, and again on a
-- click. It takes a ticket with 'asking' in the step that starts the load.
-- The fork takes the answer with 'answered', in one step.
--
-- > import Control.Monad.State.Class (gets, modify)
-- > import Halogen qualified as H
-- > import Halogen.Latest (Asks, answered, asking, noAsks, supersede)
-- >
-- > data State = State {slug :: Text, page :: Maybe (Either Text Page), loads :: Asks}
-- >
-- > data Action = Reload
-- >
-- > pageView :: (MonadIO m) => H.Component q Text o m
-- > pageView =
-- >   H.component
-- >     (\slug -> pure State {slug, page = Nothing, loads = noAsks})
-- >     render
-- >     [ H.onInitialize load
-- >     , H.onReceive $ \slug -> do
-- >         -- Another page: no answer for the old one may show.
-- >         modify (\s -> s {slug, page = Nothing, loads = supersede s.loads})
-- >         load
-- >     , H.onAction $ \Reload -> load
-- >     ]
-- >   where
-- >     load = do
-- >       slug <- gets (.slug)
-- >       ask <- asking (.loads) (\a s -> s {loads = a})
-- >       void $ H.fork $ do
-- >         result <- liftIO (fetchPage slug)
-- >         void $ answered (.loads) (\a s -> s {loads = a}) ask (\s -> s {page = Just result})
--
-- A reload keeps the page on the screen until the new one comes. A new
-- slug clears it, and 'supersede' drops every answer still out for the old
-- slug.
--
-- The getter and the setter are two functions, because core has no lens
-- library. With @NoFieldSelectors@ the getter is a record dot section,
-- @(.loads)@, and the setter a lambda.
--
-- = Tickets or 'Halogen.Query.HalogenM.kill'?
--
-- A component can also kill the fork of the older ask when it makes a new
-- one. A killed fork runs nothing more. That holds even when its answer has
-- come and its next step waits in the loop's queue: the step checks first
-- that the fork is not over, and the kill ran on the loop before it. So
-- kill keeps a stale answer out too, for an answer that comes back to a
-- fork. Use tickets when:
--
-- * The answer does not come back to a fork the component can kill. It can
--   come from a channel's push, a parent's query, or a subscription.
--
-- * The ask is a write. A killed fork stops waiting, but a request it sent
--   stays sent. Its answer must still be handled, in order.
--
-- * The earlier answer should show until the later one comes, as on a slow
--   network. With kill alone, nothing shows until the latest returns.
--
-- Kill to save work that is certainly useless: it also aborts a @fetch@ of
-- @haskell-halogen-net@. Use tickets to keep the order. Often use both.
--
-- = Two things to watch for
--
-- Tickets are not enough over a cache. A later ask answered at once from a
-- cache can bring an older answer than an earlier ask answered by the
-- server. Keep what only goes one way, such as a "read" mark, from every
-- answer, taken or not.
--
-- "The same one" is a number, never an equal value. The same page visited
-- twice is two visits. A view mounted again is a new view. The same text
-- quoted twice is two quotes. Use a ticket or a counter wherever identity
-- matters.
module Halogen.Latest
  ( -- * Tickets
    Asks
  , Ask
  , noAsks
  , newAsk
  , takes
  , supersede

    -- * In a handler
  , asking
  , answered
  )
where

-- HPrelude's 'ask' and 'asks' are the Reader's. This module uses the names.
import HPrelude hiding (ask, asks)

-- | The asks of one kind a component has made, and which answer it holds.
-- Keep one in the component's state for each kind of ask.
data Asks = Asks
  { next :: Int
  -- ^ The next ticket's number.
  , settled :: Int
  -- ^ Answers to asks numbered up to this one are not taken.
  }
  deriving stock (Eq, Show)

-- | One ask's ticket. Each ticket of an 'Asks' has a new number. Use a
-- ticket only with the 'Asks' it came from.
newtype Ask = Ask Int
  deriving stock (Eq, Show)

-- | No asks made yet.
noAsks :: Asks
noAsks = Asks {next = 1, settled = 0}

-- | Take a ticket for a new ask. Do it in the step that starts the ask.
newAsk :: Asks -> (Ask, Asks)
newAsk asks = (Ask asks.next, asks {next = asks.next + 1})

-- | Whether to take this ask's answer. 'Just' the new 'Asks' when no answer
-- to a later ask was taken, and the ask was not superseded: take the
-- answer, and keep the new 'Asks'. 'Nothing' when the answer is too old:
-- drop it.
takes :: Ask -> Asks -> Maybe Asks
takes (Ask n) asks
  | n > asks.settled = Just asks {settled = n}
  | otherwise = Nothing

-- | Make every ask that is out too old. Use it when the input changes, or
-- the view leaves, and no answer to an earlier ask may show. Asks made
-- after it are taken as usual.
supersede :: Asks -> Asks
supersede asks = asks {settled = asks.next - 1}

-- | Take a ticket in a handler, in one step of the program. The first two
-- arguments read and write the component's 'Asks'.
asking :: (MonadState st m) => (st -> Asks) -> (Asks -> st -> st) -> m Ask
asking getAsks setAsks = state $ \st -> let (ask, asks) = newAsk (getAsks st) in (ask, setAsks asks st)

-- | Take an answer in a handler, if it is not too old: apply the change to
-- the state, and return 'True'. Return 'False' and change nothing for an
-- answer that is too old. It compares and changes in one step of the
-- program, so nothing can come in between.
answered :: (MonadState st m) => (st -> Asks) -> (Asks -> st -> st) -> Ask -> (st -> st) -> m Bool
answered getAsks setAsks ask change = state $ \st -> case takes ask (getAsks st) of
  Just asks -> (True, change (setAsks asks st))
  Nothing -> (False, st)
