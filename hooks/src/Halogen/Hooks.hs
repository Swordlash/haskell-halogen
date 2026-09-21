-- | Hooks for @haskell-halogen-core@, after
-- <https://github.com/thomashoneyman/purescript-halogen-hooks purescript-halogen-hooks>.
--
-- A component written with hooks is one function from its input to its HTML.
-- Whatever it needs to do that — state, effects, memoised values, a query
-- handler — it asks for along the way, and the parts of a component that are
-- ordinarily spread across @initialState@, @render@ and @handleAction@ stay
-- next to each other.
--
-- @
-- {-\# LANGUAGE QualifiedDo \#-}
--
-- import Halogen.Hooks qualified as Hooks
--
-- counter :: H.Component H.VoidF () Void BrowserDOM
-- counter = Hooks.component $ \\_input -> Hooks.do
--   (count, countId) <- Hooks.useState (0 :: Int)
--
--   -- an effect's body is ordinary 'HookM', so it is an ordinary @do@
--   Hooks.useTickEffect count $ do
--     liftIO $ putStrLn ("count is now " <> show count)
--     pure Nothing
--
--   Hooks.pure $
--     HH.div_
--       [ HH.button [HE.onClick $ \\_ -> Hooks.modify_ countId (+ 1)] [HH.text "more"]
--       , HH.text (show count)
--       ]
-- @
--
-- The @Hooks.do@ is @QualifiedDo@: a hook program is an indexed monad, and
-- what it is indexed by is the list of hooks it uses. That is what enforces
-- the rules of hooks — the same hooks in the same order on every render — and
-- it enforces them as a type error rather than as a runtime check. A hook
-- inside an @if@ or a @for@ will not compile.
--
-- Two differences from the PureScript original are worth knowing if you are
-- porting code across:
--
--   * dependencies are an ordinary value compared with '==', so
--     @Hooks.captures {x, y} Hooks.useTickEffect@ becomes
--     @Hooks.useTickEffect (x, y)@;
--
--   * there is no @componentWithQuery@ and there are no tokens. The query
--     algebra is part of the hook program's type, so 'useQuery' is checked
--     against the component it ends up in, and 'raise' and 'query' need
--     nothing passed to them.
module Halogen.Hooks
  ( -- * Building a component
    component

    -- * Hook programs
  , Hook
  , HookK (..)
  , HookFn
  , HookHTML

    -- ** The hooks
  , useState
  , useLifecycleEffect
  , useTickEffect
  , useMemo
  , useRef
  , useQuery

    -- ** @QualifiedDo@ support
  , pure
  , return
  , (>>=)
  , (>>)
  , fmap
  , (<$>)
  , (<*>)
  , void

    -- * Handlers
  , HookM
  , HookAction
  , StateId

    -- ** State
  , get
  , put
  , modify
  , modify_

    -- ** Talking to the component's surroundings
  , raise
  , query
  , queryAll

    -- ** Subscriptions
  , subscribe
  , subscribe'
  , unsubscribe

    -- ** Forks
  , fork
  , kill

    -- ** Refs
  , getRef
  )
where

import Halogen.Component (Component, ComponentSpec' (..), mkComponent)
import Halogen.Hooks.Hook
import Halogen.Hooks.HookM
import Halogen.Hooks.Internal.Eval (HookFn, HookState (..), evalHook, initialHookState)
import Halogen.Hooks.Types (HookK (..), StateId)
import Protolude hiding (fmap, get, gets, modify, pure, put, return, state, void, (<$>), (<*>), (>>), (>>=))

-- | Turn a hook program into a component.
--
-- The program runs on the component's first render, on every input it
-- receives, and again whenever a handler changes state. What it returns is
-- what the component renders.
--
-- Nothing in the resulting 'Component' mentions the row of child slots, so a
-- component that renders no children has to say which row it means some other
-- way: either a signature on the hook function, or @Hooks.component \@Empty@
-- — the slot row is deliberately the first type argument for that reason.
component
  :: forall slots q input output m hooks
   . (MonadIO m)
  => HookFn q input slots output m hooks
  -> Component q input output m
component hookFn =
  mkComponent
    ComponentSpec
      { initialState = initialHookState hookFn
      , render = \st -> st.result
      , eval = evalHook
      }
