-- | A component whose state changes are debounced.
--
-- What is delayed here is the state write, and so the render that follows it.
-- It wraps a component that is already written — hooks or not — and slows the
-- whole of it down, which is why it takes over the component's @eval@.
--
-- To delay one particular thing a component does rather than everything it
-- does, there is @Halogen.Hooks.Extra.Hooks.UseDebouncer@ in
-- @haskell-halogen-hooks@: it debounces a single action, and cancels through
-- the driver's own register of forks rather than an 'Async' of its own.
module Halogen.Component.Debounced where

import Control.Monad.State.Delayed.Delayer
import Control.Monad.State.Delayed.Trans
import Data.NT
import Data.Time
import HPrelude
import Halogen.Component
import Halogen.Query.HalogenM
import System.IO.Unsafe (unsafePerformIO)

unsafeMkDebouncedComponent
  :: forall state query action slots input output m
   . NominalDiffTime
  -> ComponentSpec' state query action slots input output m (DelayedStateT state (HalogenM state action slots output m))
  -> Component query input output m
unsafeMkDebouncedComponent timeout cs = do
  mkDebouncedComponent' (unsafePerformIO $ mkEmptyDelayer timeout) cs

mkDebouncedComponent'
  :: forall state query action slots input output m
   . Delayer state
  -> ComponentSpec' state query action slots input output m (DelayedStateT state (HalogenM state action slots output m))
  -> Component query input output m
mkDebouncedComponent' delayer ComponentSpec {..} =
  mkComponent
    $ ComponentSpec
      { initialState = initialState
      , render = render
      , eval = NT $ runDelayedStateT delayer . runNT eval
      }
