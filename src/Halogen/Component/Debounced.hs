-- | Component capable of debouncing actions
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
  :: NominalDiffTime
  -> ComponentSpec' state query action slots input output m (DelayedStateT state (HalogenM state action slots output m))
  -> Component query input output m
unsafeMkDebouncedComponent timeout cs = do
  mkDebouncedComponent' (unsafePerformIO $ mkEmptyDelayer timeout) cs

mkDebouncedComponent'
  :: Delayer state
  -> ComponentSpec' state query action slots input output m (DelayedStateT state (HalogenM state action slots output m))
  -> Component query input output m
mkDebouncedComponent' delayer ComponentSpec {..} =
  mkComponent
    $ ComponentSpec
      { initialState = initialState
      , render = render
      , eval = NT $ runDelayedStateT delayer . runNT eval
      }
