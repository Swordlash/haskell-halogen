{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Lesson (component) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Hooks qualified as Hooks

-- The hooks useCounter uses, in order, in front of the caller's.
type UseCounter hooks = Hooks.UseState Int : hooks

data Counter scope slots output m = Counter
  { count :: Int
  , increment :: Hooks.HookM scope slots output m ()
  , decrement :: Hooks.HookM scope slots output m ()
  }

useCounter :: Int -> Hooks.Hook scope q slots output m (UseCounter hooks) hooks (Counter scope slots output m)
useCounter initial = Hooks.do
  (count, countId) <- Hooks.useState initial
  Hooks.pure
    Counter
      { count
      , increment = Hooks.modify_ countId (+ 1)
      , decrement = Hooks.modify_ countId (subtract 1)
      }

component :: forall q o m. (MonadIO m) => H.Component q () o m
component = Hooks.component @Empty $ \_ -> Hooks.do
  counter <- useCounter 0
  (seconds, secondsId) <- Hooks.useState (0 :: Int)
  Hooks.useLifecycleEffect $ do
    ticker <- Hooks.fork $ forever $ do
      liftIO (threadDelay 1000000)
      Hooks.modify_ secondsId (+ 1)
    pure (Just (Hooks.kill ticker))
  Hooks.pure $
    HH.div_
      [ HH.button [HE.onClick (\_ -> counter.decrement)] [HH.text "-"]
      , HH.button [HE.onClick (\_ -> counter.increment)] [HH.text "+"]
      , HH.p_ [HH.text ("Count: " <> T.pack (show counter.count))]
      , HH.p_ [HH.text ("On the page for " <> T.pack (show seconds) <> " s")]
      ]
