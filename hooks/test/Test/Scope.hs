{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- These coercions must be rejected by the compiler. Deferring the errors lets
-- the ordinary test runner check that rejection on every supported backend.
module Test.Scope (spec) where

import Control.Exception qualified as Exception
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Hooks qualified as Hooks
import Protolude
import Test.Harness (dispose, start)
import Test.Hspec (Spec, describe, it, shouldThrow)

escape :: Hooks.StateId scope Int -> Hooks.StateId () Int
escape = coerce

escapeAction :: Hooks.HookM scope Empty Void IO () -> Hooks.HookM () Empty Void IO ()
escapeAction = coerce

isScopeError :: Exception.TypeError -> Bool
isScopeError (Exception.TypeError message) = "Couldn't match type" `List.isInfixOf` message

component :: H.Component H.VoidF () Void IO
component = Hooks.component @Empty $ \_ -> Hooks.do
  (_, stateId) <- Hooks.useState (0 :: Int)
  Hooks.useLifecycleEffect $ do
    liftIO $ evaluate (escape stateId) `shouldThrow` isScopeError
    liftIO $ evaluate (escapeAction (Hooks.put stateId 1)) `shouldThrow` isScopeError
    pure Nothing
  Hooks.pure $ HH.text "scoped"

spec :: Spec
spec =
  describe "hook scopes"
    $ it "rejects coercing state handles and actions to another scope"
    $ start component () >>= dispose
