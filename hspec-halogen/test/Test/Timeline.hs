-- | What a failing test says of the tree it tested: the events the tree
-- went through, oldest first, after the expectation's own message.
module Test.Timeline (spec) where

import Data.List (findIndex)
import Data.Text qualified as T
import Example.Counter qualified as Counter
import Halogen qualified as H
import Protolude hiding (find)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m =
  it "shows what the tree did when an expectation fails" $ do
    -- A page of its own, which fails on purpose: what it throws is what
    -- hspec would have printed.
    outcome <- try $ runPage $ do
      ui <- mount m (H.named "Counter" Counter.component) ()
      find ui "button.increment" >>= click
      find ui ".count" >>= (`shouldHaveText` "1")
      expectationFailure "failing on purpose"
    case outcome of
      Left (HUnitFailure _ (Reason message)) -> do
        let shown = T.lines (toS message)
            about what = filter (\l -> "Counter#" `T.isInfixOf` l && what `T.isInfixOf` l) shown
        take 1 shown `Hspec.shouldBe` ["failing on purpose"]
        shown `Hspec.shouldContain` ["What the tree did (the last 50 events, oldest first):"]
        for_ ["mounted", "initialized", "action", "state changed", "rendered in", "raised an output"] $ \what ->
          about what `Hspec.shouldSatisfy` (not . null)
        -- One line an event, oldest first: the click's action comes before
        -- the change it made.
        let indexOf what = findIndex (\l -> "Counter#" `T.isInfixOf` l && what `T.isInfixOf` l) shown
        (indexOf "  action" < indexOf "state changed") `Hspec.shouldBe` True
      Left other -> Hspec.expectationFailure ("another failure: " <> show other)
      Right () -> Hspec.expectationFailure "the test passed"
