-- | The check that a prerendered page is adopted with nothing repaired,
-- against adoption.html, whose script plays a program that adopts the page
-- in the way its @?mode=@ says: cleanly, or repairing one thing, which
-- must be reported as what it is.
module Test.Adoption (spec) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Test.HUnit.Lang (HUnitFailure (..))
import Test.Hspec

spec :: Text -> SpecWith Playwright
spec base = describe "the adoption of a prerendered page" $ do
  let at mode = base <> "adoption.html?mode=" <> mode
      options = defaultAdoptionOptions {program = "**/adoption-program.js"}

  -- The page's own parts that no render makes (the space between its tags,
  -- the comment that keeps two texts apart) are taken away, its texts set
  -- to what they were, a listener added: none of it is a repair.
  it "finds nothing repaired in a page adopted as it was" $ \pw -> withPage pw $ \page -> do
    adoptWith options page (at "clean") `shouldReturn` []
    -- The program is running: the page is its.
    click (locator page "#app button")
    textContent (locator page "#app .count") `shouldReturn` "1"

  it "reports a node replaced by an equal one, as replaced" $ \pw -> withPage pw $ \page ->
    adoptWith options page (at "replace")
      `shouldReturn` [ Replaced
                         { path = "#app > div.counter > p.count:nth-child(3)"
                         , was = "<p class=\"count\">0</p>"
                         , now = "<p class=\"count\">0</p>"
                         }
                     ]

  it "reports a text changed, with what it was and is" $ \pw -> withPage pw $ \page ->
    adoptWith options page (at "text")
      `shouldReturn` [TextChanged {path = "#app > div.counter > p.count:nth-child(3) > text()", was = "0", now = "1"}]

  it "reports a node built that the page did not have" $ \pw -> withPage pw $ \page ->
    adoptWith options page (at "add")
      `shouldReturn` [Built {path = "#app > div.counter > p.note:nth-child(6)", now = "<p class=\"note\">added</p>"}]

  it "reports a node removed, the outermost of what went" $ \pw -> withPage pw $ \page ->
    adoptWith options page (at "remove") `shouldReturn` [Removed {path = "#app > div.counter > h1:nth-child(1)", was = "<h1>Count</h1>"}]

  it "reports an attribute set" $ \pw -> withPage pw $ \page ->
    adoptWith options page (at "attribute")
      `shouldReturn` [AttributeChanged {path = "#app > div.counter > p.count:nth-child(3)", attribute = "data-count", before = Nothing, after = Just "0"}]

  it "leaves out what it is told to, and what is outside the root" $ \pw -> withPage pw $ \page -> do
    repairs <- adoptWith options page (at "island")
    map describeRepair repairs
      `shouldBe` [ "attribute data-stuck changed #app > div.counter: absent to \"\""
                 , "replaced #app > div.counter > div.island:nth-child(5) > span: <span>typeset by a library</span> by <b>typeset again</b>"
                 ]
    -- An island's content and an attribute the page's script writes left out.
    let quiet = options {leaveOut = [".island"], ignoredAttributes = ["data-stuck"]}
    withPage pw $ \other -> adoptWith quiet other (at "island") `shouldReturn` []
    withPage pw $ \other -> adoptWith options other (at "outside") `shouldReturn` []

  it "fails an expectation with every repair listed" $ \pw -> withPage pw $ \page -> do
    shouldAdoptCleanlyWith options page (at "clean")
    try (shouldAdoptCleanlyWith options page (at "text")) >>= \case
      Left (HUnitFailure _ reason) -> do
        let said = Text.pack (show reason)
        forM_ ["adopting the page at", "repaired 1 thing(s)", "text changed #app > div.counter > p.count:nth-child(3) > text()"] $ \part ->
          said `shouldSatisfy` Text.isInfixOf part
      Right () -> expectationFailure "a text changed passed as adopted cleanly"

  -- The two halves, with a gate of the test's own and something done
  -- between them, as a test that also holds the API does.
  it "marks and compares in two halves" $ \pw -> withPage pw $ \page -> do
    gate <- hold page "**/adoption-program.js"
    goto page (at "add")
    waitHeld gate
    -- What the server sent, before the program runs.
    count (locator page "#app .note") `shouldReturn` 0
    markServed options page
    release gate
    waitStarted page
    repairs <- repairsSinceMarked page
    map describeRepair repairs `shouldBe` ["built #app > div.counter > p.note:nth-child(6): <p class=\"note\">added</p>"]
    try (shouldHaveNoRepairs repairs) >>= \case
      Left (HUnitFailure _ reason) -> show reason `shouldSatisfy` Text.isInfixOf "built #app" . Text.pack
      Right () -> expectationFailure "a node built passed as no repair"
    closeGate gate
