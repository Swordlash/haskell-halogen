-- | The Islands example: while the islands of a page wait in their
-- @Initialize@, the components around them go on. The islands wait five
-- seconds, and what they must not hold up is given one.
module Test.Islands (spec) where

import Example.Islands qualified as Islands
import Halogen qualified as H
import Halogen.Router qualified as Router
import Halogen.Router.Duplex qualified as RD
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  -- The shell subscribes to the router in its own Initialize, which runs
  -- once its page's has run up to its first wait (not, as in
  -- purescript-halogen, once the islands' have ended), so it hears the
  -- link. The tree is mounted meanwhile, too.
  it "follows a link on the first page it shows while the page's islands wait" $ runPage $ do
    (router, ui) <- mountAt m slowly (Just 1)
    islandsWait ui
    find ui ".plain-link" >>= click
    showsSoon ui ".plain" "the plain page"
    (length <$> findAll ui ".island") `shouldReturn` 0
    unsafeIOToPageM (Router.current router) `shouldReturn` Nothing

  it "hears the editor, has its questions answered, and leaves while the islands wait" $ runPage $ do
    (_, ui) <- mountAt m slowly Nothing
    find ui ".slow-link" >>= click
    islandsWait ui
    -- The editor's output reaches the page view, and the page view's the
    -- shell, at once.
    find ui ".shout" >>= click
    showsSoon ui ".page-heard" "shouted"
    showsSoon ui ".heard" "shouted"
    -- A query to the editor is answered, and the answer rendered.
    find ui ".ask" >>= click
    showsSoon ui ".answer" "dirty: False"
    -- The editor asks to leave, and the shell replaces the page.
    find ui ".leave" >>= click
    showsSoon ui ".plain" "the plain page"
    (length <$> findAll ui ".island") `shouldReturn` 0

  -- This one held before too: the page view's click is its own action, and
  -- the shell had subscribed long before it made the page.
  it "follows the page view's link on a later page while the islands wait" $ runPage $ do
    (_, ui) <- mountAt m slowly Nothing
    find ui ".slow-link" >>= click
    islandsWait ui
    find ui ".plain-link" >>= click
    showsSoon ui ".plain" "the plain page"
    (length <$> findAll ui ".island") `shouldReturn` 0

  it "shows the islands loaded once they have waited" $ runPage $ do
    (_, ui) <- mountAt m 100 (Just 1)
    _ <- find ui ".island"
    islands <- findAll ui ".island"
    length islands `shouldBe` 2
    for_ islands (`shouldHaveText` "loaded")
    -- Back where the other tests start.
    find ui ".plain-link" >>= click
    showsSoon ui ".plain" "the plain page"

-- | How long the islands wait, in milliseconds: well past every check.
slowly :: Int
slowly = 5000

-- | An element matching the selector shows this text within a second. Each
-- attempt looks once (not with 'find', which waits by itself), so that a
-- failure says what the page showed instead.
showsSoon :: Mounted s q o m -> Text -> Text -> PageM s ()
showsSoon ui selector expected = eventuallyWithin 1000 $ do
  shown <- traverse textContent =<< findAll ui selector
  unless (expected `elem` shown) $
    expectationFailure ("expected " <> show selector <> " to show " <> show expected <> ", found " <> show shown)

-- | The shell, with a hash router of its own at this route, its islands
-- waiting so long.
mountAt :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Int -> Islands.Route -> PageM s (Router.Router Islands.Route, Mounted s H.VoidF Void m)
mountAt m waitMs route = do
  router <- unsafeIOToPageM (runTest @m (Router.newHashRouter Nothing (RD.root (RD.optional (RD.int RD.segment)))))
  unsafeIOToPageM (Router.redirect router route)
  ui <- mount m (Islands.component :: H.Component H.VoidF Islands.Input Void m) Islands.Input {router, waitMs}
  pure (router, ui)

-- | Both islands are on the page, still waiting.
islandsWait :: Mounted s q o m -> PageM s ()
islandsWait ui = do
  _ <- find ui ".island"
  islands <- findAll ui ".island"
  length islands `shouldBe` 2
  for_ islands $ \i -> textContent i `shouldReturn` "waiting"
