-- | Which page and which document asked for each request (test/web/
-- documents.html): its own requests, its frame's, its worker's, its
-- navigation's own, one it made before the page went elsewhere, and a
-- beacon it sends as it goes, which the browser reports late or not at
-- all; two pages of one context told apart;
-- and 'requestsOf', a page's current documents' requests, without going to
-- @about:blank@ first.
module Test.Documents (spec) where

import Control.Monad (forM_)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec

-- | The requests whose address ends so.
endingIn :: Text -> [Request] -> [Request]
endingIn suffix = filter (Text.isSuffixOf suffix . (.url))

spec :: Text -> SpecWith Playwright
spec base = describe "the document that asked" $ do
  let at path = base <> path

  it "says which document of which page asked, and gives a page's current documents' requests" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    page <- newPage ctx
    goto page (at "documents.html")
    first' <- currentDocument page
    first'.url `shouldBe` at "documents.html"
    shown <- documents page
    map (.url) shown `shouldBe` [at "documents.html", at "framed.html?documents"]
    let frameDocument = (.id) (last shown)
    asked <- requestsOf page
    -- The navigation's own request is the document it loads, and the
    -- frame's document is one of its own.
    [nav] <- pure (endingIn "/documents.html" asked)
    nav.resourceType `shouldBe` "document"
    (nav.page, nav.document, nav.documentUrl) `shouldBe` (Just (pageNumber page), Just first'.id, Just (at "documents.html"))
    [framed] <- pure (endingIn "/framed.html?documents" asked)
    (framed.document, framed.documentUrl) `shouldBe` (Just frameDocument, Just (at "framed.html?documents"))
    frameDocument `shouldNotBe` first'.id

    -- A dedicated worker's script and its fetch are its document's.
    click (locator page "#worker")
    locator page "#said" `shouldHaveText` "fetched"
    worker <- endingIn "/worker.js" <$> requestsOf page
    fetched <- endingIn "/plain.html" <$> requestsOf page
    map (.document) (worker <> fetched) `shouldBe` [Just first'.id, Just first'.id]

    -- A request held while the page goes elsewhere stays the first
    -- document's, and is none of the second's.
    gate <- hold page "**/plain.html?late"
    click (locator page "#late")
    waitHeld gate
    goto page (at "second.html")
    release gate
    second <- currentDocument page
    second.id `shouldNotBe` first'.id
    late <- endingIn "/plain.html?late" <$> requests ctx
    map (\r -> (r.document, r.documentUrl)) late `shouldBe` [(Just first'.id, Just (at "documents.html"))]
    now <- requestsOf page
    map (.url) now `shouldBe` [at "second.html"]
    for_ now $ \r -> r.document `shouldBe` Just second.id

  -- A beacon the document sends as it goes. Whether the browser reports it
  -- at all depends on how it changes documents: when the navigation gives
  -- the frame a new host (Chromium's RenderDocument, on for some machines
  -- and not for others), the old document's last requests are sent and
  -- never reported, to Playwright or to the protocol, and there is nothing
  -- to attribute; otherwise the report comes once the next document's
  -- navigation is under way. So: the page's current document asked only
  -- for itself either way, and a beacon that is reported is the document's
  -- that sent it, once the driver has matched it with the browser's report
  -- (waited for in the log; its exchange may never end).
  it "never gives the next document what the one before asked for as it went" $ \pw ->
    withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      forM_ [1 :: Int .. 10] $ \i -> do
        let run = "run=" <> Text.pack (show i)
        goto page (at "documents.html?" <> run)
        sender <- currentDocument page
        goto page (at "second.html?" <> run)
        next <- currentDocument page
        eventually page $ do
          beacons <- endingIn ("plain.html?beacon&" <> run) <$> requests ctx
          map (\r -> (r.document, r.documentUrl)) beacons `shouldBe` map (const (Just sender.id, Just sender.url)) beacons
        nows <- requestsOf page
        map (\r -> (r.url, r.document)) nows `shouldBe` [(at "second.html?" <> run, Just next.id)]

  it "tells two pages of one context apart" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    one <- newPage ctx
    two <- newPage ctx
    goto one (at "documents.html")
    goto two (at "documents.html")
    ones <- requestsOf one
    twos <- requestsOf two
    map (.url) ones `shouldBe` map (.url) twos
    all (requestedBy one) ones `shouldBe` True
    all (requestedBy two) twos `shouldBe` True
    any (requestedBy two) ones `shouldBe` False
    oneDocument <- currentDocument one
    twoDocument <- currentDocument two
    oneDocument.id `shouldNotBe` twoDocument.id
    everything <- requests ctx
    length (filter (requestedBy one) everything) `shouldBe` length ones

  it "says the same of an exchange" $ \pw -> withContext pw defaultContextOptions {recordExchanges = Just 0} $ \ctx -> do
    page <- newPage ctx
    goto page (at "documents.html")
    document <- currentDocument page
    e <- waitForExchange ctx (\x -> "/framed.html?documents" `Text.isSuffixOf` x.url)
    framed <- (.id) . last <$> documents page
    (e.page, e.document, e.documentUrl) `shouldBe` (Just (pageNumber page), Just framed, Just (at "framed.html?documents"))
    nav <- waitForExchange ctx (\x -> "/documents.html" `Text.isSuffixOf` x.url)
    (nav.page, nav.document) `shouldBe` (Just (pageNumber page), Just document.id)
