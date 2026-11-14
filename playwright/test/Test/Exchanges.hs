-- | What crossed the network: routes and gates that take only some kinds
-- of request. A server smoke test's need: an API held while the page's
-- images load.
module Test.Exchanges (spec) where

import Data.Aeson (Value)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Test.Server (standInImage, withServer)

-- | What a request from the page got: its status and body, or why it
-- failed.
fetchFrom :: Page -> Text -> Text -> Text -> IO Text
fetchFrom page method address body =
  evaluateWith
    page
    "async ([method, address, body]) => {\
    \  try {\
    \    const r = await fetch(address, method === 'GET' ? {cache: 'no-store'} : {method, body, cache: 'no-store'});\
    \    return r.status + ' ' + await r.text();\
    \  } catch (e) { return 'failed'; }\
    \}"
    (method, address, body)

spec :: SpecWith Playwright
spec = describe "what crossed the network" $ do
  describe "a filter" $ do
    -- The wiki's smoke test: the API held while the page loads, its images
    -- let through, or the page's load never comes.
    it "holds an API's fetch requests and lets its images through" $ \pw -> withServer $ \base -> withPage pw $ \page -> do
      gate <- holdWith page (routeFilter "**/api/**") {resourceTypes = ["fetch"]}
      goto page (base <> "api.html")
      waitHeld gate
      heldCount gate `shouldReturn` 1
      textContent (locator page "#data") `shouldReturn` "waiting"
      -- The image under the same glob loaded.
      evaluate page "document.getElementById('pic').naturalWidth" `shouldReturn` (32 :: Int)
      release gate
      locator page "#data" `shouldHaveText` "from the server"

    it "routes the methods it names, and never asks about the rest" $ \pw -> withServer $ \base -> withPage pw $ \page -> do
      goto page (base <> "basics.html")
      asked <- newIORef (0 :: Int)
      _ <- routeWith page (routeFilter "**/api/echo") {methods = ["post"]} $ \_ -> do
        atomicModifyIORef' asked (\n -> (n + 1, ()))
        pure (fulfill 503 (Just "text/plain") "routed")
      fetchFrom page "POST" "api/echo" "x" `shouldReturn` "503 routed"
      fetchFrom page "GET" "api/echo" "" >>= (`shouldSatisfy` Text.isPrefixOf "200 ")
      readIORef asked `shouldReturn` 1
      -- By resource type too: the page's image is not a fetch.
      images <- newIORef (0 :: Int)
      _ <- routeWith page (routeFilter "**/api/pic.png*") {resourceTypes = ["image"]} $ \_ -> do
        atomicModifyIORef' images (\n -> (n + 1, ()))
        pure (fulfill 200 (Just "image/png") standInImage)
      fetchFrom page "GET" "api/pic.png" "" >>= (`shouldSatisfy` Text.isPrefixOf "200 ")
      readIORef images `shouldReturn` 0
      _ :: Value <- evaluate page "new Promise((done) => { const i = new Image(); i.onload = i.onerror = () => done(null); i.src = 'api/pic.png?again'; })"
      readIORef images `shouldReturn` 1
