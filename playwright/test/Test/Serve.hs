-- | "Halogen.Playwright.Serve", asked by the browser itself, as a page's
-- own requests would ask it: what a static host would answer.
module Test.Serve (spec) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Serve
import System.Directory (doesFileExist)
import Test.Hspec

-- | What a @fetch@ in the page got.
data Answer = Answer
  { status :: Int
  , finalURL :: Text
  , contentType :: Maybe Text
  , contentLength :: Maybe Text
  , lastModified :: Maybe Text
  , body :: Text
  }
  deriving stock (Show)

instance FromJSON Answer where
  parseJSON = withObject "Answer" $ \o ->
    Answer
      <$> o .: "status"
      <*> o .: "url"
      <*> o .: "type"
      <*> o .: "length"
      <*> o .: "modified"
      <*> o .: "body"

-- | A request from the page, redirects followed.
ask :: Page -> Text -> Text -> IO Answer
ask page method address =
  evaluateWith
    page
    "async ([method, address]) => {\
    \  const r = await fetch(address, {method, cache: 'no-store'});\
    \  return {status: r.status, url: r.url, type: r.headers.get('content-type'),\
    \    length: r.headers.get('content-length'), modified: r.headers.get('last-modified'), body: await r.text()};\
    \}"
    (method, address)

spec :: SpecWith Playwright
spec = describe "serving a directory" $ do
  it "serves files with their media types, and a directory's index.html" $ \pw ->
    serveDirectory "test/web" $ \base -> withPage pw $ \page -> do
      goto page (base <> "plain.html")
      plain <- ask page "GET" (base <> "plain.html")
      plain.status `shouldBe` 200
      plain.contentType `shouldBe` Just "text/html; charset=utf-8"
      script <- ask page "GET" (base <> "worker.js")
      script.contentType `shouldBe` Just "text/javascript; charset=utf-8"
      index <- ask page "GET" (base <> "served/")
      index.status `shouldBe` 200
      Text.unpack index.body `shouldContain` "A directory's index"
      -- A directory's address without its slash goes to the one with it, so
      -- that the page's relative addresses resolve inside the directory.
      redirected <- ask page "GET" (base <> "served")
      redirected.finalURL `shouldBe` base <> "served/"
      redirected.status `shouldBe` 200
      missing <- ask page "GET" (base <> "nothing.html")
      missing.status `shouldBe` 404
      -- Every type a built program is made of, application/wasm first:
      -- instantiateStreaming refuses a binary served as anything else.
      map mediaType ["app.wasm", "glue.mjs", "style.css", "rootfs.json", "font.woff2", "archive.tar.zst.01"]
        `shouldBe` ["application/wasm", "text/javascript; charset=utf-8", "text/css; charset=utf-8", "application/json", "font/woff2", "application/octet-stream"]

  it "serves nothing from outside the directory, however the address spells it" $ \pw ->
    serveDirectory "test/web/served" $ \base -> withPage pw $ \page -> do
      goto page base
      -- A browser resolves ".." itself before asking, but not an encoded
      -- slash: the server gets one segment, "../plain.html" or
      -- "/etc/hostname", which joined to the directory would be a file
      -- beside it, or anywhere.
      beside <- ask page "GET" (base <> "..%2Fplain.html")
      beside.status `shouldBe` 404
      absolute <- ask page "GET" (base <> "%2Fetc%2Fhostname")
      absolute.status `shouldBe` 404
      backslashed <- ask page "GET" (base <> "..%5Cplain.html")
      backslashed.status `shouldBe` 404
      -- The file is there to be found, were the segment let through.
      doesFileExist "test/web/plain.html" `shouldReturn` True

  it "answers HEAD with what GET would say, and only GET and HEAD" $ \pw ->
    serveDirectory "test/web" $ \base -> withPage pw $ \page -> do
      goto page (base <> "plain.html")
      got <- ask page "GET" (base <> "plain.html")
      headed <- ask page "HEAD" (base <> "plain.html")
      headed.status `shouldBe` 200
      headed.body `shouldBe` ""
      headed.contentLength `shouldBe` Just (Text.pack (show (Text.length got.body)))
      headed.lastModified `shouldSatisfy` (/= Nothing)
      posted <- ask page "POST" (base <> "plain.html")
      posted.status `shouldBe` 405

  it "serves under a base path, with a 404 page and another directory mounted" $ \pw -> do
    let options = defaultServeOptions {basePath = "site", notFound = Just "404.html", mounts = [("more", "test/web/served")]}
    serveDirectoryWith options "test/web" $ \base -> withPage pw $ \page -> do
      Text.isSuffixOf "/site/" base `shouldBe` True
      let origin = Text.dropEnd (Text.length "site/") base
      goto page (base <> "plain.html")
      textContent (locator page "#go") `shouldReturn` "go"
      -- The base without its slash goes to the base.
      bare <- ask page "GET" (Text.dropEnd 1 base)
      bare.finalURL `shouldBe` base
      -- Outside the base is another site's: a bare 404, not the 404 page.
      outside <- ask page "GET" (origin <> "plain.html")
      outside.status `shouldBe` 404
      Text.unpack outside.body `shouldNotContain` "No such page"
      -- Under it, an address with no file gets the 404 page, with a 404.
      missing <- ask page "GET" (base <> "no/such/page/")
      missing.status `shouldBe` 404
      Text.unpack missing.body `shouldContain` "No such page"
      mounted <- ask page "GET" (base <> "more/")
      mounted.status `shouldBe` 200
      Text.unpack mounted.body `shouldContain` "A directory's index"
