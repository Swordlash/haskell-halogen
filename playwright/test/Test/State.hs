-- | A context's state and what the page is made to see: cookies, the
-- network, media, permissions, a phone, dialogs, print.
module Test.State (spec) where

import Data.ByteString.Char8 qualified as Char8
import Data.List (find)
import Data.Text (Text)
import Halogen.Playwright
import Halogen.Playwright.Hspec

spec :: Text -> SpecWith Playwright
spec base = describe "a context's state" $ do
  let at path = base <> path

  -- smoke-test-server.mjs and test-passkeys.mjs: signed in once, the
  -- session cookie read and given to fresh contexts; then signed out.
  it "reads a context's cookies, gives them to another, and clears them" $ \pw -> do
    token <- withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (at "state.html")
      _ :: Maybe Text <- evaluate page "(document.cookie = 'wiki_session=the-token; path=/', null)"
      kept <- cookies ctx
      let session = find (\c -> c.name == "wiki_session") kept
      fmap (.domain) session `shouldBe` Just (Just "127.0.0.1")
      fmap (.expires) session `shouldBe` Just Nothing
      pure (maybe "" (.value) session)
    token `shouldBe` "the-token"
    withContext pw defaultContextOptions $ \fresh -> do
      addCookies fresh [cookie "wiki_session" token base]
      page <- newPage fresh
      goto page (at "state.html")
      locator page "#cookie" `shouldHaveText` "wiki_session=the-token"
      clearCookies fresh
      cookies fresh `shouldReturn` []
      reload page
      locator page "#cookie" `shouldHaveText` ""

  -- smoke-test.mjs and smoke-test-server.mjs: the reader's browser
  -- offline, then online again.
  it "takes a context off the network and puts it back, its pages hearing it" $ \pw ->
    withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (at "state.html")
      setOffline ctx True
      -- The page's handler has run: setOffline settled it.
      textContent (locator page "#network") `shouldReturn` "offline"
      evaluate page "navigator.onLine" `shouldReturn` False
      evaluate page "fetch('second.html').then(() => 'fetched', () => 'failed')" `shouldReturn` ("failed" :: Text)
      setOffline ctx False
      textContent (locator page "#network") `shouldReturn` "online"

  -- smoke-test.mjs: a dark page made light, and a deck printed.
  it "emulates the colour scheme and print, and the page hears it" $ \pw ->
    withContext pw defaultContextOptions {colorScheme = Just Dark} $ \ctx -> do
      page <- newPage ctx
      goto page (at "state.html")
      textContent (locator page "#scheme") `shouldReturn` "dark"
      emulateMedia page (Scheme Light)
      textContent (locator page "#scheme") `shouldReturn` "light"
      emulateMedia page Print
      textContent (locator page "#media") `shouldReturn` "print"
      isVisible (locator page "#screen-only") `shouldReturn` False
      emulateMedia page Screen
      textContent (locator page "#media") `shouldReturn` "screen"

  -- smoke-test.mjs: the deck printed with its CSS's page size, one slide
  -- to a page, counted in the PDF.
  it "prints a page to PDF, a sheet to a page" $ \pw -> withPage pw $ \page -> do
    goto page (at "state.html")
    printed <- pdf page
    Char8.take 5 printed `shouldBe` "%PDF-"
    pdfPages printed `shouldBe` 3

  -- smoke-test.mjs: Copy puts a page's source on the clipboard, read back
  -- with the permissions granted.
  it "grants a context's pages permissions" $ \pw ->
    withContext pw defaultContextOptions {permissions = ["clipboard-read", "clipboard-write"]} $ \ctx -> do
      page <- newPage ctx
      goto page (at "state.html")
      click (locator page "#copy")
      eventually page (evaluate page "navigator.clipboard.readText()" `shouldReturn` ("copied text" :: Text))

  -- smoke-test.mjs: a page, the editor and the administration on a phone,
  -- its buttons tapped, a dialog that fits the screen.
  it "is a phone, whose elements are tapped" $ \pw ->
    withContext pw defaultContextOptions {viewport = Just (390, 844), isMobile = True, hasTouch = True} $ \ctx -> do
      page <- newPage ctx
      goto page (at "state.html")
      viewportSize page `shouldReturn` (390, 844)
      evaluate page "innerWidth" `shouldReturn` (390 :: Int)
      textContent (locator page "#pointer") `shouldReturn` "coarse"
      tap (locator page "#touch")
      textContent (locator page "#touch") `shouldReturn` "touched"

  it "fails a tap where there is no touch" $ \pw -> withPage pw $ \page -> do
    goto page (at "state.html")
    tap (locator page "#touch") `shouldThrow` \(e :: PlaywrightError) -> e.method == "tap"

  -- the tutorial's test and smoke-test-server.mjs: leaving with unsaved work
  -- asks, and the walk says leave; a page that should open no dialog.
  it "answers dialogs as the context says, and keeps them" $ \pw -> do
    withPage pw $ \page -> do
      goto page (at "state.html")
      click (locator page "#confirm")
      textContent (locator page "#confirmed") `shouldReturn` "false"
      dialogs page `shouldReturn` [Dialog "confirm" "Leave?"]
    withContext pw defaultContextOptions {acceptDialogs = True} $ \ctx -> do
      page <- newPage ctx
      goto page (at "state.html")
      click (locator page "#confirm")
      textContent (locator page "#confirmed") `shouldReturn` "true"
      -- A page that asks before it is left, after the reader has acted on
      -- it (without that, the browser does not ask), is left.
      _ :: Maybe Text <- evaluate page "(addEventListener('beforeunload', (e) => { e.preventDefault(); e.returnValue = ''; }), null)"
      goto page (at "second.html")
      title page `shouldReturn` "Second"
      map (.kind) <$> dialogs page `shouldReturn` ["confirm", "beforeunload"]
  where
    -- The page objects in a PDF, as the smoke test counts them.
    pdfPages bytes = length (filter isPage (Char8.tails bytes))
    isPage rest =
      "/Type" `Char8.isPrefixOf` rest
        && let after = Char8.dropWhile (== ' ') (Char8.drop 5 rest)
            in "/Page" `Char8.isPrefixOf` after && not ("/Pages" `Char8.isPrefixOf` after)
