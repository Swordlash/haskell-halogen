-- | Every page of the site as the server sends it, and as its program
-- adopts it.
module Tutorial.Pages (spec) where

import Control.Monad (forM_, unless)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Tutorial.Site

spec :: Site -> IORef [Text] -> SpecWith Playwright
spec site allowed = describe "every page" $ do
  -- Every lesson's page holds its text before any script runs: its heading,
  -- the base path the pages were built for, and the element the compiled
  -- program mounts into; the introduction links every lesson and the
  -- playground, whose page is prerendered too. Read with JavaScript off, so
  -- that what is read is what the server sent.
  it "is prerendered: its heading, the base path and the result's element are in the page the server sends" $ \pw ->
    withContext pw defaultContextOptions {javaScriptEnabled = False} $ \ctx -> do
      page <- withTimeout 30000 <$> newPage ctx
      withFailureContext page $ do
        let served path = do
              status <- gotoStatus page (site.url <> path)
              unless (status == Just 200) $ expectationFailure (Text.unpack (path <> ": status " <> tshow status))
              content page
            has what html part = unless (part `Text.isInfixOf` html) $ expectationFailure (Text.unpack (what <> ": no " <> part <> " in the page"))
        forM_ site.lessons $ \lesson -> do
          html <- served (lesson.slug <> "/")
          has lesson.slug html ("<h1>" <> tshow lesson.number <> ". " <> escapeHtml lesson.title <> "</h1>")
          has lesson.slug html ("<base href=\"" <> site.base <> "\">")
          has lesson.slug html "id=\"learn-result\""
        home <- served ""
        forM_ site.lessons $ \lesson -> has "the introduction" home ("href=\"" <> site.base <> lesson.slug <> "/\"")
        has "the introduction" home ("href=\"" <> site.base <> "playground/\"")
        playground <- served "playground/"
        has "playground" playground "<h1>Playground</h1>"
        has "playground" playground ("<base href=\"" <> site.base <> "\">")
        has "playground" playground "id=\"learn-result\""
        report ("every page is prerendered (" <> tshow (length site.lessons) <> " lessons, the playground and the introduction)")

  -- Each page, held before app.wasm, marked, and adopted by the program
  -- with nothing repaired: no node of the server's replaced or removed,
  -- none built that it did not send, no text or attribute changed. On a
  -- narrow screen, where the compiler does not load by itself, so that a
  -- page's start is the app's alone. What the app's Initialize fills in or
  -- changes on purpose, once it has adopted the page, is left out: the
  -- editor's column (CodeMirror takes the element the app renders empty for
  -- it, and the code shown until it is there goes), the result's column
  -- (what the compiler's panel says, now that the app knows the screen is
  -- narrow), and the status line in the header (the same). Only what is in
  -- those three is left out: each is still the element the server sent, with
  -- its attributes. The lesson's text, the header's links and the pagers are
  -- compared node by node.
  it "is adopted by its program with nothing repaired" $ \pw ->
    withContext pw defaultContextOptions {viewport = Just (400, 800)} $ \ctx -> do
      page <- withTimeout 30000 <$> newPage ctx
      watch <- newWatch site ctx (const False) allowed
      withFailureContext page $ do
        let options = defaultAdoptionOptions {leaveOutContents = [".learn-code", ".learn-result-column", ".learn-status"]}
            paths = "" : "playground/" : map (\l -> l.slug <> "/") site.lessons
        -- The manifest is held as well as the program, until the page has
        -- been compared: what the app reads of it (the compiler's size, which
        -- the introduction states, and the playground's layers) arrives in a
        -- fork of Initialize, which may render before or after the
        -- comparison, and is no repair of the page either way. One gate of
        -- each, armed again for every page and never taken away: taking a
        -- route away while the page's requests are under way can leave them
        -- stalled. (The library's adoptWith does this but for the
        -- manifest's gate.)
        program <- hold page options.program
        sizes <- hold page "**/rootfs/rootfs.json"
        forM_ paths $ \path -> do
          rearm program
          rearm sizes
          goto page (site.url <> path)
          waitHeld program
          markServed options page
          release program
          waitStarted page
          repairs <- repairsSinceMarked page
          release sizes
          entered watch page
          unless (null repairs) $
            expectationFailure (Text.unpack (Text.unlines (("adopting " <> site.base <> path <> " repaired:") : map (("  " <>) . describeRepair) repairs)))
        -- Nor did any page download the compiler meanwhile.
        asked <- requests ctx
        let archives = [r.url | r <- asked, archive r]
        unless (null archives) $ expectationFailure ("a narrow screen downloaded archives: " <> show archives)
        checkWatch watch page
        report ("every page (" <> tshow (length paths) <> ") is adopted by its program with nothing repaired in the lesson's text, the header and the pagers")

escapeHtml :: Text -> Text
escapeHtml = Text.replace ">" "&gt;" . Text.replace "<" "&lt;" . Text.replace "&" "&amp;"
