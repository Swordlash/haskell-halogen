-- | The walk after every lesson has compiled: the check as the reader
-- types, back and forward, lessons 6, 5 and 15, the editor's keys, and a
-- second visit, from the cache.
module Tutorial.Later (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless)
import Data.IORef (writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Tutorial.Site
import Tutorial.Walk

compiledAndRan :: Text -> IO ()
compiledAndRan said = unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack said)

spec :: Walk -> SpecWith Page
spec walk = do
  let site = walk.site
      binding page text = withText text (locator page ".learn-bindings button.learn-binding")
      warning page text = withText text (locator page ".learn-diagnostics li.learn-warning")
      -- The end of label's line, by the list: the editor scrolls itself,
      -- so the line may be out of its view for a click.
      atLabel page = do
        click (binding page "label :: State -> Text")
        press page "End"

  -- The live check, on lesson 20: what GHC says of the code as it is, with
  -- no Compile, is in the editor and under it, with the types of the
  -- module's top-level bindings, in a list and over their names. A check
  -- never touches the result. The check's delay after a change and the
  -- check itself are the program's (a liftIO each), so quiet is when the
  -- check has been reported.
  step "checks lesson 20 as the reader types, with no Compile" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    watch <- watchOf walk page
    case lessonBySlug site "20-holes" of
      Nothing -> pendingWith "no lesson 20-holes"
      Just holes -> do
        openLesson site watch page holes
        click (locator page "button.learn-reset")
        runBefore <- getAttribute (locator page ".learn-outcome") "data-run"
        ((), took) <- timed (waitQuiet (long site page))
        -- Start.hs's own check: its holes, which Solution.hs, what the
        -- lessons' loop left on the page that compiled lesson 20, does not
        -- have.
        readOnce (warning page "Found hole: _ :: State -> State") "the hole's warning"
        checkLine <- textContent (locator page ".learn-check")
        unless ("Checked in" `Text.isPrefixOf` checkLine) $ expectationFailure (Text.unpack ("the check's line says " <> checkLine))
        readOnce (binding page "render :: State -> HH.HTML w Action") "render's type"
        readOnce (binding page "label :: State -> Text") "label's type"
        count (binding page "rename ::") `shouldReturn` 0
        readOnce (warning page "_rename :: Text -> State -> State") "_rename's hole"
        warnings <- allTextContents (locator page ".learn-diagnostics li.learn-warning")
        unless (any (\w -> "standing for" `Text.isInfixOf` w && "Text" `Text.isInfixOf` w) warnings) $
          expectationFailure ("no hole standing for Text among " <> show warnings)
        getAttribute (locator page ".learn-outcome") "data-run" `shouldReturn` runBefore
        report ("lesson 20 is checked with no Compile, " <> seconds took <> " after Reset: " <> checkLine)

        -- Hovering a binding's name shows its type (CodeMirror's tooltip,
        -- which opens after a delay of its own: a condition); a click in
        -- the list goes to its line (render's first, for the editor to
        -- scroll it into view).
        click (binding page "render :: State -> HH.HTML w Action")
        box <- boundingBox (first (withText "render st =" (locator page ".learn-editor .cm-line")))
        forM_ box $ \b -> mouseMove page (b.x + 12) (b.y + 8)
        waitFor (withText "render :: State -> HH.HTML w Action" (locator page ".cm-tooltip .cm-learn-type")) Attached
        click (binding page "label :: State -> Text")
        active <- textContent (locator page ".cm-activeLine")
        unless ("label st =" `Text.isPrefixOf` active) $ expectationFailure (Text.unpack ("a binding in the list goes to the line it is bound at, not " <> active))
        report "hovering render shows its type, and the list goes to label's line"

        -- A typed error is marked within the check's delay, the types
        -- listed stay, said to be from before, and undoing it checks clean
        -- again.
        atLabel page
        ((), marked) <- timed $ do
          typeKeys page " 1"
          waitQuiet (long site page)
        readOnce (locator page ".cm-lintRange-error") "the typed error's mark"
        contains (locator page ".learn-check") "does not compile"
        readOnce (withText "From the last time" (locator page ".learn-bindings p")) "the types said to be from before"
        press page "Backspace"
        press page "Backspace"
        waitQuiet (long site page)
        contains (locator page ".learn-check") "it compiles"
        count (locator page ".cm-lintRange-error") `shouldReturn` 0
        report ("a typed error is marked " <> seconds marked <> " after the key, and goes when it is undone")

        -- A check under way when Compile is pressed is not reported over
        -- the compile: an error typed, its check let start (the delay is
        -- 1.2 s, so 1.3 s is after it: the race is the point, and it is
        -- one in time), undone, and compiled at once, leaves the compile's
        -- clean result standing, whether the check was over by then or
        -- not. Both are the program's: quiet is when both have reported.
        click (locator page "button.learn-solution")
        waitQuiet (long site page)
        readOnce (binding page "rename :: Text -> State -> State") "rename's type, in Solution.hs"
        atLabel page
        typeKeys page " 1"
        threadDelay 1300000
        press page "Backspace"
        press page "Backspace"
        compile site page "lesson 20, Solution.hs, with a check before it" >>= compiledAndRan
        count (locator page ".learn-diagnostics li") `shouldReturn` 0
        outcome page >>= compiledAndRan
        count (locator page ".learn-bindings p") `shouldReturn` 0
        report "a check under way when Compile is pressed is not reported over the compile"

  -- Back to the introduction the lesson the page is on was opened from,
  -- and forward to that lesson again.
  step "goes back and forward" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    here <- url page
    heading <- textContent (locator page "article h1")
    goBack page
    waitForURL page site.url
    readOnce (locator page ".learn-home h1") "the introduction"
    goForward page
    waitForURL page here
    textContent (locator page "article h1") `shouldReturn` heading
    report "back and forward"

  -- Lesson 6's hash router, under the page's <base href>: its links and
  -- what it pushes are the lesson's own address with the route after the
  -- #, not the base's, and Back goes to its route before without leaving
  -- the lesson.
  step "keeps lesson 6's routes under the lesson's address, Back included" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    watch <- watchOf walk page
    case lessonBySlug site "06-router" of
      Nothing -> pendingWith "no lesson 06-router"
      Just routing -> do
        openLesson site watch page routing
        click (locator page "button.learn-solution")
        compile site page "lesson 6, Solution.hs" >>= compiledAndRan
        let about = withText "About" (locator page "#learn-result a")
        getAttribute about "href" `shouldReturn` Just (site.base <> "06-router/#/about")
        click about
        waitForURL page (site.url <> "06-router/#/about")
        readOnce (withText "You are at About" (locator page "#learn-result p")) "the About route"
        goBack page
        waitForURL page (site.url <> "06-router/")
        readOnce (withText "You are at Home" (locator page "#learn-result p")) "the Home route"
        count (withText routing.title (locator page "article h1")) `shouldReturn` 1
        report "lesson 6's links keep the lesson's address, and Back returns to the route before, in the lesson"

  -- What lesson 5's Finalize prints, as the next compile ends the last
  -- run's component, shows under the editor, every time.
  step "shows what lesson 5's Finalize prints on every compile after the first" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    watch <- watchOf walk page
    case lessonBySlug site "05-lifecycle" of
      Nothing -> pendingWith "no lesson 05-lifecycle"
      Just lifecycle -> do
        openLesson site watch page lifecycle
        click (locator page "button.learn-solution")
        compile site page "lesson 5, Solution.hs" >>= compiledAndRan
        forM_ [1 :: Int .. 4] $ \i -> do
          compile site page ("lesson 5 again, " <> tshow i) >>= compiledAndRan
          out <- Text.concat <$> allTextContents (locator page ".learn-stdout")
          let finalized = any (\l -> "Finalized after " `Text.isPrefixOf` l && " seconds." `Text.isSuffixOf` l) (Text.lines out)
          unless finalized $ expectationFailure ("compile " <> show i <> " does not show what the last run's Finalize printed: " <> show out)
        report "what lesson 5's Finalize prints shows on every compile after the first"

  -- Lesson 15's store: adding one renders the count's view again and not
  -- the name's, and a new name renders the name's view alone.
  step "renders lesson 15's connected views again only for their own part of the store" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    watch <- watchOf walk page
    case lessonBySlug site "15-store" of
      Nothing -> pendingWith "no lesson 15-store"
      Just store -> do
        openLesson site watch page store
        click (locator page "button.learn-solution")
        compile site page "lesson 15, Solution.hs" >>= compiledAndRan
        let view text = readOnce (withText text (locator page "#learn-result p")) (Text.unpack text)
            add = withText "Add one" (locator page "#learn-result button")
        view "Count: 0 (rendered again: 0)"
        click add
        click add
        view "Count: 2 (rendered again: 2)"
        view "Name: Ada (rendered again: 0)"
        fill (locator page "#learn-result input") "Grace"
        view "Name: Grace (rendered again: 1)"
        view "Count: 2 (rendered again: 2)"
        report "lesson 15's connected views render again only for their own part of the store"

  -- Tab indents in the editor, and Escape then Tab leaves it. The click is
  -- on the module's header line, so that the two spaces are counted in a
  -- line that has text (an empty line's text changes by one, its line
  -- break giving way to the spaces), wherever the layout puts the editor.
  step "indents with Tab, and leaves the editor with Escape then Tab" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    let headerLine = first (withText "module Lesson" (locator page ".learn-editor .cm-line"))
    click headerLine
    typed <- Text.length <$> textContent headerLine
    press page "Tab"
    indented :: Bool <-
      evaluateWith page "(n) => [...document.querySelectorAll('.learn-editor .cm-line')].some((l) => l.textContent.includes('module Lesson') && l.textContent.length === n + 2)" typed
    unless indented $ expectationFailure "Tab does not indent the line by two spaces"
    inEditor :: Bool <- evaluate page "!!document.activeElement.closest('.cm-editor')"
    unless inEditor $ expectationFailure "Tab leaves the editor"
    press page "Escape"
    press page "Tab"
    stillIn :: Bool <- evaluate page "!!document.activeElement.closest('.cm-editor')"
    unless (not stillIn) $ expectationFailure "Escape then Tab does not leave the editor"
    click (locator page "button.learn-reset")
    report "Tab indents by two spaces, and Escape then Tab moves the focus out of the editor"

  -- The reader's code survives a reload, and the compiler comes from the
  -- cache the second time: a second visit, in a page of its own of the same
  -- context, as after closing the tab.
  step "keeps the reader's code and the compiler for a second visit" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    watch <- watchOf walk page
    -- What the first page said, before it goes.
    checkWatch watch page
    closePage page
    again <- withTimeout 30000 <$> newPage (pageContext page)
    writeIORef walk.current (Just again)
    withFailureContext again $ do
      revisit <- getMonotonicTime
      fetched <- length <$> requests (pageContext again)
      visit (long site again) (site.url <> "02-counter/")
      entered watch again
      contains (locator again ".learn-editor .cm-content") "modify (subtract 1)"
      untilReady site again revisit ""
      asked <- drop fetched <$> requests (pageContext again)
      let fromNetwork = [r.url | r <- asked, r.method == "GET", archive r]
      unless (null fromNetwork) $ expectationFailure ("the archives were downloaded again, not read from the cache: " <> show fromNetwork)
      now <- getMonotonicTime
      report ("the reader's code is kept for the next visit; the compiler was ready again " <> seconds (now - revisit) <> " after it, from the cache")
      click (locator again "button.learn-reset")
      shownNow <- editorText again
      unless (not ("subtract" `Text.isInfixOf` shownNow)) $ expectationFailure "Reset did not bring back Start.hs"
      kept :: Maybe Text <- evaluate again "localStorage.getItem('learn:02-counter')"
      kept `shouldBe` Nothing
      report "Reset brings back Start.hs and forgets the saved code"
      -- The playground's code, its layer and its arguments are kept for
      -- the next visit too, and compile as they were.
      if any (\l -> l.name == "store") site.manifest.layers
        then do
          click (first (locator again "a.learn-to-playground"))
          waitForURL again (site.url <> "playground/")
          entered watch again
          contains (locator again ".learn-editor .cm-content") "import Halogen.Store.Monad ()"
          -- The checked layer loads (from the cache): the program's.
          waitQuiet (long site again)
          count (locator again ".learn-layers input[value=\"store\"]:checked") `shouldReturn` 1
          inputValue (locator again "#learn-extra-args") `shouldReturn` "-fdefer-typed-holes"
          compiled site again
          readOnce (withText "The store's package is here." (locator again "#learn-result p")) "the kept program's paragraph"
          report "the playground's code, its checked layer and its arguments are kept for the next visit, and compile"
        else pendingWith "this rootfs has no store layer"

  step "said nothing wrong, and loaded nothing from another site but what a layer loads" $ \page0 -> do
    requireLoaded walk
    page <- pageOf walk page0
    watch <- watchOf walk page
    checkWatch watch page
    report "no errors in the walk's pages"
