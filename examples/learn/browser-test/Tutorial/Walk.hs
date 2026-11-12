-- | The walk: one reader's page, from lesson 1 as the server sends it to a
-- second visit, every example going on from where the one before left it.
-- This module has its start: the adoption, the compiler loading, lessons 1
-- and 2, the Source mode, a type error, the playground, a layer loaded while
-- the reader moves on, and an error GHC throws.
module Tutorial.Walk
  ( Walk (..)
  , newWalk
  , pageOf
  , requireLoaded
  , watchOf
  , startSpec
  )
where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Tutorial.Site

-- | What the walk's examples share beside the page.
data Walk = Walk
  { site :: Site
  , allowed :: IORef [Text]
  , watch :: IORef (Maybe Watch)
  , loaded :: IORef Bool
  -- ^ Whether the compiler loaded: nothing after it can run otherwise.
  , current :: IORef (Maybe Page)
  -- ^ The page the walk goes on in, once the first has been closed for a
  -- second visit.
  , navigated :: IORef Double
  -- ^ When the walk went to lesson 1, which the compiler's phases are
  -- timed from.
  }

newWalk :: Site -> IORef [Text] -> IO Walk
newWalk site allowed = do
  watch <- newIORef Nothing
  loaded <- newIORef False
  current <- newIORef Nothing
  navigated <- newIORef 0
  pure Walk {site, allowed, watch, loaded, current, navigated}

-- | The page the walk is on.
pageOf :: Walk -> Page -> IO Page
pageOf walk page = fromMaybe page <$> readIORef walk.current

watchOf :: Walk -> Page -> IO Watch
watchOf walk page =
  readIORef walk.watch >>= \case
    Just w -> pure w
    Nothing -> do
      w <- newWatch walk.site (pageContext page) (const False) walk.allowed
      writeIORef walk.watch (Just w)
      pure w

-- | Nothing after the compiler's load can run if it did not load, and that
-- example has failed already.
requireLoaded :: Walk -> IO ()
requireLoaded walk = do
  ok <- readIORef walk.loaded
  unless ok $ pendingWith "the compiler did not load (see the examples before)"

-- | The parts of a lesson's page that the app's Initialize fills in or
-- changes on purpose, once it has adopted the page (see "Tutorial.Pages").
adoption :: AdoptionOptions
adoption = defaultAdoptionOptions {leaveOut = [".learn-code", ".learn-result-column", ".learn-status"]}

-- | Where a line's text starts, by what it holds, and where a name is in
-- it: @line:column@, from 1, as the Source mode marks an element.
position :: [Text] -> Text -> Text -> Maybe (Int, Int)
position code holding name = do
  i <- findIndex (holding `Text.isInfixOf`) code
  let (before, found) = Text.breakOn name (code !! i)
  if Text.null found then Nothing else Just (i + 1, Text.length before + 1)

showPosition :: (Int, Int) -> Text
showPosition (line, column) = tshow line <> ":" <> tshow column

-- | A program for the playground with a paragraph built inside a chain of
-- @$@ with another operator in it, and one whose @HH.p@ is in parentheses
-- before a @$@: Source marks each paragraph where its @HH.p@ is, not only
-- the div around it.
chainProgram :: Text
chainProgram =
  Text.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "module Lesson (component) where"
    , ""
    , "import Control.Monad.IO.Class (MonadIO)"
    , "import Halogen qualified as H"
    , "import Halogen.HTML qualified as HH"
    , ""
    , "component :: forall q o m. (MonadIO m) => H.Component q () o m"
    , "component = H.component (\\_ -> pure ()) render []"
    , "  where"
    , "    render () = HH.div_ [chain, parenthesised]"
    , "    chain = HH.div_ $ pure $ HH.p [] $ [HH.text \"a\"] <> [HH.text \"b\"]"
    , "    parenthesised = (HH.p []) $ [HH.text \"c\"]"
    ]

-- | A program for the playground that needs the store layer's package: it
-- imports one of its modules, and shows that it compiled.
storeProgram :: Text
storeProgram =
  Text.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "module Lesson (component) where"
    , ""
    , "import Control.Monad.IO.Class (MonadIO)"
    , "import Halogen qualified as H"
    , "import Halogen.HTML qualified as HH"
    , "import Halogen.Store.Monad ()"
    , ""
    , "component :: forall q o m. (MonadIO m) => H.Component q () o m"
    , "component = H.component (\\_ -> pure ()) (\\_ -> HH.p_ [HH.text \"The store's package is here.\"]) []"
    ]

-- | The window's flag that the page has not been loaded again since the
-- test set it: following the tutorial's links never reloads.
noReload :: Page -> IO ()
noReload page = do
  flag :: Maybe Int <- evaluate page "window.learnNoReload ?? null"
  unless (flag == Just 1) $ expectationFailure "the page was loaded again: following the tutorial's links must not reload it"

startSpec :: Walk -> SpecWith Page
startSpec walk = do
  let site = walk.site
      -- The suite's first example has checked that there are two.
      (lesson1, lesson2) = case site.lessons of
        a : b : _ -> (a, b)
        _ -> error "the tutorial has fewer than two lessons"

  -- The lesson shows before app.wasm has arrived, and the program adopts
  -- the page as it is: nothing repaired in the lesson's text, the header
  -- and the pagers (the parts Initialize fills in are left out, see
  -- "Tutorial.Pages"), and the editor is up once it has started.
  step "shows lesson 1 before app.wasm, and its program adopts the page" $ \page -> do
    watch <- watchOf walk page
    gate <- hold page "**/app.wasm"
    started <- getMonotonicTime
    writeIORef walk.navigated started
    goto page (site.url <> lesson1.slug <> "/")
    waitHeld gate
    heading <- textContent (locator page "article h1")
    unless (heading == tshow lesson1.number <> ". " <> lesson1.title) $ expectationFailure (Text.unpack ("lesson 1's heading, before app.wasm, is " <> heading))
    report "lesson 1 shows before app.wasm has loaded"
    markServed adoption page
    _ :: Maybe Value <- evaluate page "(window.learnNoReload = 1, null)"
    release gate
    waitStarted page
    repairsSinceMarked page >>= shouldHaveNoRepairs
    -- The gate goes: the compiler's requests, which start now, pass no
    -- route of the test's.
    closeGate gate
    entered watch page
    readOnce (locator page ".learn-editor .cm-editor") "the editor, made in Initialize"
    now <- getMonotonicTime
    report ("adopted with nothing repaired, the editor is up (" <> seconds (now - started) <> " after navigation)")

  -- The compiler loads by itself on a wide screen; each step it shows is
  -- printed with when it showed. The manifest has the version and the
  -- sizes, so the page asks the server for neither: no HEAD request.
  step "loads the compiler by itself, with the manifest's version and sizes" $ \page -> do
    since <- readIORef walk.navigated
    untilReady site page since ""
    ready <- filter (\m -> "learn: compiler ready" `Text.isPrefixOf` m.text) <$> consoleMessages page
    forM_ (take 1 ready) $ \m -> note m.text
    writeIORef walk.loaded True
    heads <- filter (\r -> r.method == "HEAD") <$> requests (pageContext page)
    unless (null heads) $ expectationFailure ("HEAD requests with a manifest that has a version and sizes: " <> show (map (.url) heads))
    status <- textContent (locator page ".learn-status")
    now <- getMonotonicTime
    report ("compiler ready " <> seconds (now - since) <> " after navigation: " <> status)
    report ("the manifest's version (" <> fromMaybe "none" site.manifest.version <> ") and sizes are used: no HEAD request")

  -- A layer with assets (material's script and stylesheet) has them on the
  -- page once it has loaded, whether or not a lesson uses it yet. The load
  -- is asked of the page's JavaScript directly, whose promise the
  -- evaluation awaits.
  step "has a layer's assets on the page once the layer has loaded" $ \page -> do
    requireLoaded walk
    case filter (not . null . (.assets)) site.manifest.layers of
      [] -> pendingWith "no layer of this rootfs has assets"
      layer : _ -> do
        _ :: Maybe Value <- evaluateWith (long site page) "(name) => globalThis.halogenLearn.playground.ensureLayers([name]).then(() => null)" layer.name
        forM_ layer.assets $ \(kind, path) -> do
          let selector = if kind == "stylesheet" then "link[rel=stylesheet][href$=\"/" <> path <> "\"]" else "script[src$=\"/" <> path <> "\"]"
          n <- count (locator page selector)
          unless (n == 1) $ expectationFailure (Text.unpack (path <> " is on the page " <> tshow n <> " times"))
        report ("the " <> layer.name <> " layer's assets are on the page once it has loaded")

  -- Lesson 1: the start renders Hello; the solution adds a paragraph, in a
  -- tree that replaces the last run's rather than adds to it.
  step "lesson 1 compiles and runs as its text says" $ \page -> do
    requireLoaded walk
    said <- compile site page "lesson 1, Start.hs"
    unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack said)
    readOnce (withTextExact "Hello" (locator page "#learn-result p")) "lesson 1's Hello"
    report "lesson 1's start shows Hello"
    click (locator page "button.learn-solution")
    said' <- compile site page "lesson 1, Solution.hs"
    unless ("Compiled and ran" `Text.isPrefixOf` said') $ expectationFailure (Text.unpack said')
    readOnce (withText "My name is Ada." (locator page "#learn-result p")) "lesson 1's solution's paragraph"
    hellos <- count (withTextExact "Hello" (locator page "#learn-result p"))
    unless (hellos == 1) $ expectationFailure ("the last run's tree is replaced, not added to: " <> show hellos <> " Hellos")
    report "lesson 1's solution shows its paragraph"

  -- Lesson 2, by its Next link: the address changes, the page does not
  -- reload, the result starts empty, and the counter counts.
  step "lesson 2, by its Next link, counts" $ \page -> do
    requireLoaded walk
    watch <- watchOf walk page
    click (locator page "header a.learn-next")
    waitForURL page (site.url <> lesson2.slug <> "/")
    entered watch page
    -- Leaving lesson 1 ends its run, in a fiber that waits for the page.
    waitQuiet (long site page)
    contains (locator page "article h1") lesson2.title
    noReload page
    inside <- evaluateOn (locator page "#learn-result") "(e) => e.innerHTML" ()
    unless (inside == ("" :: Text)) $ expectationFailure ("a new lesson starts with an empty result, not " <> show inside)
    readOnce (locator page ".learn-editor .cm-editor") "lesson 2's editor"
    click (locator page "button.learn-solution")
    said <- compile site page "lesson 2, Solution.hs"
    unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack said)
    let plus = withTextExact "+" (locator page "#learn-result button")
    click plus
    click plus
    -- Each click is settled: the compiled program's handler has run.
    readOnce (withText "Count: 2" (locator page "#learn-result p")) "Count: 2"
    report "lesson 2 via Next: the counter counts to 2"

  -- Source, on lesson 2's counter: every element the code built carries
  -- where it was built; hovering the + button outlines it, labelled with
  -- that, and marks its line in the editor; a click goes to the line, and
  -- the program does not count it; the editor's cursor outlines the element
  -- its line made; edits since the compile move the marks with the code;
  -- off, the outline goes and a click counts again. The box is placed by
  -- the page's pointer handler as the event is dispatched, and the line
  -- marked by the program's answer to it, so both are there once the
  -- action has settled: read once.
  step "Source on lesson 2 outlines and marks what made each element, following edits" $ \page -> do
    requireLoaded walk
    let code = Text.lines lesson2.solution
        plus = withTextExact "+" (locator page "#learn-result button")
        minus = withTextExact "-" (locator page "#learn-result button")
        label = locator page ".learn-source-label"
        sourceLine = locator page ".learn-editor .cm-line.learn-source-line"
        plusText = "Increment)] [HH.text \"+\"]"
        activeLine = locator page ".cm-activeLine"
        gutter = locator page ".learn-editor .cm-lineNumbers .cm-activeLineGutter"
        counted n = readOnce (withText ("Count: " <> tshow n) (locator page "#learn-result p")) ("Count: " <> show n)
    (plusLine, plusColumn) <- maybe (expectationFailure "lesson 2's solution has no + button on a line of its own" >> pure (0, 0)) pure (position code "Increment)] [HH.text" "HH.button")
    getAttribute plus "data-learn-src" `shouldReturn` Just (showPosition (plusLine, plusColumn))
    click (locator page "button.learn-source:not([disabled])")
    getAttribute (locator page "button.learn-source") "aria-pressed" `shouldReturn` Just "true"
    hover plus
    outlined plus
    textContent label `shouldReturn` ("Lesson.hs:" <> showPosition (plusLine, plusColumn))
    count (withText plusText sourceLine) `shouldReturn` 1
    click plus
    contains activeLine plusText
    -- The click went to the line, and not to the program.
    counted (2 :: Int)
    -- The pointer off the result: the cursor, at the + button's position
    -- now, outlines it; a line up, the - button's.
    mouseMove page 5 5
    outlined plus
    count sourceLine `shouldReturn` 0
    press page "ArrowUp"
    outlined minus
    -- Edits since the compile: the marks are where the code was compiled,
    -- and the editor maps them through its changes. Two lines added at the
    -- top move the + button's line two down: hovering it labels and marks
    -- that line, a click goes there, and the cursor there outlines it;
    -- taken out again, it is back at its line. Its code deleted, it has
    -- none.
    let plusAt line what = do
          mouseMove page 5 5
          hover plus
          outlined plus
          said <- textContent label
          unless (said == "Lesson.hs:" <> showPosition (line, plusColumn)) $ expectationFailure (Text.unpack ("the outline says " <> said <> ", " <> what))
          count (withText plusText sourceLine) `shouldReturn` 1
          count sourceLine `shouldReturn` 1
          click plus
          at <- textContent gutter
          unless (at == tshow line) $ expectationFailure (Text.unpack ("a click goes to the element's line, " <> what <> ": the cursor is at line " <> at))
          contains activeLine plusText
    press page "ControlOrMeta+Home"
    press page "Enter"
    press page "Enter"
    plusAt (plusLine + 2) "two lines added above"
    mouseMove page 5 5
    outlined plus
    press page "ArrowUp"
    outlined minus
    press page "ControlOrMeta+Home"
    press page "Delete"
    press page "Delete"
    plusAt plusLine "the two lines taken out"
    -- The + button's code deleted (from its indentation to the end of its
    -- line): no line to mark or go to.
    press page "Home"
    press page "Shift+End"
    press page "Backspace"
    press page "ControlOrMeta+Home"
    mouseMove page 5 5
    hover plus
    outlined plus
    textContent label `shouldReturn` "Lesson.hs: deleted since the compile"
    count sourceLine `shouldReturn` 0
    click plus
    textContent gutter `shouldReturn` "1"
    counted (2 :: Int)
    report ("Source follows edits: two lines added above move the + button's line to " <> tshow (plusLine + 2) <> ", taken out it is back at " <> tshow plusLine <> ", and with its code deleted it has no line")
    click (locator page "button.learn-source")
    getAttribute (locator page "button.learn-source") "aria-pressed" `shouldReturn` Just "false"
    count (locator page ".learn-source-box") `shouldReturn` 0
    click plus
    counted (3 :: Int)
    click minus
    counted (2 :: Int)
    report ("Source on lesson 2: the + button is outlined as Lesson.hs:" <> showPosition (plusLine, plusColumn) <> " and its line marked, a click goes there uncounted, the cursor outlines its line's element, and off a click counts")

  -- A type error (a number where HH.text wants text: there is no Num
  -- Text): reported at its line, marked in the editor, the last result
  -- still on the page, and a click on it goes to its line. GHC quotes the
  -- code it compiled, Source's marks and all; the page takes them out of
  -- what it shows.
  step "reports a type error at its line, the last result staying" $ \page -> do
    requireLoaded walk
    let broken = Text.replace "HH.text (\"Count: \" <> T.pack (show count))" "HH.text 42" lesson2.solution
    when (broken == lesson2.solution) $ expectationFailure "lesson 2's solution renders the count with T.pack"
    let line = maybe 0 (+ 1) (findIndex ("HH.text 42" `Text.isInfixOf`) (Text.lines broken))
    setEditor page broken
    said <- compile site page "a type error"
    unless ("did not compile" `Text.isInfixOf` said) $ expectationFailure (Text.unpack said)
    let diagnostic = first (locator page ".learn-diagnostics li.learn-error")
    at <- Text.strip <$> textContent (locator diagnostic ".learn-location")
    unless (("Lesson.hs:" <> tshow line <> ":") `Text.isPrefixOf` at) $ expectationFailure (Text.unpack ("the diagnostic is not at line " <> tshow line <> ": " <> at))
    readOnce (locator page ".cm-lintRange-error") "the error's mark in the editor"
    readOnce (withText "Count: 2" (locator page "#learn-result p")) "the last result, Count: 2"
    click (locator diagnostic "button.learn-location")
    contains (locator page ".cm-activeLine") "HH.text 42"
    said' <- Text.concat <$> ((<>) <$> allTextContents (locator page ".learn-diagnostics") <*> allTextContents (locator page ".learn-stderr"))
    when ("Learn__Spans" `Text.isInfixOf` said') $ expectationFailure (Text.unpack ("a mark in what GHC is shown to say: " <> said'))
    report ("a type error is reported at " <> at <> ", and the counter is still there")

  -- The playground, from lesson 2: Open in playground takes the lesson's
  -- code there, with the lesson's layers (none) and arguments, and it
  -- compiles as it is; Source works there too; Reset puts the template
  -- back, which compiles and counts.
  step "Open in playground takes lesson 2's code there, though localStorage refuses every write" $ \page -> do
    requireLoaded walk
    watch <- watchOf walk page
    click (locator page "button.learn-solution")
    contains (locator page ".learn-editor .cm-content") "subtract 1"
    -- With localStorage refusing every write (full, or denied) and holding
    -- the playground's code from before: the hand-off does not go through
    -- it, so the playground opens with the lesson's code all the same.
    let keptBefore = "module Lesson where -- kept before" :: Text
    _ :: Maybe Value <-
      evaluateWith
        page
        "(kept) => { localStorage.setItem('learn:playground:code', kept); const setItem = Storage.prototype.setItem; window.learnSetItem = setItem; Storage.prototype.setItem = function (key, value) { if (this === localStorage) throw new DOMException('The quota has been exceeded.', 'QuotaExceededError'); return setItem.call(this, key, value); }; return null; }"
        keptBefore
    click (locator page "button.learn-open-playground")
    waitForURL page (site.url <> "playground/")
    entered watch page
    textContent (locator page "article h1") `shouldReturn` "Playground"
    noReload page
    readOnce (locator page ".learn-editor .cm-editor") "the playground's editor"
    handed <- editorText page
    unless ("modify (subtract 1)" `Text.isInfixOf` handed && not ("kept before" `Text.isInfixOf` handed)) $
      expectationFailure ("the playground opens with lesson 2's code, though localStorage refused it: " <> show handed)
    kept :: Maybe Text <- evaluate page "localStorage.getItem('learn:playground:code')"
    kept `shouldBe` Just keptBefore
    _ :: Maybe Value <- evaluate page "(Storage.prototype.setItem = window.learnSetItem, null)"
    waitQuiet (long site page)
    said <- compile site page "lesson 2's code, in the playground"
    unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack said)
    let handedPlus = withTextExact "+" (locator page "#learn-result button")
    click handedPlus
    readOnce (withText "Count: 1" (locator page "#learn-result p")) "Count: 1"
    report "Open in playground takes lesson 2's code to the playground, though localStorage refuses every write, and it compiles and counts there"
    click (locator page "button.learn-source:not([disabled])")
    hover handedPlus
    outlined handedPlus
    click (locator page "button.learn-source")
    getAttribute (locator page "button.learn-source") "aria-pressed" `shouldReturn` Just "false"
    report "Source outlines what the pointer is over in the playground too"

  -- Reset puts the template back and forgets the playground's code; the
  -- template compiles and counts. Its elements are written with HH.div_ and
  -- HH.p_, with no list of properties: each carries where it was built all
  -- the same, and hovering the Clicks: paragraph outlines it, labelled with
  -- its line and column, and marks its line. A paragraph built inside a
  -- chain of $ is marked where its HH.p is.
  step "the playground's template compiles and counts, its every element marked" $ \page -> do
    requireLoaded walk
    click (locator page "button.learn-reset")
    contains (locator page ".learn-editor .cm-content") "Click me"
    said <- compile site page "the playground's template"
    unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack said)
    let clickMe = withText "Click me" (locator page "#learn-result button")
    click clickMe
    click clickMe
    readOnce (withText "Clicks: 2" (locator page "#learn-result p")) "Clicks: 2"
    forgotten :: Maybe Text <- evaluate page "localStorage.getItem('learn:playground:code')"
    forgotten `shouldBe` Nothing
    report "the playground's template compiles and counts, and Reset forgets the playground's code"
    let template = site.template
    clicksAt <- maybe (expectationFailure "the playground's template has no Clicks: paragraph" >> pure (0, 0)) pure (position template "HH.p_ [HH.text (\"Clicks: \"" "HH.p_")
    divAt <- maybe (expectationFailure "the playground's template has no HH.div_ line" >> pure (0, 0)) pure $ do
      i <- findIndex ((== "HH.div_") . Text.strip) template
      pure (i + 1, Text.length (fst (Text.breakOn "HH.div_" (template !! i))) + 1)
    let clicks = withText "Clicks: 2" (locator page "#learn-result p")
    getAttribute clicks "data-learn-src" `shouldReturn` Just (showPosition clicksAt)
    getAttribute (first (locator page "#learn-result [data-learn-src]")) "data-learn-src" `shouldReturn` Just (showPosition divAt)
    count (locator page "#learn-result [data-learn-src]") `shouldReturn` 3
    click (locator page "button.learn-source:not([disabled])")
    getAttribute (locator page "button.learn-source") "aria-pressed" `shouldReturn` Just "true"
    hover clicks
    outlined clicks
    textContent (locator page ".learn-source-label") `shouldReturn` ("Lesson.hs:" <> showPosition clicksAt)
    readOnce (withText "HH.p_ [HH.text (\"Clicks: \"" (locator page ".learn-editor .cm-line.learn-source-line")) "the Clicks: paragraph's line, marked"
    mouseMove page 5 5
    click (locator page "button.learn-source")
    getAttribute (locator page "button.learn-source") "aria-pressed" `shouldReturn` Just "false"
    report ("Source on the playground's template: its HH.p_ paragraph is outlined as Lesson.hs:" <> showPosition clicksAt <> " and its line marked, and its div, button and paragraph are all marked")
    setEditor page chainProgram
    said' <- compile site page "a paragraph built in a chain of $, in the playground"
    unless ("Compiled and ran" `Text.isPrefixOf` said') $ expectationFailure (Text.unpack said')
    let chain = Text.lines chainProgram
        at holding name = maybe "?" showPosition (position chain holding name)
        chainP = at "chain = " "HH.p"
        chainDiv = at "chain = " "HH.div_"
        parenP = at "parenthesised = " "HH.p"
    getAttribute (withText "ab" (locator page "#learn-result p")) "data-learn-src" `shouldReturn` Just chainP
    getAttribute (first (locator page "#learn-result div div[data-learn-src]")) "data-learn-src" `shouldReturn` Just chainDiv
    getAttribute (withTextExact "c" (locator page "#learn-result p")) "data-learn-src" `shouldReturn` Just parenP
    report ("in HH.div_ $ pure $ HH.p [] $ xs <> ys the paragraph is marked " <> chainP <> ", where HH.p is, and the div " <> chainDiv <> "; in (HH.p []) $ xs the paragraph " <> parenP)

  -- Checking the store layer loads it, says so by its checkbox, and lets
  -- a program importing Halogen.Store compile. The layer's archive is held
  -- until the progress line has shown, which a local server would
  -- otherwise make too short to see. That line is the page's JavaScript
  -- reporting its download to the program as it goes, so it is waited for
  -- as a condition; the load itself is the program's (a liftIO on the
  -- page's promise).
  step "checking the store layer loads it, and a program importing Halogen.Store compiles" $ \page -> do
    requireLoaded walk
    unless (any (\l -> l.name == "store") site.manifest.layers) $ pendingWith "this rootfs has no store layer"
    gate <- hold page "**/layers/store*.tar.zst*"
    check (locator page ".learn-layers input[value=\"store\"]")
    waitHeld gate
    locator page ".learn-compiler" `shouldContainText` "Loading the layers you checked"
    release gate
    waitQuiet (long site page)
    closeGate gate
    contains (locator (withText "store" (locator page ".learn-layer")) ".learn-layer-note") "loaded"
    fill (locator page "#learn-extra-args") "-fdefer-typed-holes"
    setEditor page storeProgram
    said <- compile site page "a program importing Halogen.Store, in the playground"
    unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack said)
    readOnce (withText "The store's package is here." (locator page "#learn-result p")) "the store program's paragraph"
    report "checking the store layer loads it, and a program importing Halogen.Store compiles"

  -- Leaving a lesson while its layer is still downloading: the lesson the
  -- reader arrives at gets its own layer once that download ends, and
  -- Compile is enabled there (it stayed disabled once). Both lessons are
  -- reached inside the app, so the compiler is not started over. The first
  -- download is held, and the panel's line about it is the page's
  -- JavaScript's report, a condition.
  step "a lesson entered while another's layer downloads gets its own layer, and Compile" $ \page -> do
    requireLoaded walk
    watch <- watchOf walk page
    case (lessonBySlug site "06-router", lessonBySlug site "07-net") of
      (Just router, Just net) -> do
        gate <- hold page "**/layers/router*.tar.zst*"
        click (locator page "header a.learn-brand")
        click (withText router.title (locator page ".learn-home ol a"))
        waitForURL page (site.url <> router.slug <> "/")
        entered watch page
        waitHeld gate
        locator page ".learn-compiler" `shouldContainText` "libraries"
        click (first (locator page "a.learn-next"))
        waitForURL page (site.url <> net.slug <> "/")
        entered watch page
        release gate
        waitQuiet (long site page)
        closeGate gate
        readOnce (locator page "button.learn-compile:not([disabled])") "Compile, enabled in lesson 7 once its layer has loaded"
        report "a lesson entered while another's layer downloads gets its own layer, and Compile"
      _ -> pendingWith "no lessons 06-router and 07-net"

  -- An error GHC throws rather than reports (one in the module's header)
  -- is a failed compile too, not a program that compiled and stopped.
  step "reports an error GHC throws as a failed compile" $ \page -> do
    requireLoaded walk
    setEditor page "module Lesson (component) where\n\nimport Data.Row (type (.==))\n"
    said <- compile site page "an import GHC cannot parse"
    unless ("did not compile" `Text.isInfixOf` said) $ expectationFailure (Text.unpack said)
    report "an error in the module's header is reported as a failed compile"
