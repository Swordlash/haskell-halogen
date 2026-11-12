-- | What the tutorial's suite knows of the site it tests, and the steps
-- every part of it takes: the lessons and the rootfs' manifest as the page
-- will read them, compiling, loading the compiler, the editor, and what
-- the pages said and asked for.
--
-- How it waits. The tutorial is a Halogen program (prerendered, adopted),
-- and each compiled lesson is a second one in the same page, linked against
-- the rootfs' own haskell-halogen-core: the probe sums both. The tutorial
-- waits for the compiler in fibers that are counted as waiting
-- ("Learn.App": @liftIO@ on the page's promises): loading it, a lesson's
-- layers, a compile ('Browser.compile'), a check ('Browser.typecheck'), and
-- the check's delay after a key (a @liftIO (threadDelay n)@, before work a
-- test waits for). A compiled program's own fibers (a fetch, a library
-- linked, a job in a worker) are counted by its runtime. So after a click
-- that compiles, loads or checks, 'waitQuiet' (with the long timeout) is
-- the wait for all of it, and what the page shows is read once, with no
-- retry. What is neither program's (the Source mode's overlay is placed in
-- the page's pointer handlers, synchronously, so it is there when an action
-- has settled; but CodeMirror's hover tooltip has a timer of its own, the
-- DevTools panel draws on a timer, Three.js arrives from a CDN, the live
-- lesson's client applies the wire's messages in its own JavaScript, and a
-- compiler's progress is reported from the page's JavaScript as it goes) is
-- waited for as a condition with a deadline, each saying why.
module Tutorial.Site
  ( -- * The site
    Site (..)
  , Lesson (..)
  , Manifest (..)
  , Layer (..)
  , readLessons
  , readTemplate
  , lessonBySlug

    -- * Timeouts
  , step
  , long

    -- * The report
  , report
  , note
  , seconds
  , timed
  , tshow

    -- * The tutorial's own steps
  , compiled
  , compile
  , untilReady
  , openLesson
  , setEditor
  , editorText
  , outlined
  , outcome
  , readOnce
  , contains

    -- * What the pages said and asked for
  , Watch
  , newWatch
  , entered
  , checkWatch
  , archive
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, when)
import Data.Aeson (FromJSON (..), Value, withObject, (.!=), (.:), (.:?))
import Data.Char (isDigit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Clock (getMonotonicTime)
import GHC.Stack (HasCallStack)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

----------------------------------------------------------------------
-- The site

-- | The site as it is served, and what the page will read.
data Site = Site
  { url :: Text
  -- ^ The site's address, ending in its base path:
  -- @http://127.0.0.1:<port>/haskell-halogen/@.
  , base :: Text
  -- ^ The base path alone, a slash at each end (@/@ at the root).
  , rootfs :: FilePath
  , rootfsInTree :: Bool
  -- ^ Whether the rootfs is the tree's own (@rootfs/@ in it), rather than
  -- one @LEARN_ROOTFS_DIR@ mounted in its place.
  , manifest :: Manifest
  , manifestJSON :: Value
  -- ^ The manifest as it is, for a page given one with less in it.
  , lessons :: [Lesson]
  , template :: [Text]
  -- ^ The playground's template, a line a list element, as
  -- "Learn.Playground" writes it.
  , timeout :: Int
  -- ^ What loading the compiler, a compile or a layer may take, in ms.
  }

-- | A lesson as the app reads it: the directory's name, the header's title
-- and layers, and the solution's code.
data Lesson = Lesson
  { slug :: Text
  , number :: Int
  , title :: Text
  , layers :: [Text]
  , solution :: Text
  }

-- | What the suite reads of rootfs.json.
data Manifest = Manifest
  { version :: Maybe Text
  , firstPart :: Text
  -- ^ The base archive's first part's path, which a page with no sizes
  -- asks the server about.
  , layers :: [Layer]
  }

-- | A layer: its name, its assets (a kind and a path), and what it says
-- it loads from another site.
data Layer = Layer
  { name :: Text
  , assets :: [(Text, Text)]
  , external :: [Text]
  }

instance FromJSON Manifest where
  parseJSON = withObject "rootfs.json" $ \o -> do
    version <- o .:? "version"
    base <- o .: "rootfs"
    parts <- base .:? "parts" .!= []
    firstPart <- case parts of
      p : _ -> p .: "path"
      [] -> base .: "archive"
    layers <- o .: "layers"
    pure Manifest {version, firstPart, layers}

instance FromJSON Layer where
  parseJSON = withObject "layer" $ \o -> do
    name <- o .: "name"
    assets <- traverse (withObject "asset" (\a -> (,) <$> a .: "kind" <*> a .: "path")) =<< (o .:? "assets" .!= [])
    external <- traverse (withObject "external" (.: "url")) =<< (o .:? "external" .!= [])
    pure Layer {name, assets, external}

-- | Every lesson of @lessons/@, in order: each directory's name, and its
-- @lesson.md@'s header and @Solution.hs@.
readLessons :: FilePath -> IO [Lesson]
readLessons dir = do
  entries <- sort <$> listDirectory dir
  fmap concat $ traverse lessonIn entries
  where
    lessonIn entry = do
      isDir <- doesDirectoryExist (dir </> entry)
      if not isDir
        then pure []
        else do
          markdown <- Text.readFile (dir </> entry </> "lesson.md")
          solution <- Text.readFile (dir </> entry </> "Solution.hs")
          let header = takeWhile (not . Text.null . Text.strip) (Text.lines markdown)
              field key = Text.strip <$> find (not . Text.null) (mapMaybe (Text.stripPrefix (key <> ":")) header)
              slug = Text.pack entry
          pure
            [ Lesson
                { slug
                , number = read (Text.unpack (Text.takeWhile isDigit slug))
                , title = fromMaybe slug (field "title")
                , layers = filter (not . Text.null) (Text.split (\c -> c == ',' || c == ' ') (fromMaybe "" (field "layers")))
                , solution
                }
            ]

-- | The playground's template, as "Learn.Playground" writes it: a Haskell
-- string literal a line of a list, which 'read' reads as GHC would.
readTemplate :: FilePath -> IO [Text]
readTemplate path = do
  source <- Text.readFile path
  let after = snd (Text.breakOn "template =" source)
      literal line = case Text.stripPrefix "[ " (Text.stripStart line) <|> Text.stripPrefix ", " (Text.stripStart line) of
        Just rest | "\"" `Text.isPrefixOf` rest -> Just (Text.pack (read (Text.unpack (Text.stripEnd rest)) :: String))
        _ -> Nothing
      lines' = takeWhile (\l -> Text.strip l /= "]") (drop 1 (dropWhile (not . Text.isInfixOf "T.unlines") (Text.lines after)))
  pure (mapMaybe literal lines')
  where
    a <|> b = maybe b Just a

lessonBySlug :: Site -> Text -> Maybe Lesson
lessonBySlug site s = find (\l -> l.slug == s) site.lessons

----------------------------------------------------------------------
-- Timeouts

-- | An example whose page's calls have 30 s, not the driver's five: the
-- lessons run as bytecode in GHC's interpreter, slower than compiled code,
-- on a CI runner slower still, and every action on them settles. Nothing
-- waits that long unless something is wrong.
step :: String -> (Page -> IO ()) -> SpecWith Page
step name body = it name (body . withTimeout 30000)

-- | The page with the long timeout, for what the compiler does.
long :: Site -> Page -> Page
long site = withTimeout site.timeout

----------------------------------------------------------------------
-- The report

-- | A line of the report: what was checked.
report :: Text -> IO ()
report line = Text.putStrLn ("    ok  " <> line)

-- | A line of the report that checks nothing: what was measured.
note :: Text -> IO ()
note line = Text.putStrLn ("        " <> line)

seconds :: Double -> Text
seconds s = Text.pack (printf "%.1f s" s)

-- | An action and how long it took, in seconds.
timed :: IO a -> IO (a, Double)
timed io = do
  start <- getMonotonicTime
  a <- io
  end <- getMonotonicTime
  pure (a, end - start)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

----------------------------------------------------------------------
-- The tutorial's own steps

-- | Press Compile and wait for the compile to be over: the click's action
-- waits for the compiler in a liftIO, so quiet is when it has come back
-- and the outcome has been rendered, and the compiled program has done
-- what it waits for too (a fetch, a library linked). The outcome is then a
-- new one, which its @data-run@, the compiles started, says: read once.
compiled :: (HasCallStack) => Site -> Page -> IO ()
compiled site page = do
  before <- getAttribute (locator page ".learn-outcome") "data-run"
  click (locator page "button.learn-compile:not([disabled])")
  waitQuiet (long site page)
  after <- getAttribute (locator page ".learn-outcome") "data-run"
  when (after == before) $ expectationFailure ("Compile started no compile: the outcome's data-run is still " <> show before)
  enabled <- count (locator page "button.learn-compile:not([disabled])")
  unless (enabled == 1) $ expectationFailure "Compile is not enabled again once the compile is over"

-- | 'compiled', with a line of the report: what was compiled, how long it
-- took and what the page says of it; the outcome.
compile :: (HasCallStack) => Site -> Page -> Text -> IO Text
compile site page what = do
  ((), took) <- timed (compiled site page)
  said <- outcome page
  note ("compile " <> what <> ": " <> seconds took <> " (" <> said <> ")")
  pure said

outcome :: Page -> IO Text
outcome page = textContent (locator page ".learn-outcome")

-- | Wait for the compiler to load, and say how it went. The program waits
-- for it ('Learn.App''s loadCompiler, a liftIO on the page's promise), so
-- quiet is when it has loaded or failed; meanwhile each phase the page
-- shows is printed when it shows (after the label, for a page other than
-- the first), which is only a report: the phases are the page's
-- JavaScript's, reported to the program as they go.
untilReady :: (HasCallStack) => Site -> Page -> Double -> Text -> IO ()
untilReady site page since label = do
  lastPhase <- newIORef ""
  let poll = forever $ (try @SomeException (statusOf page) >>=) $ \case
        Left _ -> threadDelay 100000
        Right status -> do
          let phase = Text.unwords (map (\w -> if Text.all (\c -> isDigit c || c == '.') w then "…" else w) (Text.words status))
          seen <- readIORef lastPhase
          unless (phase == seen) $ do
            now <- getMonotonicTime
            note (label <> seconds (now - since) <> "  " <> status)
            writeIORef lastPhase phase
          threadDelay 100000
  withAsync poll $ \_ -> waitQuiet (long site page)
  status <- statusOf page
  now <- getMonotonicTime
  note (label <> seconds (now - since) <> "  " <> status)
  unless ("in your browser" `Text.isInfixOf` status) $
    expectationFailure (Text.unpack ("the compiler did not load (the page says " <> status <> ")"))

-- | The status line, and what the compiler's panel says.
statusOf :: Page -> IO Text
statusOf page =
  evaluate
    page
    "[document.querySelector('.learn-status')?.textContent, document.querySelector('.learn-compiler p')?.textContent].filter(Boolean).join(': ')"

-- | A lesson, from the introduction's list, once it can compile: the links
-- are the router's, followed without a page load; the lesson's layers, the
-- check its arrival starts, are the program's to wait for.
openLesson :: (HasCallStack) => Site -> Watch -> Page -> Lesson -> IO ()
openLesson site watch page lesson = do
  click (locator page "header a.learn-brand")
  click (withText lesson.title (locator page ".learn-home ol a"))
  waitForURL page (site.url <> lesson.slug <> "/")
  entered watch page
  waitQuiet (long site page)
  heading <- textContent (locator page "article h1")
  unless (lesson.title `Text.isInfixOf` heading) $ expectationFailure (Text.unpack ("the lesson's heading is " <> heading))
  readOnce (locator page ".learn-editor .cm-editor") "the lesson's editor"
  readOnce (locator page "button.learn-compile:not([disabled])") "Compile, enabled once the lesson's layers have loaded"

-- | The element is there, read once: what put it there has settled.
readOnce :: (HasCallStack) => Locator -> String -> IO ()
readOnce l what = do
  n <- count l
  unless (n >= 1) $ expectationFailure (what <> ": no element matches " <> show l)

-- | The element's text has this in it, read once.
contains :: (HasCallStack) => Locator -> Text -> IO ()
contains l part = do
  actual <- textContent l
  unless (part `Text.isInfixOf` actual) $ expectationFailure ("the text of " <> show l <> " is " <> show actual <> ", without " <> show part)

-- | Replace the editor's text as the reader would paste it. CodeMirror
-- draws the lines in the viewport as it is scrolled, at its own frames, so
-- what it shows is waited for as a condition.
setEditor :: Page -> Text -> IO ()
setEditor page text = do
  click (locator page ".learn-editor .cm-content")
  press page "ControlOrMeta+a"
  insertText page text
  waitForFunctionWith
    page
    "(expected) => document.querySelector('.learn-editor .cm-content').innerText.replace(/\\u200b/g, '').trim().length >= expected.trim().length - 5"
    text

-- | What the editor shows.
editorText :: Page -> IO Text
editorText page = textContent (locator page ".learn-editor .cm-content")

-- | The Source mode's box is over the element: placed by the page's
-- pointer handler, or by the program's answer to the editor's cursor, as
-- the action that moved either settled. Read once.
outlined :: (HasCallStack) => Locator -> IO ()
outlined target = do
  over :: Bool <-
    evaluateOn
      target
      "(el) => { const box = document.querySelector('.learn-source-box'); if (!box || box.hidden) return false; const a = box.getBoundingClientRect(); const b = el.getBoundingClientRect(); return ['left', 'top', 'width', 'height'].every((side) => Math.abs(a[side] - b[side]) < 1); }"
      ()
  unless over $ expectationFailure ("the Source mode's box is not over " <> show target)

----------------------------------------------------------------------
-- What the pages said and asked for

-- | A context's pages, watched: where each went when (by the context's
-- request log, so that a request made there is that lesson's), the
-- console errors it is expected to log, the other sites' addresses that
-- were allowed (for the report), and what went wrong.
data Watch = Watch
  { site :: Site
  , context :: Context
  , expected :: Text -> Bool
  , visits :: IORef [(Int, Text)]
  -- ^ The context's request count when a page went somewhere, and where.
  , allowed :: IORef [Text]
  }

newWatch :: Site -> Context -> (Text -> Bool) -> IORef [Text] -> IO Watch
newWatch site ctx expected allowed = do
  visits <- newIORef []
  pure Watch {site, context = ctx, expected, visits, allowed}

-- | The page is where it is now: what it asks for from here is this
-- address's.
entered :: Watch -> Page -> IO ()
entered watch page = do
  n <- length <$> requests watch.context
  address <- url page
  modifyIORef' watch.visits ((n, address) :)

-- | Nothing logged as an error by the page (but what it is expected to
-- log), nothing thrown; and nothing loaded from another site but what a
-- layer says it loads itself, from a lesson that uses that layer, or from
-- anywhere when the layer's stylesheet names it (the Material Icons font:
-- the stylesheet stays on the page once the layer has loaded, whichever
-- lesson loaded it, and the browser fetches the font when it first needs
-- it). So neither the app nor the compiler depends on a CDN.
checkWatch :: (HasCallStack) => Watch -> Page -> IO ()
checkWatch watch page = do
  logged <- filter (\m -> m.kind == "error" && not (watch.expected m.text)) <$> consoleMessages page
  thrown <- pageErrors page
  unless (null logged && null thrown) $
    expectationFailure ("errors in the page: " <> show (map (.text) logged <> thrown))
  asked <- requests watch.context
  visits <- reverse <$> readIORef watch.visits
  let site = watch.site
      externals = [(u, (l.name, any ((== "stylesheet") . fst) l.assets)) | l <- site.manifest.layers, u <- l.external]
      whereAt i = maybe "" snd (lastMaybe (takeWhile ((<= i) . fst) visits))
      lessonAt address = find (\l -> (site.url <> l.slug <> "/") `Text.isPrefixOf` address) site.lessons
      offSite =
        [ (r.url, address, lookup r.url externals)
        | (i, r) <- zip [0 :: Int ..] asked
        , any (`Text.isPrefixOf` r.url) ["http://", "https://"]
        , not (site.url `Text.isPrefixOf` r.url)
        , let address = whereAt i
        ]
      allowedHere (_, address, layer) = case layer of
        Just (_, True) -> True
        Just (name, False) -> maybe False (\l -> name `elem` l.layers) (lessonAt address)
        Nothing -> False
      (fine, wrong) = (filter allowedHere offSite, filter (not . allowedHere) offSite)
  modifyIORef' watch.allowed (<> [u | (u, _, _) <- fine])
  unless (null wrong) $
    expectationFailure ("the page loaded from another site than the test server's: " <> show [(u, a) | (u, a, _) <- wrong])
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)

-- | A request for an archive of the rootfs or a part of one.
archive :: Request -> Bool
archive r = ".tar.zst" `Text.isSuffixOf` r.url || isPart
  where
    isPart = case Text.breakOnEnd ".tar.zst." r.url of
      (before, rest) -> not (Text.null before) && Text.length rest == 2 && Text.all isDigit rest
