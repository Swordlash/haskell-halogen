-- | The tutorial (examples/learn) as GitHub Pages deploys it, in headless
-- Chromium, compiler and all: what @npm run test-learn@ runs, and what
-- gates the deploy.
--
-- > npm run test-learn     # after: npm run build-rootfs, sh toolchain/build-wasm.sh learn
-- > HALOGEN_BASE_PATH=/haskell-halogen/ npm run test-learn   # the deployed form, built the same way
--
-- It builds nothing. It serves the built tree (@dist-newstyle/wasm/public/learn@
-- from the repository's root, or the directory @HALOGEN_EXAMPLE_DIR@ names)
-- as Pages serves what it deploys: the app, its pages, and the compiler's
-- rootfs in @rootfs/@ inside it, where @examples/learn/bundle.sh@ copied it,
-- under @HALOGEN_BASE_PATH@ (default @/@), the base path the pages were
-- built for. @LEARN_ROOTFS_DIR@, when set, is served at @rootfs/@ in place
-- of the tree's own (for a tree built before the rootfs was, or another
-- rootfs). @LEARN_TIMEOUT@ is the seconds loading the compiler, a compile
-- or a layer may take (300); @LEARN_DEBUG=1@ prints what the pages log, as
-- it arrives (it sets @HALOGEN_PLAYWRIGHT_CONSOLE@, which the driver reads).
--
-- What it checks: each lesson's page arrives with its text already in it,
-- and the program adopts every page with nothing repaired; the compiler
-- loads (the timings are printed), with the manifest's version and sizes
-- and so without a HEAD request; the first two lessons compile and run as
-- their text says, and a type error is reported at its line with the last
-- result still showing; Source on lesson 2 outlines what the pointer and
-- the editor's cursor are on, marks its line and goes there on a click the
-- program does not get, at the line its code is on after edits since the
-- compile, and no mark shows in what GHC says; the playground's page is
-- prerendered, takes lesson 2's code from its Open in playground (with
-- localStorage refusing writes), compiles its template, whose every element
-- Source marks (its HH.p_ paragraph outlined at its line and column), marks
-- a paragraph built inside a chain of $ where its HH.p is, loads the store
-- layer when it is checked, and keeps all that for the next visit; every
-- lesson in examples/learn/lessons compiles its Solution.hs without a
-- diagnostic and shows something (two lessons at a time, on two pages, and
-- one line per lesson, which is how a new lesson is checked); an error GHC
-- throws rather than reports is still a failed compile; lesson 13's spec
-- runs in the page, its Start.hs with one failure and its Solution.hs with
-- none, by hspec's report under the editor, and leaves nothing mounted;
-- lesson 19's scene is drawn and a click on its sphere changes the colour
-- the text names; the page checks the code as the reader types (lesson 20:
-- the types of the bindings listed and over their names with no Compile, a
-- typed error marked within the check's delay, and a check never reported
-- over a compile asked for after it); the routes survive back and forward;
-- lesson 6's hash router keeps the lesson's address under the page's
-- <base href>, Back included; what lesson 5's Finalize prints shows on
-- every compile after the first; Escape then Tab leaves the editor; the
-- reader's code survives a reload, and the compiler a second visit, from
-- the Cache API; on a narrow screen nothing large is downloaded until
-- asked; a compiler that fails to load can be retried, and loads from a
-- manifest without a version or sizes too (asking the server for them); a
-- layer that fails to load is its lessons' failure alone, and can be
-- retried; nothing is logged as an error; and nothing is loaded from
-- another site but what a layer loads itself.
--
-- How it waits is in "Tutorial.Site". The walk's examples share one page
-- ('onOnePage'), in order, so one of them cannot be run alone with
-- @--match@. Without the built tree the examples are pending, with the
-- commands that build it, unless @HALOGEN_EXAMPLE_REQUIRED=1@ (as
-- @npm run test-learn@ sets it), when they fail.
module Main (main) where

import Control.Monad (unless, when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Serve (ServeOptions (..), defaultServeOptions, serveDirectoryWith)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Text.Read (readMaybe)
import Tutorial.Later qualified as Later
import Tutorial.Lessons qualified as Lessons
import Tutorial.Loading qualified as Loading
import Tutorial.Pages qualified as Pages
import Tutorial.Site
import Tutorial.Walk (newWalk, startSpec)

main :: IO ()
main = do
  -- The report quotes the page, which writes "…" and hspec's "✔": in UTF-8
  -- whatever the locale, rather than fail on them under LANG=C.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  tree <- fromMaybe "../../dist-newstyle/wasm/public/learn" <$> lookupEnv "HALOGEN_EXAMPLE_DIR"
  rootfsEnv <- (>>= \d -> if null d then Nothing else Just d) <$> lookupEnv "LEARN_ROOTFS_DIR"
  required <- (== Just "1") <$> lookupEnv "HALOGEN_EXAMPLE_REQUIRED"
  debug <- maybe False (not . null) <$> lookupEnv "LEARN_DEBUG"
  when debug $ setEnv "HALOGEN_PLAYWRIGHT_CONSOLE" "1"
  basePath <- maybe "/" Text.pack <$> lookupEnv "HALOGEN_BASE_PATH"
  timeoutSeconds <- maybe 300 (fromMaybe 300 . readMaybe) <$> lookupEnv "LEARN_TIMEOUT"
  let rootfs = fromMaybe (tree </> "rootfs") rootfsEnv
      base = case filter (not . Text.null) (Text.splitOn "/" basePath) of
        [] -> "/"
        parts -> "/" <> Text.intercalate "/" parts <> "/"
  built <- (&&) <$> doesFileExist (tree </> "index.html") <*> doesFileExist (rootfs </> "rootfs.json")
  if not built
    then
      hspec
        $ describe "the tutorial, as Pages deploys it"
        $ it "is served"
        $ (if required then expectationFailure else pendingWith)
          ( "no tutorial at "
              <> tree
              <> " with a rootfs at "
              <> rootfs
              <> ": build the rootfs (npm run build-rootfs), then the app (sh toolchain/build-wasm.sh learn), or set LEARN_ROOTFS_DIR; npm run test-learn runs this suite"
          )
    else do
      manifestJSON <- either (\e -> fail ("rootfs.json: " <> e)) pure =<< Aeson.eitherDecodeFileStrict (rootfs </> "rootfs.json")
      manifest <- either (\e -> fail ("rootfs.json: " <> e)) pure (Aeson.Types.parseEither Aeson.parseJSON manifestJSON)
      lessons <- readLessons "lessons"
      template <- readTemplate "src/Learn/Playground.hs"
      allowed <- newIORef []
      let options = defaultServeOptions {basePath = base, mounts = [("rootfs", dir) | Just dir <- [rootfsEnv]]}
      serveDirectoryWith options tree $ \address -> do
        let site =
              Site
                { url = address
                , base
                , rootfs
                , rootfsInTree = isNothing rootfsEnv
                , manifest
                , manifestJSON
                , lessons
                , template
                , timeout = timeoutSeconds * 1000
                }
        walk <- newWalk site allowed
        hspec (spec site walk allowed)
  where
    spec site walk allowed = playwright $ describe "the tutorial, as Pages deploys it" $ do
      it "is served from a tree whose rootfs has a version, and is a directory" $ \_ -> do
        unless (length site.lessons >= 2) $ expectationFailure "at least the two first lessons"
        unless (any ("HH.p_ [HH.text (\"Clicks: \"" `Text.isInfixOf`) site.template) $ expectationFailure "the playground's template has its Clicks: paragraph"
        when (isNothing site.manifest.version) $ expectationFailure "rootfs.json has no version"
        -- What Pages is given is the files themselves: a link would be
        -- followed by the upload or not, and either way is not what this
        -- tests.
        when site.rootfsInTree $ do
          isDir <- doesDirectoryExist site.rootfs
          isLink <- pathIsSymbolicLink site.rootfs
          unless (isDir && not isLink) $ expectationFailure (site.rootfs <> " is not a directory, or is a link")
        report ("rootfs " <> (if site.rootfsInTree then "in the tree" else "from " <> Text.pack site.rootfs) <> ", version " <> fromMaybe "none" site.manifest.version)
      Pages.spec site allowed
      describe "the walk" $ onOnePage defaultContextOptions {viewport = Just (1400, 900), acceptDialogs = True} (\_ -> pure ()) $ do
        startSpec walk
        Lessons.spec walk
        Later.spec walk
      Loading.spec site allowed
      it "loaded nothing from another site but the layers' own" $ \_ -> do
        seen <- nub <$> readIORef (allowed :: IORef [Text])
        report ("nothing loaded from another site but the layers' external dependencies (" <> (if null seen then "none of them" else Text.intercalate ", " seen) <> ")")
