{-# LANGUAGE MultiWayIf #-}

-- | Reading the lessons' directory at compile time, for "Learn.Lessons".
module Learn.Lessons.Embed (embedLessons) where

import Control.Monad.Fail (fail)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q, loc_filename, location, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Haskell.TH.Syntax qualified as TH
import Learn.Lesson (Lesson (..), parseDirectoryName, parseLesson)
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (</>))

-- | Every lesson in the directory of this name, nearest the module that
-- splices this among it and the directories above it (the package's), as a
-- @[Lesson]@, in the order its @index.txt@ lists them: one lesson
-- directory per line (blank lines and lines starting with @#@ aside). A
-- lesson that cannot be read fails the build, with what is wrong with it, and
-- so does a lesson directory the index leaves out.
--
-- Files are read as UTF-8, whatever the locale the build runs in.
--
-- Each file read is a dependency of the module, so editing one recompiles
-- it, and the package's @extra-source-files@ make cabal rebuild the package
-- for it. A file or a directory that appears is another matter: neither GHC
-- nor cabal notices one (cabal 3.16 does not rebuild for a new file that its
-- globs match), which is what the index is for. Adding a lesson edits it, and
-- that rebuilds the package and recompiles this module.
embedLessons :: FilePath -> Q Exp
embedLessons name = do
  here <- loc_filename <$> location
  root <- runIO $ findDirectory (takeDirectory here) name
  let indexPath = root </> "index.txt"
  addDependentFile indexPath
  listed <-
    runIO (B.readFile indexPath)
      <&> filter (\l -> not (T.null l) && not ("#" `T.isPrefixOf` l))
      . map T.strip
      . T.lines
      . decodeUtf8
  entries <- runIO $ listDirectory root
  directories <- runIO $ filterM (doesDirectoryExist . (root </>)) entries
  for_ directories $ \entry ->
    unless (toS entry `elem` listed) $
      fail (root </> entry <> ": a lesson directory that " <> indexPath <> " does not list")
  lessons <- for listed $ \slug -> do
    let dir = root </> toS slug
    exists <- runIO $ doesDirectoryExist dir
    unless exists $ fail (indexPath <> ": " <> toS slug <> " is not a directory beside it")
    either (fail . toS) pure (void (parseDirectoryName slug))
    let source file = do
          let path = dir </> file
          present <- runIO $ doesFileExist path
          if present
            then Just <$> (addDependentFile path >> runIO (decodeUtf8 <$> B.readFile path))
            else pure Nothing
        required file = source file >>= maybe (fail (dir </> file <> ": a lesson needs this file")) pure
    markdown <- required "lesson.md"
    start <- required "Start.hs"
    solution <- required "Solution.hs"
    main <- source "Main.hs"
    either (fail . toS) pure (parseLesson slug markdown start solution main)
  checkNumbers lessons
  TH.lift lessons
  where
    checkNumbers :: [Lesson] -> Q ()
    checkNumbers lessons =
      for_ (zip [1 :: Int ..] lessons) $ \(n, lesson) ->
        when (lesson.number /= n) $
          fail (toS lesson.slug <> (": lessons are numbered 01, 02, ... in order, with no gaps, and this one should be " <> show n :: [Char]))

findDirectory :: FilePath -> FilePath -> IO FilePath
findDirectory start name = search =<< makeAbsolute start
  where
    search dir = do
      let candidate = dir </> name
      exists <- doesDirectoryExist candidate
      if
        | exists -> pure candidate
        | takeDirectory dir == dir -> die (toS ("embedLessons: no directory " <> name <> " above " <> start))
        | otherwise -> search (takeDirectory dir)
