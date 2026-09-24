-- | The runner's JavaScript, compiled into the executable so that it is
-- found wherever the executable is: installed, in a build directory, or run
-- by cabal as a test wrapper from another package's directory.
module Embed (embedText) where

import Language.Haskell.TH (Exp (..), Lit (..), Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
import System.IO (IOMode (..), hGetContents', hSetEncoding, utf8, withFile)

-- | A file of this package, relative to its root, as a string literal.
-- Editing the file rebuilds the executable.
embedText :: FilePath -> Q Exp
embedText relative = do
  path <- makeRelativeToProject relative
  addDependentFile path
  LitE . StringL <$> runIO (withFile path ReadMode (\handle -> hSetEncoding handle utf8 >> hGetContents' handle))
