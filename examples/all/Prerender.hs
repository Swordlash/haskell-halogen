-- | Write the gallery's pages, one per route, each with "Gallery.Shell"
-- already rendered into it for that route.
--
--   halogen-example-all-prerender TEMPLATE OUTPUT-DIR
--
-- The landing page goes to @OUTPUT-DIR\/index.html@ and each example's to
-- @OUTPUT-DIR\/\<slug>\/index.html@, so a static server has a file at every
-- address the gallery has. The site's base path, from @HALOGEN_BASE_PATH@
-- (@\/@ when unset; @toolchain\/build-wasm.sh@ passes it on), is written into
-- each page's @\<base href>@: the template's relative @.\/index.js@ and
-- @.\/material.css@ then resolve to the same files from every directory, and
-- the program reads its router's base from it ('Router.pageBase').
module Main (main) where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Gallery.Shell (Route (..))
import Gallery.Shell qualified as Shell
import Halogen.Router qualified as Router
import Halogen.Static (fillMountPoint, renderToHtml)
import Halogen.VDom.DOM.Monad (runMemDOM)
import Protolude
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

main :: IO ()
main = do
  (template, outputDir) <-
    getArgs >>= \case
      [template, outputDir] -> pure (template, outputDir)
      _ -> die "usage: halogen-example-all-prerender TEMPLATE OUTPUT-DIR"
  base <- maybe "/" toS <$> lookupEnv "HALOGEN_BASE_PATH"
  page <- decodeUtf8 <$> B.readFile template
  router <- runMemDOM (Router.newRouter base Home Shell.route)
  for_ [minBound .. maxBound] $ \r -> do
    html <- renderToHtml (Shell.component router) r
    let based = withBase (Router.href router Home) (withTitle r page)
    filled <- either (die . ((toS template <> ": ") <>)) pure (fillMountPoint "app" html based)
    let dir = outputDir </> toS (T.drop (T.length (Router.href router Home)) (Router.href router r))
    createDirectoryIfMissing True dir
    B.writeFile (dir </> "index.html") (encodeUtf8 filled)

-- | The page with a @\<base href>@ first in its head, before anything it
-- applies to.
withBase :: Text -> Text -> Text
withBase base = T.replace "<head>" ("<head>\n    <base href=\"" <> base <> "\">")

-- | An example's page is titled after it.
withTitle :: Route -> Text -> Text
withTitle = \case
  Home -> identity
  r -> T.replace "<title>haskell-halogen examples</title>" ("<title>" <> Shell.title r <> " — haskell-halogen examples</title>")
