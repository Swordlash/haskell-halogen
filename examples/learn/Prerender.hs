-- | Write the tutorial's pages, one per route, each with "Learn.App"
-- already rendered into it for that route.
--
--   halogen-example-learn-prerender TEMPLATE OUTPUT-DIR
--
-- The introduction goes to @OUTPUT-DIR\/index.html@ and each lesson's to
-- @OUTPUT-DIR\/\<NN-slug>\/index.html@, so a static server has a file at
-- every address the tutorial has. The site's base path, from
-- @HALOGEN_BASE_PATH@ (@\/@ when unset; @toolchain\/build-wasm.sh@ passes it
-- on), is written into each page's @\<base href>@: the template's relative
-- @.\/index.js@ and @.\/learn.css@ then resolve to the same files from every
-- directory, the program reads its router's base from it
-- ('Router.pageBase'), and the compiler is looked for at @rootfs\/@ under it.
module Main (main) where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.Static (fillMountPoint, renderToHtml)
import Halogen.VDom.DOM.Monad (runMemDOM)
import Learn.App qualified as App
import Learn.Lesson (Lesson (..))
import Learn.Route (Route (..), lessonOf, route, routes)
import Protolude
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

main :: IO ()
main = do
  (template, outputDir) <-
    getArgs >>= \case
      [template, outputDir] -> pure (template, outputDir)
      _ -> die "usage: halogen-example-learn-prerender TEMPLATE OUTPUT-DIR"
  base <- maybe "/" toS <$> lookupEnv "HALOGEN_BASE_PATH"
  page <- decodeUtf8 <$> B.readFile template
  for_ routes $ \r -> do
    -- A router at the route's address, as the browser's is when the page
    -- loads, so that the component renders that route.
    router <- runMemDOM (Router.newRouter base Home route)
    runMemDOM (Router.navigate router r)
    html <- renderToHtml App.component router
    let home = Router.href router Home
        based = withBase home (withTitle r page)
    filled <- either (die . ((toS template <> ": ") <>)) pure (fillMountPoint "app" html based)
    let dir = outputDir </> toS (T.drop (T.length home) (Router.href router r))
    createDirectoryIfMissing True dir
    B.writeFile (dir </> "index.html") (encodeUtf8 filled)
  putStrLn ("Wrote " <> show (length routes) <> " pages." :: Text)

-- | The page with a @\<base href>@ first in its head, before anything it
-- applies to.
withBase :: Text -> Text -> Text
withBase base = T.replace "<head>" ("<head>\n    <base href=\"" <> escape base <> "\">")

-- | A lesson's page is titled after it.
withTitle :: Route -> Text -> Text
withTitle r = case lessonOf r of
  Nothing -> identity
  Just lesson -> T.replace "<title>Learn Halogen</title>" ("<title>" <> escape (show lesson.number <> ". " <> lesson.title) <> " — Learn Halogen</title>")

-- | Text as it is written in an attribute's value in double quotes or in an
-- element's text: a title may hold a @<@ or an @&@ (\"Forms & inputs\"), and
-- a base path anything @HALOGEN_BASE_PATH@ says.
escape :: Text -> Text
escape = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  c -> T.singleton c
