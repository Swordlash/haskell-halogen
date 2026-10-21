-- | Write the vanilla example's page with the component already in it.
--
--   halogen-example-vanilla-prerender TEMPLATE OUTPUT-DIR
--
-- The template, @examples/vanilla/web/index.html@, has an empty
-- @\<div id=\"app\">\<\/div>@; the @index.html@ written into OUTPUT-DIR has
-- the component's HTML in it, rendered natively by "Halogen.Static", so the
-- page shows the example before @app.wasm@ has loaded. That is the
-- interface @toolchain/build-wasm.sh@ runs every example's prerender
-- executable with.
module Main (main) where

import Data.ByteString qualified as B
import Example.Vanilla (component)
import Halogen.Static (fillMountPoint, renderToHtml)
import Protolude
import System.FilePath ((</>))

main :: IO ()
main = do
  (template, outputDir) <-
    getArgs >>= \case
      [template, outputDir] -> pure (template, outputDir)
      _ -> die "usage: halogen-example-vanilla-prerender TEMPLATE OUTPUT-DIR"
  page <- decodeUtf8 <$> B.readFile template
  html <- renderToHtml component ()
  either (die . ((toS template <> ": ") <>)) (B.writeFile (outputDir </> "index.html") . encodeUtf8) (fillMountPoint "app" html page)
