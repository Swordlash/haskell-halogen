-- | A directory served over HTTP on a free port, for the pages a suite
-- drives.
module Test.Server (withDirectory, staticApp) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))

-- | Serve the directory for the body, which is given the port.
withDirectory :: FilePath -> (Int -> IO a) -> IO a
withDirectory dir = Warp.testWithApplication (pure (staticApp dir))

-- | The files of a directory, @index.html@ for a directory's address, 404
-- for anything else (and for any path that climbs out with @..@).
staticApp :: FilePath -> Wai.Application
staticApp dir request respond = do
  let parts = filter (not . Text.null) (Wai.pathInfo request)
      file = foldl (</>) dir (map Text.unpack parts)
      candidates = if null parts then [dir </> "index.html"] else [file, file </> "index.html"]
  found <- if ".." `elem` parts then pure Nothing else firstExisting candidates
  case found of
    Nothing -> respond (Wai.responseLBS status404 [(hContentType, "text/plain")] "not found")
    Just path -> do
      body <- LazyByteString.readFile path
      respond (Wai.responseLBS status200 [(hContentType, mediaType path)] body)
  where
    firstExisting = \case
      [] -> pure Nothing
      p : ps -> doesFileExist p >>= \e -> if e then pure (Just p) else firstExisting ps

mediaType :: FilePath -> ByteString
mediaType path = case takeExtension path of
  ".html" -> "text/html; charset=utf-8"
  ".js" -> "text/javascript; charset=utf-8"
  ".mjs" -> "text/javascript; charset=utf-8"
  ".css" -> "text/css; charset=utf-8"
  ".json" -> "application/json"
  ".wasm" -> "application/wasm"
  ".svg" -> "image/svg+xml"
  ".png" -> "image/png"
  _ -> "application/octet-stream"
