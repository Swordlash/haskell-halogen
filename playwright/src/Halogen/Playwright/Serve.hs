-- | A built directory served over HTTP for a test's duration, as a static
-- host (GitHub Pages, any plain web server) serves it, so that what the
-- browser loads in a test is what it would load there.
--
-- @
-- main :: IO ()
-- main = serveDirectory "dist-newstyle/wasm/public/vanilla" $ \\base ->
--   hspec $ playwright $ it "starts" $ inPage $ \\page -> visit page base
-- @
--
-- It sends nothing a static host would not: no cross-origin isolation
-- headers, no caching headers of its own, so a page that works here
-- does not depend on them.
module Halogen.Playwright.Serve
  ( serveDirectory
  , serveDirectoryWith
  , ServeOptions (..)
  , defaultServeOptions
  , staticApplication
  , mediaType
  )
where

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types (hContentType, hLocation, methodGet, methodHead, status200, status301, status404, status405)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeExtension, (</>))

-- | How the directory is served.
data ServeOptions = ServeOptions
  { basePath :: Text
  -- ^ The path the directory is served under, as a project's site is
  -- under its name on GitHub Pages (@/haskell-halogen/@): an address
  -- outside it gets a bare 404, and the base without its last slash is
  -- redirected to it. Slashes at either end are optional here.
  , notFound :: Maybe FilePath
  -- ^ A file of the directory (@404.html@) served, with the status 404,
  -- for an address under the base with no file, as Pages serves a site's
  -- @404.html@. Without one, such an address gets a bare 404.
  , mounts :: [(Text, FilePath)]
  -- ^ Paths under the base served from other directories: @("rootfs",
  -- dir)@ serves @dir@ at @<base>rootfs/@, for a part of the site built
  -- elsewhere.
  }
  deriving stock (Eq, Show)

-- | At the root, with no @404.html@ and no mounts.
defaultServeOptions :: ServeOptions
defaultServeOptions = ServeOptions {basePath = "/", notFound = Nothing, mounts = []}

-- | Serve the directory with 'defaultServeOptions' on a free port of
-- 127.0.0.1 for the body, which is given the site's address
-- (@http://127.0.0.1:<port>/@).
serveDirectory :: FilePath -> (Text -> IO a) -> IO a
serveDirectory = serveDirectoryWith defaultServeOptions

-- | 'serveDirectory' with options; the address given to the body ends in
-- the base path.
serveDirectoryWith :: ServeOptions -> FilePath -> (Text -> IO a) -> IO a
serveDirectoryWith options dir body =
  Warp.testWithApplication (pure (staticApplication options dir)) $ \port ->
    body ("http://127.0.0.1:" <> Text.pack (show port) <> normalBase options)

-- | The base path with one slash at each end (just @/@ for the root).
normalBase :: ServeOptions -> Text
normalBase options = case filter (not . Text.null) (Text.splitOn "/" options.basePath) of
  [] -> "/"
  parts -> "/" <> Text.intercalate "/" parts <> "/"

-- | The server itself, as a WAI application:
--
-- * a file's address serves the file with its 'mediaType' (@HEAD@ too, with
--   the length and the modification time a @GET@ would have);
-- * a directory's address serves its @index.html@; without its last slash
--   it is redirected to the address with one, as a static host does, so
--   that the page's relative addresses resolve inside it;
-- * an address outside the base or with no file gets a 404 (the 'notFound'
--   page, under the base), and so does one with a segment that would lead
--   out of the directory: @..@, or one holding a slash, which a request
--   can carry as @%2F@ and which, joined to the directory as it is, would
--   name a file anywhere on the machine;
-- * a method other than @GET@ and @HEAD@ gets a 405.
staticApplication :: ServeOptions -> FilePath -> Wai.Application
staticApplication options dir request respond
  | Wai.requestMethod request `notElem` [methodGet, methodHead] =
      respond (Wai.responseLBS status405 [(hContentType, "text/plain")] "only GET and HEAD")
  | otherwise = case stripBase (Wai.pathInfo request) of
      Nothing
        | Wai.pathInfo request == baseParts && not (null baseParts) -> redirect (normalBase options)
        | otherwise -> respond bare404
      Just parts
        | any leadsOut parts -> respond bare404
        | otherwise -> serve parts
  where
    baseParts = filter (not . Text.null) (Text.splitOn "/" (normalBase options))
    -- WAI gives the path's segments decoded, so a segment can hold what no
    -- file's name can: a separator (from @%2F@ or @%5C@), which would make
    -- it a path of its own (an absolute one, if it starts with a slash), or
    -- a NUL. None is ever a file of the directory.
    leadsOut part = part `elem` ["..", "."] || Text.any (`elem` ['/', '\\', '\0']) part
    -- The path under the base, with a trailing slash as an empty last
    -- part (WAI's pathInfo keeps it so).
    stripBase path
      | baseParts `isPrefixOf` path && length path > length baseParts = Just (drop (length baseParts) path)
      | null baseParts = Just path
      | otherwise = Nothing
    -- The directory a path under the base is in, and the rest of it.
    located parts = case [(d, rest) | (prefix, d) <- options.mounts, Just rest <- [under (segments prefix) parts]] of
      found : _ -> found
      [] -> (dir, parts)
    under prefix parts
      | prefix `isPrefixOf` parts = Just (drop (length prefix) parts)
      | otherwise = Nothing
    segments = filter (not . Text.null) . Text.splitOn "/"
    serve parts = do
      let (root, rest) = located parts
          named = filter (not . Text.null) rest
          path = foldl (</>) root (map Text.unpack named)
          slashed = null parts || last parts == ""
      isDirectory <- doesDirectoryExist path
      if isDirectory && not slashed
        then redirect (Text.decodeUtf8Lenient (Wai.rawPathInfo request <> "/" <> Wai.rawQueryString request))
        else do
          let file = if isDirectory then path </> "index.html" else path
          exists <- doesFileExist file
          if exists then respond (fileResponse file) else missing
    missing = do
      page <- filterM doesFileExist (map (dir </>) (maybe [] pure options.notFound))
      case page of
        -- Read here rather than sent as a file: warp answers a file with
        -- 200 whatever status it is given.
        file : _ -> LazyByteString.readFile file >>= respond . Wai.responseLBS status404 [(hContentType, mediaType file)]
        [] -> respond bare404
    -- Warp sends a file with its length and its modification time, and
    -- answers HEAD and conditional requests for it as a static host does.
    fileResponse file = Wai.responseFile status200 [(hContentType, mediaType file)] file Nothing
    redirect to = respond (Wai.responseLBS status301 [(hLocation, Text.encodeUtf8 to)] "")
    bare404 = Wai.responseLBS status404 [(hContentType, "text/plain")] "not found"

-- | The media type a static host sends for a file, by its extension.
-- @application/wasm@ matters most: @WebAssembly.instantiateStreaming@
-- refuses a binary served as anything else.
mediaType :: FilePath -> ByteString
mediaType path = Char8.pack $ case map toLower (takeExtension path) of
  ".html" -> "text/html; charset=utf-8"
  ".js" -> "text/javascript; charset=utf-8"
  ".mjs" -> "text/javascript; charset=utf-8"
  ".css" -> "text/css; charset=utf-8"
  ".json" -> "application/json"
  ".map" -> "application/json"
  ".webmanifest" -> "application/manifest+json"
  ".wasm" -> "application/wasm"
  ".svg" -> "image/svg+xml"
  ".png" -> "image/png"
  ".jpg" -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".gif" -> "image/gif"
  ".webp" -> "image/webp"
  ".ico" -> "image/x-icon"
  ".woff2" -> "font/woff2"
  ".woff" -> "font/woff"
  ".ttf" -> "font/ttf"
  ".txt" -> "text/plain; charset=utf-8"
  ".md" -> "text/markdown; charset=utf-8"
  ".hs" -> "text/plain; charset=utf-8"
  ".gz" -> "application/gzip"
  ".zst" -> "application/zstd"
  ".tar" -> "application/x-tar"
  _ -> "application/octet-stream"
