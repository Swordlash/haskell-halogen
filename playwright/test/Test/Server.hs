-- | A small server of the tests' own, for what a directory served as it is
-- cannot do: an API that answers @POST@ and sets cookies, and pages sent
-- with a Content Security Policy. Everything else is @test/web@, served as
-- "Halogen.Playwright.Serve" serves it.
module Test.Server (withServer, standInImage) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Halogen.Playwright.Serve (defaultServeOptions, staticApplication)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

-- | The server, for the body, at its base address (with a slash at the
-- end):
--
-- * @api/echo@, any method: JSON of the request's method, body and cookie
--   header, setting two cookies (@a=1@ and @b=2@), as a sign-in does;
-- * @api/data@: a little JSON;
-- * @api/big@: 1000 bytes of text;
-- * @api/pic.png@: a 32 by 32 PNG;
-- * any file of @test/web@, and with @?csp@ in its address, sent with
--   @Content-Security-Policy: script-src 'self'@: the page's own scripts
--   and no others, no inline script, no @eval@.
withServer :: (Text -> IO a) -> IO a
withServer body =
  Warp.testWithApplication (pure app) $ \port -> body ("http://127.0.0.1:" <> Text.pack (show port) <> "/")
  where
    static = staticApplication defaultServeOptions "test/web"
    app request respond = case Wai.pathInfo request of
      ["api", "echo"] -> do
        sent <- Wai.strictRequestBody request
        let cookieHeader = Text.decodeUtf8Lenient <$> lookup "cookie" (Wai.requestHeaders request)
        respond $
          Wai.responseLBS
            HTTP.status200
            [("content-type", "application/json"), ("set-cookie", "a=1; Path=/"), ("set-cookie", "b=2; Path=/")]
            ( encode
                ( object
                    [ "method" .= Text.decodeUtf8Lenient (Wai.requestMethod request)
                    , "body" .= Text.decodeUtf8Lenient (LazyByteString.toStrict sent)
                    , "cookie" .= cookieHeader
                    ]
                )
            )
      ["api", "data"] -> respond (Wai.responseLBS HTTP.status200 [("content-type", "application/json")] "{\"data\":\"from the server\"}")
      ["api", "big"] -> respond (Wai.responseLBS HTTP.status200 [("content-type", "text/plain")] (LazyByteString.replicate 1000 120))
      ["api", "pic.png"] -> respond (Wai.responseLBS HTTP.status200 [("content-type", "image/png")] (LazyByteString.fromStrict standInImage))
      _
        | any ((== "csp") . fst) (Wai.queryString request) ->
            static request (respond . Wai.mapResponseHeaders (("content-security-policy", "script-src 'self'") :))
        | otherwise -> static request respond

-- | A 32 by 32 PNG.
standInImage :: ByteString.ByteString
standInImage =
  either error id . Base64.decode $
    "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAASklEQVR42mP48OUXTRHDCLDAJHUJHjRqwagFI8MCZFTR1ANENMzJoxbQyQKIKeShwWQBqeX+sLeAvPKAGF2jFoxaMHQsGG384kYAzNSeSHKJ2CAAAAAASUVORK5CYII="
