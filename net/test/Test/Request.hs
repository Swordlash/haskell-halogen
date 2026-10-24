-- | What a request is made of, on every backend; and natively, where there
-- is no browser to make it, what fetching gives. The requests themselves are
-- the browser suite's (Browser.hs).
module Test.Request (spec) where

import Data.Text (Text)
import Halogen.JSBits (inBrowser)
import Halogen.Net.Fetch
import Test.Hspec

spec :: Spec
spec = do
  describe "request" $ do
    it "has the browser's defaults" $
      request GET "/notes"
        `shouldBe` Request
          { method = GET
          , url = "/notes"
          , headers = []
          , body = Nothing
          , credentials = SameOriginCredentials
          , mode = CorsMode
          }

    it "is changed by record update, though a response has headers and an address too" $ do
      let r =
            (request POST "/notes")
              { headers = [("content-type", "application/json")]
              , body = Just (TextBody "{}")
              , credentials = IncludeCredentials
              }
      r.method `shouldBe` POST
      r.url `shouldBe` "/notes"
      r.headers `shouldBe` [("content-type", "application/json")]
      r.body `shouldBe` Just (TextBody "{}")
      r.credentials `shouldBe` IncludeCredentials
      r.mode `shouldBe` CorsMode

  -- In a browser, or under Node on the browser backends, there is an engine
  -- to make the request.
  (if inBrowser then xdescribe else describe) "natively" $ do
    it "fetch gives NotInBrowserError" $
      outcome <$> fetch (request GET "/notes") `shouldReturn` Left "NotInBrowserError"

    it "a started request gives NotInBrowserError, aborted or not" $ do
      started <- start (request POST "/notes") {body = Just (BytesBody "bytes")}
      outcome <$> response started `shouldReturn` Left "NotInBrowserError"
      abort started
      outcome <$> response started `shouldReturn` Left "NotInBrowserError"
  where
    -- The rejection's name (its message is the package's own sentence), or
    -- the response's status.
    outcome :: Either JSError Response -> Either Text Int
    outcome = either (Left . (.name)) (Right . (.status))
