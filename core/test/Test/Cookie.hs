-- | The half of "Web.HTML.Cookie" that is a string format rather than a
-- browser: what the document hands over, and what it will take back.
module Test.Cookie (spec) where

import Data.Map.Strict qualified as M
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)
import Web.HTML.Cookie

spec :: Spec
spec = describe "cookies" $ do
  describe "reading" $ do
    it "splits what the document hands over" $
      assertEqual
        "pairs"
        (M.fromList [("a", "1"), ("b", "2")])
        (parseCookies "a=1; b=2")

    it "keeps a cookie that has a name and no value" $
      assertEqual "bare name" (M.fromList [("flag", "")]) (parseCookies "flag")

    it "keeps an empty value" $
      assertEqual "empty" (M.fromList [("a", "")]) (parseCookies "a=")

    it "finds nothing in an empty jar" $
      assertEqual "empty jar" M.empty (parseCookies "")

    it "decodes what was encoded" $
      assertEqual
        "decoded"
        (M.fromList [("a b", "x; y")])
        (parseCookies "a%20b=x%3B%20y")

    it "leaves an escape it cannot read alone" $
      assertEqual "not an escape" (M.fromList [("a", "100%")]) (parseCookies "a=100%")

    it "keeps the last of two cookies of the same name" $
      assertEqual "last wins" (M.fromList [("a", "2")]) (parseCookies "a=1; a=2")

  describe "writing" $ do
    it "writes a name and a value" $
      assertEqual "plain" "a=1" (renderCookie (defaultCookie "a" "1"))

    it "writes the attributes it was given, and no others" $
      assertEqual
        "attributes"
        "session=abc; Max-Age=3600; Path=/; SameSite=Lax; Secure"
        ( renderCookie
            (defaultCookie "session" "abc")
              { maxAge = Just 3600
              , path = Just "/"
              , sameSite = Just Lax
              , secure = True
              }
        )

    it "encodes what a cookie string cannot carry" $
      assertEqual
        "encoded"
        "a%20b=x%3B%20y"
        (renderCookie (defaultCookie "a b" "x; y"))

    it "survives a round trip" $
      assertEqual
        "round trip"
        (M.fromList [("greeting", "cześć; & = done")])
        (parseCookies (renderCookie (defaultCookie "greeting" "cześć; & = done")))
