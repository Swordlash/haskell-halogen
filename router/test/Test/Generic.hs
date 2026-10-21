{-# LANGUAGE OverloadedLabels #-}

-- | The codec of a sum type, derived from one codec per constructor.
module Test.Generic (spec) where

import Data.Text qualified as T
import Halogen.Router.Duplex
import Halogen.Router.Duplex.Generic (sum, (.=), (.|))
import Protolude hiding (print, sum)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), oneof, (===))

data Route
  = Home
  | Post Int
  | Tag Text
  | Archive (Int, Int)
  | Posts
  deriving stock (Eq, Show, Generic)

instance Arbitrary Route where
  arbitrary =
    oneof
      [ pure Home
      , Post <$> arbitrary
      , Tag . T.pack <$> arbitrary
      , Archive <$> arbitrary
      , pure Posts
      ]

route :: RouteDuplex' Route
route =
  root
    $ sum
    $ #Home
      .= noArgs
      .| #Post
        .= path "post" (int segment)
      .| #Tag
        .= path "tag" segment
      .| #Archive
        .= path "archive" ((,) <$> lmap fst (int segment) <*> lmap snd (int segment))
      -- A prefix of Post's path, listed after it: the parser does not let one
      -- shadow the other.
      .| #Posts
        .= path "post" noArgs

spec :: Spec
spec = describe "a generic sum" $ do
  prop "round-trips every constructor" $ \r -> parse route (print route r) === Right r
  it "prints each constructor with its own codec" $ do
    print route Home `shouldBe` "/"
    print route (Post 7) `shouldBe` "/post/7"
    print route (Tag "a b") `shouldBe` "/tag/a%20b"
    print route (Archive (2026, 10)) `shouldBe` "/archive/2026/10"
    print route Posts `shouldBe` "/post"
  it "parses whichever constructor reads the whole path" $ do
    parse route "/" `shouldBe` Right Home
    parse route "/post" `shouldBe` Right Posts
    parse route "/post/3" `shouldBe` Right (Post 3)
  it "reports the failure that got furthest" $
    parse route "/archive/2026/x" `shouldBe` Left (Invalid "not a whole number: x")
