-- | Every combinator prints what it parses and parses what it prints.
module Test.Duplex (spec) where

import Data.Text qualified as T
import Halogen.Router.Duplex
import Protolude hiding (many, optional, print)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, forAll, listOf, listOf1, (===))

text :: Gen Text
text = T.pack <$> arbitrary

-- | Printing a value and parsing what was printed gives the value back.
roundTrips :: (Eq a, Show a) => RouteDuplex' a -> a -> Property
roundTrips codec a = parse codec (print codec a) === Right a

data Search = Search {query :: Text, page :: Maybe Int, draft :: Bool}
  deriving stock (Eq, Show)

instance Arbitrary Search where
  arbitrary = Search <$> text <*> arbitrary <*> arbitrary

search :: RouteDuplex' Search
search =
  root
    $ path "search"
    $ Search
      <$> lmap (.query) (param "q")
      <*> lmap (.page) (optional (int (param "page")))
      <*> lmap (.draft) (flag (param "draft"))

spec :: Spec
spec = describe "route codecs" $ do
  describe "round trips" $ do
    prop "segment" $ forAll text (roundTrips (root (prefix "s" segment)))
    prop "segment, at the root" $ forAll text (roundTrips segment)
    prop "prefix and suffix" $ forAll text (roundTrips (root (suffix (prefix "a" segment) "z")))
    prop "path" $ forAll text (roundTrips (root (path "a/b/" segment)))
    prop "param" $ forAll text (roundTrips (root (prefix "p" (param "key with space"))))
    prop "flag" $ roundTrips (root (prefix "f" (flag (param "on"))))
    prop "int" $ roundTrips (root (int segment))
    prop "boolean" $ roundTrips (root (boolean segment))
    prop "string" $ forAll text (roundTrips (root (prefix "t" (string segment))))
    prop "as" $ roundTrips (root (as show (maybe (Left "not a char") Right . readMaybe) segment)) . (identity :: Char -> Char)
    prop "optional segment" $ roundTrips (root (prefix "o" (optional (int segment))))
    prop "optional param" $ roundTrips (root (prefix "o" (optional (int (param "n")))))
    prop "many" $ forAll (listOf text) (roundTrips (root (prefix "m" (many segment))))
    prop "many1" $ forAll (listOf1 text) (roundTrips (root (prefix "m" (many1 segment))))
    prop "rest" $ forAll (listOf text) (roundTrips (root (prefix "r" rest)))
    prop "end" $ forAll text (roundTrips (root (end (prefix "e" segment))))
    prop "noArgs" $ roundTrips (root (path "home/" noArgs))
    prop "a record, built with lmap and <*>" $ roundTrips search
    prop "an optional value before a fixed segment" $ roundTrips (root (suffix (optional (int segment)) "x"))
    prop "many values before a fixed segment" $ roundTrips (root (suffix (many (int segment)) "x"))
    prop "percent-encoding" $ forAll text $ \t -> decodeComponent (encodeComponent t) === t

  describe "addresses" $ do
    it "prints the root as a slash, and a trailing slash as an empty segment" $ do
      print (root noArgs) () `shouldBe` "/"
      print (root (path "posts/" noArgs)) () `shouldBe` "/posts/"
      print (root (path "posts" noArgs)) () `shouldBe` "/posts"
    it "encodes as encodeURIComponent does" $
      encodeComponent "a b/ä?&=#!'()*~" `shouldBe` "a%20b%2F%C3%A4%3F%26%3D%23!'()*~"
    it "leaves a stray percent sign as it is" $
      decodeComponent "100%-%zz%41" `shouldBe` "100%-%zzA"
    it "prints the query, a parameter without a value as its key alone" $
      print search (Search "a&b" (Just 2) True) `shouldBe` "/search?q=a%26b&page=2&draft"
    it "reads parameters in any order, and ignores the fragment" $
      parse search "/search?draft&page=3&q=x#top" `shouldBe` Right (Search "x" (Just 3) True)
    it "takes an address apart" $
      parseAddress "/a/b%20c/?x=1&y#frag" `shouldBe` RouteState ["", "a", "b c", ""] [("x", "1"), ("y", "")] "frag"

  describe "parsing" $ do
    it "reads a route only when the whole path is consumed" $ do
      let post = root (prefix "post" (int segment))
      parse post "/post/1" `shouldBe` Right 1
      parse post "/post/1/extra" `shouldBe` Left (ExpectedEndOfPath "extra")
    it "says which segment it expected" $
      parse (root (path "post" noArgs)) "/page" `shouldBe` Left (Expected "post" "page")
    it "says that a parameter is missing" $
      parse search "/search" `shouldBe` Left (MissingParam "q")
    it "says why a value is not one" $
      parse (root (int segment)) "/x" `shouldBe` Left (Invalid "not a whole number: x")
    it "says that the path ended too soon" $
      parse (root (prefix "post" (int segment))) "/post" `shouldBe` Left EndOfPath
    it "is told by end where a route stops" $
      parse (root (end (prefix "a" noArgs))) "/a/b" `shouldBe` Left (ExpectedEndOfPath "b")
