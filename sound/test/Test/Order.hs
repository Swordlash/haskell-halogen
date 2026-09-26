module Test.Order (spec) where

import Data.List (sort, unfoldr)
import Halogen.Sound.Order
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)

takeTracks :: Int -> Order Int -> [Int]
takeTracks n = take n . unfoldr nextTrack

spec :: Spec
spec = describe "album order" $ do
  it "plays every track once a round, over many rounds and seeds" $
    and
      [ map sort (chunks (takeTracks 40 (newOrder s [1 .. 5]))) == replicate 8 [1 .. 5]
      | s <- [0 .. 200]
      ]
      `shouldBe` True

  it "never plays a track twice in a row across rounds" $
    and
      [ and (zipWith (/=) ts (drop 1 ts))
      | s <- [0 .. 200]
      , let ts = takeTracks 60 (newOrder s [1 .. 3])
      ]
      `shouldBe` True

  it "shows the tracks to come without taking them" $ do
    let o = newOrder 7 [1 .. 6 :: Int]
    upcoming 4 o `shouldBe` takeTracks 4 o

  it "differs between seeds" $
    (length . filter id)
      [takeTracks 6 (newOrder s [1 .. 6]) /= takeTracks 6 (newOrder (s + 1) [1 .. 6]) | s <- [0 .. 50]]
      `shouldBe` 51

  it "has nothing to play for an empty album" $
    takeTracks 3 (newOrder 1 ([] :: [Int])) `shouldBe` []
  where
    chunks [] = []
    chunks xs = take 5 xs : chunks (drop 5 xs)
