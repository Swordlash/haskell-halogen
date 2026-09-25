-- | The order an album is played in.
--
-- Every track once per round, the rounds shuffled one by one, and never the
-- same track twice in a row across the seam between two rounds. The
-- randomness is a seeded SplitMix, so an order is reproducible from its seed
-- (and testable).
module Halogen.Sound.Order
  ( Order
  , newOrder
  , nextTrack
  , upcoming
  )
where

import Data.List ((!!))
import Protolude

data Order a = Order
  { tracks :: [a]
  , queue :: [a]
  -- ^ What is left of this round.
  , seed :: Word64
  , previous :: Maybe a
  }

newOrder :: Word64 -> [a] -> Order a
newOrder seed tracks = Order {tracks, queue = [], seed, previous = Nothing}

-- | The next track, and the order after it. 'Nothing' for an empty album.
nextTrack :: (Eq a) => Order a -> Maybe (a, Order a)
nextTrack order = case order.queue of
  t : rest -> Just (t, order {queue = rest, previous = Just t})
  []
    | null order.tracks -> Nothing
    | otherwise ->
        let (shuffled, seed') = shuffle order.seed order.tracks
         in nextTrack order {queue = avoidRepeat order.previous shuffled, seed = seed'}

-- | The next @n@ tracks, without taking them.
upcoming :: (Eq a) => Int -> Order a -> [a]
upcoming n order
  | n <= 0 = []
  | otherwise = case nextTrack order of
      Nothing -> []
      Just (t, order') -> t : upcoming (n - 1) order'

-- | A new round starting with the track that ended the last one swaps it
-- with its neighbour.
avoidRepeat :: (Eq a) => Maybe a -> [a] -> [a]
avoidRepeat (Just p) (a : b : rest) | a == p = b : a : rest
avoidRepeat _ shuffled = shuffled

shuffle :: Word64 -> [a] -> ([a], Word64)
shuffle seed0 = go seed0 []
  where
    go seed acc [] = (acc, seed)
    go seed acc xs =
      let (r, seed') = splitMix seed
          i = fromIntegral (r `mod` fromIntegral (length xs))
       in go seed' ((xs !! i) : acc) (take i xs <> drop (i + 1) xs)

-- | SplitMix64: the next number and the next seed.
splitMix :: Word64 -> (Word64, Word64)
splitMix s =
  let s' = s + 0x9e3779b97f4a7c15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
   in (z2 `xor` (z2 `shiftR` 31), s')
