module Halogen.VDom.Utils where

import Data.Map.Strict qualified as M
import HPrelude

{-# SPECIALIZE diffWithIxE ::
  [a]
  -> [b]
  -> (Int -> a -> b -> IO (Maybe c))
  -> (Int -> a -> IO (Maybe c))
  -> (Int -> b -> IO (Maybe c))
  -> IO [c]
  #-}
diffWithIxE :: (Monad m) => [b] -> [c] -> (Int -> b -> c -> m (Maybe d)) -> (Int -> b -> m (Maybe d)) -> (Int -> c -> m (Maybe d)) -> m [d]
diffWithIxE u v onThese onThis onThat = reverse . catMaybes <$> go 0 u v []
  where
    go _ [] [] acc = pure acc
    go i (x : xs) [] acc = do
      void $ onThis i x
      go (i + 1) xs [] acc
    go i [] (y : ys) acc = do
      val <- onThat i y
      go (i + 1) [] ys (val : acc)
    go i (x : xs) (y : ys) acc = do
      val <- onThese i x y
      go (i + 1) xs ys (val : acc)

{-# SPECIALIZE diffWithKeyAndIxE ::
  Map Text a
  -> [b]
  -> (b -> Text)
  -> (Text -> Int -> a -> b -> IO c)
  -> (Text -> a -> IO d)
  -> (Text -> Int -> b -> IO c)
  -> IO (Map Text c)
  #-}
diffWithKeyAndIxE
  :: (Monad m)
  => Map Text a
  -> [b]
  -> (b -> Text)
  -> (Text -> Int -> a -> b -> m c)
  -> (Text -> a -> m d)
  -> (Text -> Int -> b -> m c)
  -> m (Map Text c)
diffWithKeyAndIxE o1 as fk f1 f2 f3 = do
  o2 <- foldM go M.empty (effective fk as)
  traverse_ (uncurry f2) (M.toAscList (M.difference o1 o2))
  pure o2
  where
    go acc (i, a) = do
      let k = fk a
      val <- case M.lookup k o1 of
        Just v -> f1 k i v a
        Nothing -> f3 k i a
      pure $ M.insert k val acc

{-# SPECIALIZE strMapWithIxE ::
  [a]
  -> (a -> Text)
  -> (Text -> Int -> a -> IO b)
  -> IO (Map Text b)
  #-}
strMapWithIxE :: (Monad m) => [a] -> (a -> Text) -> (Text -> Int -> a -> m b) -> m (Map Text b)
strMapWithIxE as f g = foldM go M.empty (effective f as)
  where
    go acc (i, a) = do
      let k = f a
      val <- g k i a
      pure $ M.insert k val acc

-- | The entries that actually take effect: at most one per key, the last of
-- any duplicates, renumbered over the survivors.
--
-- Both functions above apply an effect per entry but record one value per
-- key, so a duplicate key would apply two effects and remember one - and not
-- the one whose effect ran last, which is how a duplicate could survive a
-- build and a patch looking right and then settle on the wrong value on the
-- patch after that. Dropping the shadowed entries before anything runs is
-- what makes "the last one wins" true of the effects and not just of the
-- map.
--
-- The renumbering matters for children, where the index is the position to
-- insert at: two children under one key are one child, and the ones after
-- them have to close up.
effective :: (a -> Text) -> [a] -> [(Int, a)]
effective f as = zip [0 ..] [a | (i, a) <- indexed, M.lookup (f a) lastIndex == Just i]
  where
    indexed = zip [0 :: Int ..] as
    -- fromList keeps the last binding for a repeated key.
    lastIndex = M.fromList [(f a, i) | (i, a) <- indexed]
