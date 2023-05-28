module Hascript.VDom.Utils where

import Protolude

diffWithIxE :: Monad m => [b] -> [c] -> (Int -> b -> c -> m (Maybe d)) -> (Int -> b -> m (Maybe d)) -> (Int -> c -> m (Maybe d)) -> m [d]
diffWithIxE u v onThese onThis onThat = reverse . catMaybes <$> go 0 u v []
  where
    go _ []     [] acc = pure acc

    go i (x:xs) [] acc = do
      void $ onThis i x
      go (i+1) xs [] acc

    go i [] (y:ys) acc = do
      val <- onThat i y
      go (i+1) [] ys (val:acc)

    go i (x:xs) (y:ys) acc = do
      val <- onThese i x y
      go (i+1) xs ys (val:acc)