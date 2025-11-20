module Main (main) where

import Control.Arrow (Arrow (first, second))
import Control.Monad (replicateM_)

moreGT' :: (Ord a, Num b) => a -> a -> (b, b) -> (b, b)
moreGT' a x
  | x < a = first (+ 1)
  | x > a = second (+ 1)
  | otherwise = id

moreGT :: (Foldable f, Ord a) => a -> f a -> Bool
moreGT a = uncurry (<) . foldr (moreGT' a) (0, 0)

logic :: (Ord a, Num a, Foldable f) => a -> f a -> a
logic n xs
  | moreGT n xs = n + 1
  | otherwise = n - 1

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      (logic . (!! 1) . map read . words <$> getLine <*> (map read . words <$> getLine) >>= print)