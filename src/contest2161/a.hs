module Main (main) where

import Control.Monad (replicateM_)
import Numeric.Natural (Natural)

logic :: (Num a, Ord a, Foldable f) => a -> a -> a -> f Bool -> Natural
logic r x d =
  snd
    . foldl
      (\(ri, nr) b -> if b && ri >= x then (ri, nr) else (ri - d, nr + 1))
      (r, 0)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    r : x : d : _ <- map read . words <$> getLine
    l <- map (== '2') <$> getLine
    print $ logic r x d l