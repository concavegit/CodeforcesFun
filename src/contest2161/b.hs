{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (forM, forM_)
import Data.List (elemIndices)
import Data.Set (Set)
import qualified Data.Set as Set

isSquare :: Set (Int, Int) -> Bool
isSquare xs
  | length xs /= 4 = False
  | otherwise =
      ((-) <$> maximum <*> minimum) (Set.map fst xs) == 1
        && (subtract <$> minimum <*> maximum) (Set.map snd xs) == 1

areLegalIntercepts :: (Foldable f) => f Int -> Bool
areLegalIntercepts s
  | length s == 2 = abs (foldr1 (-) s) == 1
  | otherwise = length s == 1

isAscendingZigZag :: Set (Int, Int) -> Bool
isAscendingZigZag = areLegalIntercepts . Set.map (uncurry subtract)

isDescendingZigZag :: Set (Int, Int) -> Bool
isDescendingZigZag = areLegalIntercepts . Set.map (uncurry (+))

isLegal :: Set (Int, Int) -> Bool
isLegal b = null b || isSquare b || isAscendingZigZag b || isDescendingZigZag b

main :: IO ()
main = do
  t <- readLn
  forM_ [1 .. t] $ \_ -> do
    n <- readLn
    points <- forM [0 .. n - 1] $ \i ->
      Set.fromList . map (i,) . elemIndices '#' <$> getLine
    putStrLn $ if isLegal (Set.unions points) then "YES" else "NO"