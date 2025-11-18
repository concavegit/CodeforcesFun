module Main (main) where

import Control.Monad (replicateM_)
import Data.List (sort, sortOn)

killMonsters :: (Ord a, Num b) => a -> [(a, a)] -> b
killMonsters _ [] = 0
killMonsters power ((monsterHp, monsterSword) : rest)
  | power >= monsterHp = 1 + killMonsters (max power monsterSword) rest
  | otherwise = killMonsters power rest

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    as <- sort . map read . words <$> getLine
    bs <- map read . words <$> getLine
    cs <- (map read . words <$> getLine) :: IO [Int]
    print $ killMonsters (maximum as) (sortOn fst (zip bs cs))