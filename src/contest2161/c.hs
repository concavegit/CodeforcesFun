module Main (main) where

import Control.Arrow (first)
import Control.Monad (replicateM_)
import Data.List (sort)
import Data.Sequence (Seq)
import Data.Foldable (toList)

takeUntil :: (b -> Bool) -> (a -> b -> b) -> [a] -> b -> (([a], [a]), b)
takeUntil c f (x : xs) acc
  | c acc = (([], x : xs), acc)
  | c newAcc = (([x], xs), newAcc)
  | otherwise = first (first (x :)) $ takeUntil c f xs newAcc
  where
    newAcc = f x acc
takeUntil _ _ [] acc = (([], []), acc)

generateSequence' :: (Num a, Ord a, Applicative g, Monoid (g a)) => a -> a -> a -> g a -> [a] -> (a, (g a, [a]))
generateSequence' threshold largeNum currentSum accumulatedSequence remainingSequence =
    (sumWithoutLarge + largeNum - threshold, (accumulatedSequence <> foldMap pure numsToTake <> pure largeNum, remainingNums))
    where ((numsToTake, remainingNums), sumWithoutLarge) = takeUntil (>= threshold - largeNum) (+) remainingSequence currentSum

generateSequence :: (Num a, Ord a, Foldable f, Applicative g, Monoid (g a)) => a -> f a -> [a] -> g a
generateSequence x largeNums smallNums =
  uncurry ((. foldMap pure) . (<>)) . snd $
    foldr
      ( \largeNum (currentSum, (accumulatedSequence, remainingSequence)) ->
          generateSequence' x largeNum currentSum accumulatedSequence remainingSequence
      )
      (0, (mempty, smallNums))
      largeNums

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    n : x : _ <- map read . words <$> getLine
    as <- sort . map read . words <$> getLine
    let (smallNums, largeNums) = splitAt (n - sum as `div` x) as
    print $ sum largeNums
    putStrLn . unwords . fmap show . toList $ (generateSequence x largeNums smallNums :: Seq Int)