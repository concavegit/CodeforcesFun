import Control.Applicative (Alternative, empty)
import Control.Arrow ((&&&), (***))
import Control.Monad (replicateM_)
import Data.List (sort)

solveSortedAccum :: (Integral m, Applicative t, Monoid (t (m, m))) => m -> m -> ((m, m), t (m, m)) -> ((m, m), t (m, m))
solveSortedAccum m a ((i, previous), acc)
  | i == 0 = (nextIndexAndValue, acc)
  | i >= m * 2 = (nextIndexAndValue, pure (a, previous) <> acc)
  | odd i = (nextIndexAndValue, pure (previous, a) <> acc)
  | otherwise = (nextIndexAndValue, acc)
  where
    nextIndexAndValue = (i + 1, a)

solveSorted :: (Integral m, Foldable t, Applicative u, Monoid (u (m, m))) => m -> t m -> u (m, m)
solveSorted = (snd .) . flip foldr ((0, 0), mempty) . solveSortedAccum

handleZeroAccum ::
  (Num m, Ord m, Num n, Applicative t, Monoid (t (n, n)), Foldable u, Applicative u) =>
  (m, n) ->
  ((u (m, n), m), (n, t (n, n))) ->
  ((u (m, n), m), (n, t (n, n)))
handleZeroAccum (value, index) ((target, progress), (previousIndex, acc))
  | null target = ((pure (value, index), progress), (index, acc))
  | foldr (const . (<= progress) . fst) False target = ((target, progress), (index, pure (index, previousIndex) <> acc))
  | otherwise = ((target, progress + value), (index, pure (index, foldr (const . snd) 0 target) <> acc))

handleZero :: (Foldable t, Num m, Ord m, Num n, Alternative u, Applicative v, Monoid (v (n, n))) => t (m, n) -> u (v (n, n))
handleZero as
  | foldr (const . (> progress) . fst) False target = empty
  | otherwise = pure result
  where
    ((target, progress), (_, result)) = foldr handleZeroAccum ((Nothing, 0), (0, mempty)) as

findSequence :: (Integral m, Applicative t, Alternative u, Monoid (t (m, m))) => m -> m -> [m] -> u (t (m, m))
findSequence n m as
  | m > n `div` 2 = empty
  | m == 0 = handleZero (sort (zip as [0 ..]))
  | otherwise = pure . solveSorted m . fmap snd . sort $ zip as [0 ..]

showResult' :: (Num a, Show a, Foldable f) => f (a, a) -> String
showResult' = (<>) . show . length <*> foldr ((<>) . ("\n" <>) . uncurry ((<>) . (<> " ")) . (showIndex *** showIndex)) ""
  where
    showIndex = show . (+ 1)

showResult :: (Foldable f, Num a, Show a) => f [(a, a)] -> String
showResult = foldr (const . showResult') "-1"

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( ( (. map read . words)
            . uncurry findSequence
            . (head &&& (!! 1))
            . map read
            . words
            <$> getLine
            <*> getLine
        )
          >>= putStrLn . (showResult :: Maybe [(Int, Int)] -> String)
      )