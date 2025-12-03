import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import Data.Maybe (fromJust)

processMinMaxs :: (Num a, Foldable f) => a -> f (a, a) -> a
processMinMaxs n =
  snd
    . foldl
      ( \(lastL, currentCount) (newL, newR) ->
          (newL, currentCount + (newL - lastL) * (2 * n + 1 - newR))
      )
      (0, 0)

combineMinMaxTuples :: (Ord a) => (a, a) -> (a, a) -> (a, a)
combineMinMaxTuples (a, b) (c, d) = (min a c, max b d)

findMinMaxs :: (Ord a) => [a] -> [a] -> [(a, a)]
findMinMaxs topRow bottomRow = zipWith combineMinMaxTuples topTuples bottomTuples
  where
    topTuples = zip (scanl1 min topRow) (scanl1 max topRow)
    bottomTuples = zip (scanr1 min bottomRow) (scanr1 max bottomRow)

findSolution :: (Ord n, Num n) => n -> [n] -> [n] -> n
findSolution n topRow bottomRow = processMinMaxs n truncatedMinMaxs
  where
    minMaxs = findMinMaxs topRow bottomRow
    sortedMinMaxs = sort minMaxs
    truncatedMaxs = scanr min (2 * n) (snd <$> sortedMinMaxs)
    truncatedMinMaxs = zip (fst <$> sortedMinMaxs) truncatedMaxs

main :: IO ()
main = do
  t <- fst . fromJust . C.readInt <$> C.getLine
  replicateM_ t $ do
    n <- fst . fromJust . C.readInt <$> C.getLine
    topRow <- (fst . fromJust . C.readInt <$>) . C.words <$> C.getLine
    bottomRow <- (fst . fromJust . C.readInt <$>) . C.words <$> C.getLine
    print $ findSolution n topRow bottomRow