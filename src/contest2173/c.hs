import Control.Applicative (Alternative, empty)
import Control.Monad (replicateM_)
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set

checkMinElem :: (Num a, Ord a, Alternative f) => a -> Set a -> Set a -> f (a, Set a)
checkMinElem k bs remainingBs
  | allMultiplesPresent = pure (minElem, remaining Set.\\ minElemMultiples)
  | otherwise = empty
  where
    (minElem, remaining) = Set.deleteFindMin remainingBs
    minElemMultiples = Set.fromDistinctAscList . takeWhile (<= k) . iterate (+ minElem) $ 2 * minElem
    allMultiplesPresent = Set.isSubsetOf minElemMultiples bs

constructMinSet :: (Num a, Ord a, Alternative f) => a -> Set a -> f (Set a)
constructMinSet k bs = constructMinSet' k bs bs

constructMinSet' :: (Num a, Ord a, Alternative f) => a -> Set a -> Set a -> f (Set a)
constructMinSet' k bs remainingBs
  | null remainingBs = pure Set.empty
  | Just (minElem, remainingBs') <- checkMinElem k bs remainingBs = Set.insert minElem <$> constructMinSet' k bs remainingBs'
  | otherwise = empty

showJustMinSet :: (Foldable f, Show n) => f n -> String
showJustMinSet = (<>) . (<> "\n") . show . length <*> unwords . map show . toList

showMinSet :: (Foldable f, Foldable g, Show n) => f (g n) -> String
showMinSet = foldr (const . showJustMinSet) "-1"

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( (. Set.fromList . fmap read . words) . constructMinSet . read . (!! 1) . words <$> getLine <*> getLine
          >>= putStrLn . (showMinSet :: Maybe (Set Int) -> String)
      )