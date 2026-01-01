import Control.Monad (replicateM_)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

sumFromLeft'' :: (Num a) => (a, Bool) -> a -> (a, Bool)
sumFromLeft'' (currentSum, isNotFirst) a = (currentSum + bool a (abs a) isNotFirst, True)

sumFromLeft' :: (Eq b, Semigroup (f a), Applicative f, Num b, Num a) => b -> ((f a, b), (a, Bool)) -> a -> ((f a, b), (a, Bool))
sumFromLeft' numElems ((previousSums, counter), (latestSum, isNotFirst)) a
  | counter == numElems - 1 = ((previousSums, counter), (latestSum, isNotFirst))
  | otherwise = ((previousSums <> pure (fst newAns), counter + 1), newAns)
  where
    newAns = sumFromLeft'' (latestSum, isNotFirst) a

sumFromLeft :: (Foldable t, Eq b, Semigroup (f a), Applicative f, Num b, Num a) => b -> t a -> f a
sumFromLeft numElems = fst . fst . foldl (sumFromLeft' numElems) ((pure 0, 0), (0, False))

sumFromRight' :: (Eq b, Semigroup (f a), Applicative f, Num b, Num a) => b -> a -> (f a, (a, b)) -> (f a, (a, b))
sumFromRight' numElems a (previousSums, (latestSum, counter))
  | counter == numElems - 1 = (previousSums, (latestSum, counter))
  | otherwise = (pure newAns <> previousSums, (newAns, counter + 1))
  where
    newAns = latestSum - a

sumFromRight :: (Foldable t, Eq b, Semigroup (f a), Applicative f, Num b, Num a) => b -> t a -> f a
sumFromRight numElems = fst . foldr (sumFromRight' numElems) (pure 0, (0, 0))

maxSum :: (Ord a, Foldable t, Num a, Num b, Eq b) => b -> t a -> a
maxSum numElems xs = maximum $ Seq.zipWith (+) (sumFromLeft numElems xs) (sumFromRight numElems xs)

main :: IO ()
main = do
  t <- fst . fromJust . C.readInt <$> C.getLine
  replicateM_ t $ do
    n <- fst . fromJust . C.readInt <$> C.getLine
    ns <- fmap (fst . fromJust . C.readInt) . C.words <$> C.getLine
    print $ maxSum n ns