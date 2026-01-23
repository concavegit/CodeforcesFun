{-# LANGUAGE TupleSections #-}

import Control.Arrow (first)
import Control.Monad (replicateM_)
import Data.Bool (bool)

getSequence :: (Ord a) => a -> [a] -> [a] -> [a]
getSequence z zeroIndices oneIndices =
  takeWhile (< z) oneIndices
    <> dropWhile (< z) zeroIndices

getIndicesAccum ::
  (Num n, Applicative f, Semigroup (f n)) =>
  Bool ->
  ((n, n), (f n, f n)) ->
  ((n, n), (f n, f n))
getIndicesAccum b ((i, z), (falseIndices, trueIndices)) =
  ( (i - 1, z + bool 1 0 b),
    ( bool (pure i <> falseIndices) falseIndices b,
      bool trueIndices (pure i <> trueIndices) b
    )
  )

getIndices ::
  (Foldable t, Applicative u, Num n, Monoid (u n)) =>
  n ->
  t Bool ->
  (n, (u n, u n))
getIndices =
  (first snd .)
    . foldr getIndicesAccum
    . (,(mempty, mempty))
    . (,0)
    . subtract 1

solveAlice :: (Num n, Ord n, Foldable t) => n -> t Bool -> [n]
solveAlice = ((uncurry ($) . first (uncurry . getSequence)) .) . getIndices

showSolution :: (Show n, Num n) => [n] -> String
showSolution [] = "Bob"
showSolution xs =
  "Alice\n"
    <> show (length xs)
    <> "\n"
    <> unwords (fmap (show . (+ 1)) xs)

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( (. fmap (== '1')) . solveAlice <$> readLn <*> getLine
          >>= putStrLn . showSolution
      )