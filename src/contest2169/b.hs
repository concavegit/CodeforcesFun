module Main (main) where

import Control.Applicative (Alternative, empty)
import Control.Monad (replicateM_)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)

canInfinite' :: Char -> Either Bool Char -> Either Bool Char
canInfinite' a (Left False) = Right a
canInfinite' _ (Left True) = Left True
canInfinite' a (Right b)
  | a == '>' && b /= '>' || a == '*' && b /= '>' = Left True
  | otherwise = Right a

canInfinite :: (Foldable f) => f Char -> Bool
canInfinite = (== Left True) . foldr canInfinite' (Left False)

maximumLeft' :: (Num a) => Char -> a -> a
maximumLeft' = bool (const 0) (+ 1) . ((||) . (== '<') <*> (== '*'))

maximumLeft :: (Foldable f, Num a) => f Char -> a
maximumLeft = foldr maximumLeft' 0

maximumRight' :: (Num a) => Char -> Either a a -> Either a a
maximumRight' x acc
  | x == '>' || x == '*' = (+ 1) <$> acc
  | otherwise = acc >>= Left

maximumRight :: (Foldable f, Num a) => f Char -> a
maximumRight = either id id . foldr maximumRight' (Right 0)

logic :: (Foldable f, Num a, Ord a, Alternative g) => f Char -> g a
logic xs
  | canInfinite xs = empty
  | length xs == 1 = pure 1
  | otherwise = pure . max (maximumLeft xs) $ maximumRight xs

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= print . fromMaybe (-1) . logic)