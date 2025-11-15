module Main (main) where

import Control.Arrow (Arrow ((***)))
import Control.Monad (replicateM_)
import Data.Foldable (Foldable (toList), find)

generatePairs :: [a] -> [(a, a)]
generatePairs [] = mempty
generatePairs (x : xs) = ((,) x <$> xs) <> generatePairs xs

logic :: (Foldable m, Integral a) => m a -> Maybe (a, a)
logic x = find (even . uncurry (flip mod)) $ generatePairs (toList x)

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          *> ( maybe
                 "-1"
                 (uncurry ((. (" " <>)) . (<>)) . (show *** show))
                 . logic
                 . fmap read
                 . words
                 <$> getLine
             )
          >>= putStrLn
      )