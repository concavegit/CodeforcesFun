module Main (main) where

import Control.Monad (replicateM_)
import Data.Bool (bool)

logic :: (Ord a, Foldable f) => f a -> a -> Bool
logic as x = x >= minimum as && x <= maximum as

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          *> (logic . map read . words <$> getLine) <*> (readLn :: IO Int)
          >>= putStrLn . bool "NO" "YES" 
      )