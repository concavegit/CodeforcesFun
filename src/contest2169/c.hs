module Main (main) where

import Control.Monad (replicateM_)

coreFunction :: (Num n) => n -> n -> n
coreFunction = (-) . ((^ 2) >>= (+))

maxIncrement' :: (Foldable g, Num n, Ord n) => ((n, n), (g n, n)) -> n -> ((n, n), (Maybe n, n))
maxIncrement' ((prevIndex, prevMax), (prevMinPrefix, prevSum)) x =
  ( ( prevIndex + 1,
      max prevMax $
        coreFunction (prevIndex + 1) (prevSum + x) - newMinPrefix
    ),
    (pure newMinPrefix, prevSum + x)
  )
  where
    newMinPrefix = foldr min (coreFunction prevIndex prevSum) prevMinPrefix

maxIncrement :: (Foldable f, Num n, Ord n) => f n -> n
maxIncrement = snd . fst . foldl maxIncrement' ((0, 0), (Nothing, 0))

logic :: (Num n, Ord n, Foldable f) => f n -> n
logic = (+) . maxIncrement <*> sum

main :: IO ()
main =
  readLn >>= flip replicateM_ (getLine *> (map read . words <$> getLine) >>= print . logic)