import Control.Monad (replicateM_)

accumFindMaxScore :: (Num n, Ord n) => (n, n) -> (n, n) -> (n, n)
accumFindMaxScore (a, b) (c, d) = (min (a - c) (d - b), max (b - c) (d - a))

findMaxScore :: (Foldable f, Num n, Ord n) => f (n, n) -> n
findMaxScore = snd . foldl accumFindMaxScore (0, 0)

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          *> ( (. fmap read . words) . zip . fmap read . words
                 <$> getLine
                 <*> getLine
             )
          >>= print . findMaxScore
      )