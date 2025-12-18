import Control.Monad (replicateM_)

incrementMaxSleep :: (Num k, Eq k, Num n) => k -> (k, n) -> Bool -> (k, n)
incrementMaxSleep k (_, y) True = (k, y)
incrementMaxSleep _ (0, y) False = (0, y + 1)
incrementMaxSleep _ (x, y) False = (x - 1, y)

maxSleeps :: (Foldable f, Num k, Eq k, Num n) => k -> f Bool -> n
maxSleeps = (snd .) . flip foldl (0, 0) . incrementMaxSleep

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          >>= (<$> getLine) . (. fmap (== '1')) . maxSleeps . (!! 1) . fmap read . words
          >>= print
      )