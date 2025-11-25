import Control.Monad (replicateM_)
import Data.Bool (bool)
import Data.List (sort)

checkAdj :: (Eq a) => [a] -> Bool
checkAdj (x : y : xs) = x == y && checkAdj xs
checkAdj _ = True

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          *> getLine
          >>= putStrLn . bool "NO" "YES" . checkAdj . tail . sort . map (read :: String -> Int) . words
      )