import Control.Applicative (empty)
import Control.Monad (replicateM_)
import Data.Foldable (toList)
import Data.Sequence (Seq, index, (|>))

logic' :: (Int, Seq Int) -> Int -> (Int, Seq Int)
logic' (lastNum, answer) x
  | diff > currentIndex = (x, answer |> currentIndex + 1)
  | otherwise = (x, answer |> index answer (currentIndex - diff))
  where
    diff = x - lastNum
    currentIndex = length answer

logic :: (Foldable f) => f Int -> Seq Int
logic = snd . foldl logic' (0, empty)

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          *> getLine
          >>= putStrLn . unwords . map show . toList . logic . map read . words
      )