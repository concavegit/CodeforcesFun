import Control.Applicative (empty)
import Control.Monad (replicateM_)
import Data.Foldable (toList)
import Data.Sequence (Seq, index, (|>))

logic' :: ((Int, Int), Seq Int) -> Int -> ((Int, Int), Seq Int)
logic' ((currentIndex, lastNum), ys) x
  | diff > currentIndex = ((currentIndex + 1, x), ys |> currentIndex + 1)
  | otherwise = ((currentIndex + 1, x), ys |> index ys (length ys - diff))
  where
    diff = x - lastNum

logic :: (Foldable f) => f Int -> Seq Int
logic = snd . foldl logic' ((0, 0), empty)

main :: IO ()
main =
  readLn
    >>= flip
      replicateM_
      ( getLine
          *> getLine
          >>= putStrLn . unwords . map show . toList . logic . map read . words
      )