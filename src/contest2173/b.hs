import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

accumFindMaxScore :: (Num n, Ord n) => (n, n) -> (n, n) -> (n, n)
accumFindMaxScore (a, b) (c, d) = (min (minAB - c) (d - maxAB), max (maxAB - c) (d - minAB))
  where
    maxAB = max a b
    minAB = min a b

findMaxScore :: (Foldable f, Num n, Ord n) => f (n, n) -> n
findMaxScore = snd . foldl accumFindMaxScore (0, 0)

main :: IO ()
main =
  C.getLine
    >>= flip
      replicateM_
      ( C.getLine
          *> ( (. fmap (fst . fromJust . C.readInt) . C.words)
                 . zip
                 . fmap (fst . fromJust . C.readInt)
                 . C.words
                 <$> C.getLine
                 <*> C.getLine
                 >>= print . findMaxScore
             )
      )
      . fst
      . fromJust
      . C.readInt