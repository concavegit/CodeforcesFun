import Control.Monad (replicateM_)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

logic :: IntSet -> Int
logic = IntSet.findMin . (IntSet.fromList . enumFromTo 0 . (+ 1) . IntSet.size >>= IntSet.difference)

main :: IO ()
main = readLn >>= flip replicateM_ (getLine *> getLine >>= print . logic . IntSet.fromList . map read . words)