import Control.Monad (replicateM_)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    n <- readLn
    y : r : _ <- map read . words <$> getLine
    print . min n $ div y 2 + r
