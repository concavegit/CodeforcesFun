import Control.Monad (replicateM_)

logic' :: (Num k, Eq k, Num n) => k -> (k, n) -> Bool -> (k, n)
logic' k (_, y) True = (k, y)
logic' _ (0, y) False = (0, y + 1)
logic' _ (x, y) False = (x - 1, y)

logic :: (Foldable f, Num k, Eq k, Num n) => k -> f Bool -> n
logic k = snd . foldl (logic' k) (0, 0)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ : k : _ <- map read . words <$> getLine
    s <- map (== '1') <$> getLine
    print $ logic k s