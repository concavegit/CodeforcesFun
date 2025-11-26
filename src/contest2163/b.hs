import Control.Arrow ((***))
import Control.Monad (replicateM_)
import Data.Foldable (forM_)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

logic :: (Integral n) => n -> [n] -> [Bool] -> Maybe (Int, [(Int, Int)])
logic n ps xs
  | (x : _) <- xs, x || last xs || xs !! indexN || xs !! index1 = Nothing
  | otherwise =
      Just
        ( 5,
          [ ( min index1 indexN + 1,
              max index1 indexN + 1
            ),
            (1, indexN + 1),
            (index1 + 1, fromIntegral n),
            (1, index1 + 1),
            (indexN + 1, fromIntegral n)
          ]
        )
  where
    indexN = fromJust $ elemIndex n ps
    index1 = fromJust $ elemIndex 1 ps

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    n <- readLn
    ps <- map read . words <$> getLine
    xs <- map (== '1') <$> getLine
    case logic n ps xs of
      Nothing -> putStrLn "-1"
      Just (m, moves) -> do
        print m
        forM_ moves $ putStrLn . uncurry ((<>) . (<> " ")) . (show *** show)