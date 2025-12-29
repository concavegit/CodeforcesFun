import Control.Monad (replicateM_)
import Data.Bool (bool)
import Data.Monoid (Sum (Sum))

atLeastOne :: (Foldable t) => t Bool -> Bool
atLeastOne = (> 1) . foldMap (Sum . fromEnum)

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= putStrLn . bool "YES" "NO" . atLeastOne . fmap (== 'Y'))