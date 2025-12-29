import Control.Monad (replicateM_)
import Data.Bool (bool)
import Data.Monoid (Sum (Sum))

moreThanOneYes :: (Foldable t) => t Bool -> Bool
moreThanOneYes = (> 1) . foldMap (Sum . fromEnum)

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= putStrLn . bool "YES" "NO" . moreThanOneYes . fmap (== 'Y'))