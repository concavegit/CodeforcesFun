import Control.Applicative (Alternative, empty)
import Control.Arrow (Arrow (first, second))
import Control.Monad (replicateM_)
import Data.Bool (bool)

assessPoint :: (Num n) => Bool -> Bool -> (Bool, n)
assessPoint False False = (True, 1)
assessPoint _ b = (b, 0)

handleFold :: (Foldable f, Alternative f, Num m) => Bool -> (f Bool, m) -> (f Bool, m)
handleFold b (c, d) = first pure $ foldr (const . second (+ d) . flip assessPoint b) (True, bool 0 1 (not b)) c

foldBools :: (Foldable f, Foldable g, Alternative g, Num n) => f Bool -> (g Bool, n)
foldBools = foldr handleFold (empty, 0)

processFoldedBools :: (Foldable f, Num n) => (f Bool, n) -> n
processFoldedBools = uncurry ((+) . foldr (const . bool 0 1 . not) 0)

logic :: (Foldable f, Num n) => f Bool -> n
logic = processFoldedBools . (foldBools :: (Foldable f, Num n) => f Bool -> (Maybe Bool, n))

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= print . logic . fmap (== 's'))