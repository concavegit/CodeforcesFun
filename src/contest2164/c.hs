module Main (main) where

import Control.Monad (guard, replicateM_)
import qualified Data.ByteString.Char8 as C
import Data.List (partition, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Numeric.Natural (Natural)
import Data.Bool (bool)
import Control.Applicative (empty)

removeSword :: (Ord a) => a -> Map a Natural -> Map a Natural
removeSword = Map.update $ bool empty . pure . subtract 1 <*> (> 1)

addSword :: (Ord a) => a -> Map a Natural -> Map a Natural
addSword = flip (Map.insertWith (+)) 1

killMonstersZeroSword' :: (Ord a, Num c) => a -> (c, Map a Natural) -> (c, Map a Natural)
killMonstersZeroSword' monsterHp (currentKills, currentSwords) =
  maybe
    (currentKills, currentSwords)
    ((,) (currentKills + 1) . flip removeSword currentSwords . fst)
    $ Map.lookupGE monsterHp currentSwords

killMonstersZeroSword :: (Ord a, Num c, Foldable f) => Map a Natural -> f a -> c
killMonstersZeroSword = (fst .) . foldr killMonstersZeroSword' . (,) 0

killNonZeroMonsters' :: (Ord a, Num c) => (a, a) -> (c, Map a Natural) -> (c, Map a Natural)
killNonZeroMonsters' (monsterHp, monsterSword) (numKilled, currentSwords) =
  maybe
    (numKilled, currentSwords)
    ( (,)
        (numKilled + 1)
        . (removeSword <*> flip addSword currentSwords . max monsterSword)
        . fst
    )
    $ Map.lookupGE monsterHp currentSwords

killNonZeroMonsters :: (Ord a, Num c, Foldable f) => Map a Natural -> f (a, a) -> (c, Map a Natural)
killNonZeroMonsters = foldr killNonZeroMonsters' . (,) 0

killMonsters :: (Ord a, Num a, Num c) => Map a Natural -> [(a, a)] -> c
killMonsters swords monsters = numKilled + zeroMonstersKilled
  where
    (nonZeroMonsters, monstersZeroSword) = partition ((/= 0) . snd) monsters
    (numKilled, remainingSwords) = killNonZeroMonsters swords $ sortOn Down nonZeroMonsters
    zeroMonstersKilled = killMonstersZeroSword remainingSwords $ fst <$> monstersZeroSword

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    as <- (fst . fromJust . C.readInt <$>) . C.words <$> C.getLine
    bs <- (fst . fromJust . C.readInt <$>) . C.words <$> C.getLine
    cs <- (fst . fromJust . C.readInt <$>) . C.words <$> C.getLine
    print
      . killMonsters (Map.fromListWith (+) $ flip (,) 1 <$> as)
      $ zip bs cs