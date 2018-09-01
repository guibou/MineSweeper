module Utils where

import qualified Data.Set as Set
import Data.Set (Set)
import System.Random.Shuffle

-- | 'count p l' counts the number of item in 'l' matching the predicate 'p'
count :: (t -> Bool) -> [t] -> Int
count p l = length (filter p l)

-- | Returns a random subset of n elements
randomPickN :: (Ord t) => Set t -> Int -> IO (Set t)
randomPickN population n = Set.fromList . take n <$> shuffleM (Set.toList population)
