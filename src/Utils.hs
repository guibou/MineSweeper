-- | A few independent functions
module Utils where

import qualified Data.Set as Set
import Data.Set (Set)
import System.Random.Shuffle
import System.Random

-- | 'count p l' counts the number of item in 'l' matching the predicate 'p'
count :: (t -> Bool) -> [t] -> Int
count p l = length (filter p l)

-- | Returns a random subset of n elements
randomPickN
  :: (RandomGen gen, Ord t)
  => Set t -- ^ Initial population
  -> Int -- ^ size of the new population
  -> gen -- ^ Random number generator
  -> Set t -- ^ Output population
randomPickN population n gen = Set.fromList . take n $ shuffle' l (length l) gen
  where l = Set.toList population
