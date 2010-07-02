--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

--- Knuth shuffle for various Haskell data structures

module Shuffle (shuffleList, shuffleIArray, shuffleMArray)
where

import Control.Monad.ST.Lazy
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.ST
import Data.Ix
import Data.List
import Data.Map as Map
import System.Random

class (Random i, Ix i) => ShuffleIx i

instance ShuffleIx Int

shuffleList :: RandomGen g => [e] -> g -> ([e], g)
shuffleList es g = runST $ do
  a <- nla es 
  g' <- shuffleMArray a g
  es' <- getElems a
  return (es', g')
  where
    nla :: [e] -> ST s (STArray s Int e)
    nla = newListArray (1, length es)

shuffleIArray :: (IArray a e, ShuffleIx i, RandomGen g)
              => a i e -> g -> (a i e, g)
shuffleIArray a g = runST $ do
  ma <- ta a
  g' <- shuffleMArray ma g
  a' <- freeze ma
  return (a', g')
  where
    ta :: (IArray a e, ShuffleIx i)
       => a i e -> ST s (STArray s i e)
    ta = thaw

shuffleMArray :: (MArray a e m, ShuffleIx i, RandomGen g)
              => a i e -> g -> m g
shuffleMArray a g = do
  (lb, ub) <- getBounds a
  g' <- shuffle_elems g (range (lb, ub)) ub a
  return g'
  where
    shuffle_elems g [] _ a = return g
    shuffle_elems g [_] _ a = return g
    shuffle_elems g (lb : lbs) ub a = do
      let (r, g') = randomR (lb, ub) g
      array_exchange a lb r
      shuffle_elems g' lbs ub a
      where
        array_exchange a i1 i2 = do
          e1 <- readArray a i1
          e2 <- readArray a i2
          writeArray a i2 e1
          writeArray a i1 e2
