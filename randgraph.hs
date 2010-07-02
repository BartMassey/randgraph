-- Copyright © 2010 Bart Massey
-- This code is released under the "MIT License".
-- Please see the file COPYING in this distribution
-- for license information.

import Data.List
import System.Environment
import System.Random

import Shuffle

mirror :: [(a, a)] -> [(a, a)]
mirror [] = []
mirror (et@(e1, e2) : es) = et : (e2, e1) : mirror es

pair :: [a] -> [(a, a)]
pair [] = []
pair [_] = []
pair (e1 : e2 : es) =
    (e1, e2) : pair (e2 : es)

randPath :: (Integral a, RandomGen g) => a -> g -> ([(a, a)], g)
randPath n gn =
    (mirror $ pair l, gn')
    where
      (l, gn') = shuffleList [0..n-1] gn

filterR :: RandomGen g => (a -> g -> (Bool, g)) -> g -> [a] -> ([a], g)
filterR _ gn [] = ([], gn)
filterR f gn (e : es) =
    case f e gn of
      (True, gn') ->
          let (es', gn'') = filterR f gn' es in
          (e : es, gn'')
      (False, gn') ->
          filterR f gn' es

randEdges :: (Integral a, RealFrac b, Random b, RandomGen g) =>
             a -> b -> g -> ([(a, a)], g)
randEdges n p gn =
    (mirror l, gn')
    where
      (l, gn') =
          filterR wRand gn nClique
          where
            nClique = [ (v1, v2) | v1 <- [0 .. n - 1],
                                   v2 <- [v1 + 1 .. n - 1] ]
            wRand e gn'' =
                (p > p0, gn''')
                where
                  (p0, gn''') = random gn''
  
uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

randGraph :: (Integral a, RealFrac b, Random b, RandomGen g) =>
             a -> b -> g -> [[a]]
randGraph n p gn =
    map (\k -> map snd $ filter ((== k) . fst) edges) [0..n-1]
    where
      (rp, gn') = randPath n gn
      (re, _) = randEdges n p gn'
      edges = uniq (rp ++ re)

main :: IO ()
main = do
  [size, connectivity] <- getArgs
  gn <- getStdGen
  let g = randGraph (read size) ((read connectivity) :: Double) gn 
  putStr $ unlines $ map (unwords . map show) g
