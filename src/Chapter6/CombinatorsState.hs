{-# LANGUAGE TypeSynonymInstances, LiberalTypeSynonyms #-}

module Chapter6.CombinatorsState where

import Chapter6.Vector

--import Data.Default
import Data.List
import qualified Data.Map as M

type State s a = s -> (a, s)

--thenDo :: (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)
--thenDo f g s = let (resultOfF, stateAfterF) = f s
--                in g resultOfF stateAfterF

thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g = uncurry g . f
-- We know function f returns a tuple (a,s). Function g takes two
-- arguments, a and s. If we uncurry g, then it will take one
-- argument, (a,s). Since uncurry g can take the return value of
-- function f, we can compose them.

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)
-- How to read this function?
-- It's using point-free style. It takes one argument, let's call it m, which is a map.
-- Example m: [("a", [(1,1), (2,2)]), ("b", [(3,3),(4,4)])]
-- Application steps:
-- 1. fmap (centroid . map toVector) m
-- 2. fmap loops through all the values of m, first value is [(1,1), (2,2)]
-- 3. (centroid . map toVector) [(1,1), (2,2)]
-- 4. map will loop through each tuple, first value is (1,1)
-- 5. toVector (1,1), then toVector (2,2)
-- 6. map toVector will return another list
-- 7. centroid list = will call apply the list returned in step 6 to the function centroid
-- 8. fmap then contines through all the values of m
-- 9. After step 9, m will look like [("a", (1,2)), ("b", (3,4))]
-- 10. M.elems [("a", (1,2)), ("b", (3,4))] will return [(1,2), (3,4)]

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
   in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                     (distance y $ toVector p))
                                                    centrs
                      in M.adjust (p:) chosenCentroid m)
            initialMap points

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points =
  (\s -> (centroids s,s))                                  `thenDo` (\prevCentrs  ->
  (\s -> (clusterAssignments prevCentrs points, s))        `thenDo` (\assignments ->
  (\s -> (newCentroids assignments, s))                    `thenDo` (\newCentrs   ->
  (\s -> ((), s { centroids = newCentrs }))                `thenDo` (\_           ->
  (\s -> ((), s { steps = steps s + 1 }))                  `thenDo` (\_           ->
  (\s -> (threshold s, s))                                 `thenDo` (\t           ->
  (\s -> (sum $ zipWith distance prevCentrs newCentrs, s)) `thenDo` (\err  ->
  if err < t then (\s -> (newCentrs, s)) else (kMeans' points) )))))))

initialState :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initialState i k pts t = KMeansState (i k pts) t 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i k pts t = fst $ kMeans' pts (initialState i k pts t)

-- Rewritting using refined combinators

remain :: a -> (s -> (a,s))
remain x = \s -> (x,s)

access :: (s -> a) -> (s -> (a,s))
access f = \s -> (f s, s)

modify :: (s -> s) -> (s -> ((), s))
modify f = \s -> ((), f s)

kMeans2 :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans2 points =
  access centroids                                     `thenDo` (\prevCentrs  ->
  remain (clusterAssignments prevCentrs points)        `thenDo` (\assignments ->
  remain (newCentroids assignments)                    `thenDo` (\newCentrs   ->
  modify (\s -> s { centroids = newCentrs })           `thenDo` (\_           ->
  modify (\s -> s { steps = steps s + 1 })             `thenDo` (\_           ->
  access threshold                                     `thenDo` (\t           ->
  remain (sum $ zipWith distance prevCentrs newCentrs) `thenDo` (\err         ->
  if err < t then remain newCentrs else kMeans2 points )))))))





