module Chapter6.KMeans where

import Chapter6.Vector

import Data.List
import qualified Data.Map as M

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] => M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                     (distance y $ toVector p))
                                                     centroids
                      in M.adjust (p:) chosenCentroid m)
            initialMap points

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)