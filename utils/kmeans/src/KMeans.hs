-- | The KMeans algorithm.

module KMeans ( Assoc
              , Cluster(..)
              , randomPointsRs
              , kmeans
              ) where

import Geometry

import Graphics.Rendering.OpenGL ( GLfloat )

import Data.Maybe ( fromJust )
import Data.Function ( on )
import Data.List ( find, minimumBy )
import Data.Map ( Map )
import qualified Data.Map as M
import System.Random

-- | A cluster has an id and a center.
data Cluster = Cluster { clusterId :: Int
                       , clusterCenter :: Point
                       }
    deriving (Eq, Show)

-- | An association is a correspondence between a point and its cluster.
type Assoc = Map Point Cluster

-- | Initializes the assoication: 'k' clusters are randomly created
-- and each point is accoiated to a randomly chosen cluster.
initializeKMeans :: RandomGen g => [Point] -> Int -> g -> Assoc
initializeKMeans ps k g = fst $ foldr (\p (m, g') ->
            let (idx, newG) = randomR (0, k - 1) g'
                cluster = Cluster idx (clusters !! idx) in (M.insert p cluster m, newG) ) (M.empty, g) ps
    where rangeX = minmax . map fst $ ps
          rangeY = minmax . map snd $ ps
          clusters = take k $ randomPointsRs rangeX rangeY g

-- | The minimum and the maximum of a list.
minmax :: (Ord a) => [a] -> (a, a)
minmax xs = (minimum xs, maximum xs)

-- | Generates an infinite list of points contained in a box.
randomPointsRs :: RandomGen g => (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> g -> [Point]
randomPointsRs rangeX rangeY g = (x, y) : randomPointsRs rangeX rangeY newG
    where (x, gy) = randomR rangeX g
          (y, newG) = randomR rangeY gy

-- | First step: associate each point to its nearest cluster.
updateAssignments :: Assoc -> Assoc
updateAssignments m = foldr (\p m' ->
                let idx = closestCluster p clusters
                    newCluster = Cluster idx (clusters !! idx) in M.insert p newCluster m') M.empty points
    where points = map fst $ M.toList m
          clusters = map clusterCenter . map snd $ M.toList m
          closestCluster p clusters' = fst . minimumBy (compare `on` snd) . map (\(i, c) -> (i, squaredDist p c)) $ zip [ 0 .. ] clusters'

-- | Second step: replcae each cluster by the centroid of the points attached to it.
updateCenters :: Assoc -> Assoc
updateCenters m = M.fromList $ map ( \(p, Cluster { clusterId = n }) -> (p, findNewCluster n) ) assoc
    where assoc = M.toList m
          clusters = map snd $ M.toList m
          newClusters = map (updateCenters' m) clusters
          findNewCluster n = fromJust $ find (\Cluster { clusterId = n' } -> n == n') newClusters

updateCenters' :: Assoc -> Cluster -> Cluster
updateCenters' m c =
    let (ss, denom) = foldr (\p (s, d) ->
                    if fromJust (M.lookup p m) == c then (s <+> p, d + 1)
                    else (s, d))  (origin, 0) points
    in c { clusterCenter = ss </> denom }
    where points = map fst $ M.toList m
          origin = (0, 0)

-- | One step of the KMeans algorithm.
step :: Assoc -> Assoc
step = updateCenters . updateAssignments

-- | 'itermax' iterations of the KMeans algorithm.
kmeans :: RandomGen g => Int -> Int -> [Point] -> g -> Assoc
kmeans k itermax ps g = (iterate step $ initializeKMeans ps k g) !! itermax

