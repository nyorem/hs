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

data Cluster = Cluster { clusterId :: Int
                       , clusterCenter :: Point
                       }
    deriving (Eq, Show)

type Assoc = Map Point Cluster

initializeKMeans :: RandomGen g => [Point] -> Int -> g -> Assoc
initializeKMeans ps k g = fst $ foldr (\p (m, g') ->
            let (idx, newG) = randomR (0, k - 1) g'
                cluster = Cluster { clusterId = idx , clusterCenter = clusters !! idx } in
            (M.insert p cluster m, newG) ) (M.empty, g) ps
    where x = map fst ps
          y = map snd ps
          (xmin, xmax) = (minimum x, maximum x)
          (ymin, ymax) = (minimum y, maximum y)
          clusters = take k $ randomPointsRs (xmin, xmax) (ymin, ymax) g

randomPointsRs :: RandomGen g => (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> g -> [Point]
randomPointsRs (xmin, xmax) (ymin, ymax) g = (x, y) : randomPointsRs (xmin, xmax) (ymin, ymax) newG
    where (x, gy) = randomR (xmin, xmax) g
          (y, newG) = randomR (ymin, ymax) gy

updateAssignments :: Assoc -> Assoc
updateAssignments m = foldr (\p m' ->
                let idx = closestCluster p clusters
                    newCluster = Cluster { clusterId = idx, clusterCenter = (clusters !! idx) } in
                M.insert p newCluster m') M.empty points
    where points = map fst $ M.toList m
          clusters = map clusterCenter . map snd $ M.toList m
          closestCluster p clusters' = fst . minimumBy (compare `on` snd) . map (\(i, c) -> (i, squaredDist p c)) $ zip [ 0 .. ] clusters'

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

step :: Assoc -> Assoc
step = updateCenters . updateAssignments

kmeans :: RandomGen g => Int -> Int -> [Point] -> g -> Assoc
kmeans k itermax ps g = (iterate step $ initializeKMeans ps k g) !! itermax
