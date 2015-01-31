module SubSurf.Examples ( cornerCutting,
                          chaikin,
                          generalizedFourPoints,
                          fourPoints,
                          uniformSplines
                        )
where

import SubSurf.Subdiv
import SubSurf.Geometry
import SubSurf.Types
import qualified Data.Vector as V

cornerCutting :: Float -> Float -> Scheme
cornerCutting a b  = ( [ (0, a), (1, 1 - a) ]
                     , [ (0, b), (1, 1 - b) ] )

chaikin :: Scheme
chaikin = cornerCutting 0.75 0.25

generalizedFourPoints :: Float -> Scheme
generalizedFourPoints epsilon = ( [ (0, 1) ]
                                , [ (-1, - epsilon / 2), (2, -epsilon / 2)
                                  , (0, (1 + epsilon) / 2), (1, (1 + epsilon) / 2) ] )

fourPoints :: Scheme
fourPoints = generalizedFourPoints (1 / 8)

-- | Maps 'k' times a function on a vector.
mapN :: Int -> (V.Vector a -> V.Vector a) -> V.Vector a -> V.Vector a
mapN k f v
    | k == 0 = v
    | otherwise = mapN (k - 1) f (f v)

uniformSplines :: Int -> Points -> Points
uniformSplines k points = mapN k avg dup
    where dup = V.concatMap (\p -> V.fromList [p, p]) points
          avg points' = V.map (\(i, _) -> 0.5 <**> ((points' <!> i) <+> (points' <!> (i + 1)))) $ V.indexed points'

