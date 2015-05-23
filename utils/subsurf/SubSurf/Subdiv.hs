module SubSurf.Subdiv
    ( subdiv
    , (<!>)
    ) where

import qualified Data.Vector as V
import Graphics.Gloss
import SubSurf.Geometry
import SubSurf.Types

subdiv :: Scheme -> Points -> Points
subdiv (a, b) points = V.concatMap (\i -> V.fromList [subdiv' a i points, subdiv' b i points]) $ V.enumFromN 0 len
    where len = V.length points

subdiv' :: Coeff -> Int -> Points -> Point
subdiv' coeff i points = foldr (\(j, a) acc -> acc <+> (a <**> (points <!> (i + j)))) (0, 0) $ coeff

(<!>) :: V.Vector a -> Int -> a
v <!> i = v V.! (i `mod` len)
    where len = V.length v

