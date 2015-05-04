module Gen.Square
    ( generate
    ) where

import qualified Gen.Cuboid as Cuboid

import Types
import Utils

generate :: Float -> Int -> [Point2]
generate side n =
    map fst2 $ Cuboid.generate (-side) side (-side) side (-side) side n

