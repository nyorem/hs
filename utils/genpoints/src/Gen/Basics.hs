module Gen.Basics
    ( generateX
    , generateY
    , generateZ
    ) where

import Types

generateX :: Float -> Float -> Float -> Float -> Int -> [Point3]
generateX xmin xmax y z n =
    [ (xmin + (fromIntegral k) * (xmax - xmin) / (fromIntegral n), y, z) | k <- [ 0 .. n ] ]

generateY :: Float -> Float -> Float -> Float -> Int -> [Point3]
generateY ymin ymax x z n =
    [ (x, ymin + (fromIntegral k) * (ymax - ymin) / (fromIntegral n), z) | k <- [ 0 .. n ] ]

generateZ :: Float -> Float -> Float -> Float -> Int -> [Point3]
generateZ zmin zmax x y n =
    [ (x, y, zmin + (fromIntegral k) * (zmax - zmin) / (fromIntegral n)) | k <- [ 0 .. n ] ]

