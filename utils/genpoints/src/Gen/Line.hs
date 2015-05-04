module Gen.Line
    ( x
    , y
    , z
    ) where

import Gen.Basics
import Types

x :: Float -> Float -> Float -> Float -> Int -> [Point3]
x =
    generateX

y :: Float -> Float -> Float -> Float -> Int -> [Point3]
y =
    generateY

z :: Float -> Float -> Float -> Float -> Int -> [Point3]
z =
    generateZ

