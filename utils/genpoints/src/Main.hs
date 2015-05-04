module Main where

import qualified Gen.Circle as Circle
import qualified Gen.Cuboid as Cuboid
import qualified Gen.Line as Line
import qualified Gen.Square as Square
import System.Environment ( getArgs )
import Types
import Utils

main :: IO ()
main = do
    (n:_) <- fmap (map read) getArgs
    let cube = Cuboid.generateCentered 1 n
        circle = Circle.generate (0, 0, 0) 1 YZ n
        square = Square.generate 100 n
        line2 = map fst2 $ Line.x (-100) 100 0 0 n
    writeXYZ cube "cube.xyz"
    writeXYZ circle "circle.xyz"
    writeXY square "square.xy"
    writeXY line2 "line.xy"

