-- Generates uniformly distributed points on
-- a rectangle or a cuboid.

module Main where

import Data.List ( nub )
import System.Environment ( getArgs )

type Point2 = (Float, Float)
type Point3 = (Float, Float, Float)

showCouple :: (Show a) => (a, a) -> String
showCouple (x, y) =
    show x ++ " " ++ show y

showTriple :: (Show a) => (a, a, a) -> String
showTriple (x, y, z) =
    show x ++ " " ++ show y ++ " " ++ show z

generateX :: Float -> Float -> Float -> Float -> Int -> [Point3]
generateX xmin xmax y z n =
    [ (xmin + (fromIntegral k) * (xmax - xmin) / (fromIntegral n), y, z) | k <- [ 0 .. n ] ]

generateY :: Float -> Float -> Float -> Float -> Int -> [Point3]
generateY ymin ymax x z n =
    [ (x, ymin + (fromIntegral k) * (ymax - ymin) / (fromIntegral n), z) | k <- [ 0 .. n ] ]

generateZ :: Float -> Float -> Float -> Float -> Int -> [Point3]
generateZ zmin zmax x y n =
    [ (x, y, zmin + (fromIntegral k) * (zmax - zmin) / (fromIntegral n)) | k <- [ 0 .. n ] ]

cuboid :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [Point3]
cuboid xmin xmax ymin ymax zmin zmax n =
    nub $    -- bottom
             generateX xmin xmax ymin zmin n
          ++ generateX xmin xmax ymax zmin n
          ++ generateY ymin ymax xmin zmin n
          ++ generateY ymin ymax xmax zmin n
             -- top
          ++ generateX xmin xmax ymin zmax n
          ++ generateX xmin xmax ymax zmax n
          ++ generateY ymin ymax xmin zmax n
          ++ generateY ymin ymax xmax zmax n
             -- left
          ++ generateX xmin xmax ymin zmin n
          ++ generateX xmin xmax ymin zmax n
          ++ generateZ zmin zmax xmin ymin n
          ++ generateZ zmin zmax xmax ymin n
             -- right
          ++ generateX xmin xmax ymax zmin n
          ++ generateX xmin xmax ymax zmax n
          ++ generateZ zmin zmax xmin ymax n
          ++ generateZ zmin zmax xmax ymax n
             -- back
          ++ generateY ymin ymax xmin zmin n
          ++ generateY ymin ymax xmin zmax n
          ++ generateZ zmin zmax xmin ymin n
          ++ generateZ zmin zmax xmin ymax n
             -- front
          ++ generateY ymin ymax xmax zmin n
          ++ generateY ymin ymax xmax zmax n
          ++ generateZ zmin zmax xmax ymin n
          ++ generateZ zmin zmax xmax ymax n

cube :: Float -> Int -> [Point3]
cube side n =
    cuboid (-side) side (-side) side (-side) side n

fst2 :: (a, b, c) -> (a, b)
fst2 (a, b, _) =
    (a, b)

square :: Float -> Int -> [Point2]
square side n =
    map fst2 $ cuboid (-side) side (-side) side (-side) side n

showXYZs :: (Show a) => [(a, a, a)] -> String
showXYZs =
    unlines . map showTriple

showXYs :: (Show a) => [(a, a)] -> String
showXYs =
    unlines . map showCouple

writeXYZ :: [Point3] -> FilePath -> IO ()
writeXYZ points path =
    writeFile path (showXYZs points)

writeXY :: [Point2] -> FilePath -> IO ()
writeXY points path =
    writeFile path (showXYs points)

main :: IO ()
main = do
    (n:_) <- getArgs
    let c = cube 1 (read n)
        sq = square 100 (read n)
        line = map fst2 $ generateX (-100) 100 0 0 (read n)
    writeXYZ c "cube.xyz"
    writeXY sq "square.xy"
    writeXY line "line.xy"

