module Gen.Cuboid
    ( generate
    , generateCentered
    ) where

import Data.List ( nub )
import Gen.Basics
import Types

generate :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [Point3]
generate xmin xmax ymin ymax zmin zmax n =
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

generateCentered :: Float -> Int -> [Point3]
generateCentered side n =
    generate (-side) side (-side) side (-side) side n

