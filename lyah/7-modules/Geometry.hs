-- first, define which functions need to be exported
module Geometry (
	sphereVolume,
	sphereArea,
	cubeVolume,
	cubeArea,
	cuboidVolume,
	cuboidArea) where

-- then define the functions
sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4.0 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

rectangleArea :: Float -> Float -> Float
rectangleArea width height = width * height

-- cuboid = 'pavé droit' in French
cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * rectangleArea a b + 2 * rectangleArea a c + 2 * rectangleArea b c
