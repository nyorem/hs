module Main
where

import Data.List ( intercalate )
import Data.Monoid
import qualified Data.Vector as V
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe ( unsafePerformIO )

import SubSurf.Subdiv
import SubSurf.Examples
import SubSurf.Geometry
import SubSurf.Types

width :: Int
width = 800

height :: Int
height = 600

numSimulations :: Int
numSimulations = 100

main :: IO ()
main =
    play (InWindow "SubSurf" (width, height) (10, 10))
         white
         numSimulations
         initialWorld
         worldToPicture
         handleEvents
         step

-- | World consists of a list of points,
-- the corresonding polygon and the number of iterations
data World = World Points Polygon Int

initialWorld :: World
initialWorld = World V.empty [] 0

handleEvents :: Event -> World -> World
handleEvents event w@(World points poly iter) =
    case event of
        EventKey (MouseButton LeftButton) Up _ p -> World (addPoint p w) poly iter
        EventKey (Char 'r') Up _ _ -> initialWorld
        EventKey (Char 'c') Up _ _ -> World (subdiv chaikin points) poly (iter + 1)
        EventKey (Char 'f') Up _ _ -> World (subdiv fourPoints points) poly (iter + 1)
        EventKey (Char 'u') Up _ _ -> World (uniformSplines 3 points) poly (iter + 1)
        EventKey (Char 'p') Up _ _ -> World points (V.toList points) iter
        EventKey (Char 'o') Up _ _ -> unsafePerformIO (outputPolygon w "sub" >> return w)
        EventKey (Char 'a') Up _ _ -> unsafePerformIO (outputAngles w "angles" >> return w)
        _ -> w

-- | Outputs the current subdivision in a file.
outputPolygon :: World -> FilePath -> IO ()
outputPolygon (World points _ i) filename = writeFile name contents
    where name = filename ++ show i ++ ".txt"
          closed = V.snoc points (V.head points)
          contents = intercalate "\n" . V.toList $ V.map (\(x, y) -> show x ++ " " ++ show y) closed

-- | Outputs the angles of the current subdivision in a file.
outputAngles :: World -> FilePath -> IO ()
outputAngles (World points _ i) filename = writeFile name contents
    where name = filename ++ show i ++ ".txt"
          angles' = angles $ V.toList points
          contents = intercalate "\n" $ map show angles'

addPoint :: Point -> World -> Points
addPoint p (World points poly _)
    | null poly = V.snoc points p
    | otherwise = newPoints
    where i = V.minIndexBy (\q r -> squaredDist p q `compare` squaredDist p r) points
          prev = points <!> (i - 1)
          next = points <!> (i + 1)
          len = V.length points
          j = if squaredDist p prev < squaredDist p next then (i - 1) `mod` len else i
          newPoints = V.concat [V.take (j + 1) points, V.singleton p, V.drop (j + 1) points]

step :: Float -> World -> World
step _ world = world

worldToPicture :: World -> Picture
worldToPicture (World points poly _) = (mconcat . V.toList $ V.map drawPoint points) <> (color green $ lineLoop poly)
    where drawPoint (x, y) = color red . translate x y $ line [(-s, 0), (s, 0)] <> line [(0, -s), (0, s)]
          s = 5

