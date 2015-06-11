module Main where

import Control.Monad
import Graphics.Gloss
import System.Environment

width, height :: Int
width = 800
height = 600

fromFile :: FilePath -> IO [Point]
fromFile path = do
    contents <- readFile path
    return $ map (tupleToPoint . listToTuple . words) $ lines contents

toPicture :: [Point] -> Picture
toPicture points =
    let (xm, ym) = findFrame points
        scaledPoints = map (convertCoord (xm, ym)) points
    in
        pictures $ map drawPoint scaledPoints

main :: IO ()
main = do
    args <- getArgs
    unless (null args) $ do
        let file = head args
        points <- fromFile file
        display (InWindow "Gloss" (width, height) (10, 10))
                white
                (toPicture points)

listToTuple :: [a] -> (a, a)
listToTuple (x:y:_) = (x, y)
listToTuple _ = undefined

tupleToPoint :: (String, String) -> Point
tupleToPoint (x, y) =
    (read x, read y)

drawPoint :: Point -> Picture
drawPoint (x, y) =
    color red . translate x y $ circle 2

findFrame :: [Point] -> (Float, Float)
findFrame points =
    let xs = map (abs . fst) points
        ys = map (abs . snd) points
    in (maximum xs, maximum ys)

-- see: http://stackoverflow.com/questions/4520377/converting-window-coordinates-to-axis-coordinates-in-opengl
convertCoord :: (Float, Float) -> Point -> Point
convertCoord (xm, ym) (x, y) =
    ((fromIntegral width / 2) * (x / xm), (fromIntegral height / 2) * (y / ym))

