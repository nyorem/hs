module Main where

import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Environment

width, height :: Int
width = 800
height = 600

fromFile :: FilePath -> IO [Point]
fromFile path = do
    contents <- readFile path
    return $ map (tupleToPoint . listToTuple . words) $ lines contents

toPicture :: Color -> [Point] -> Picture
toPicture c points =
    let (xm, ym) = findFrame points
        scaledPoints = map (convertCoord (xm, ym)) points
    in
        pictures $ map (drawPoint c) scaledPoints

colors :: [Color]
colors =
    cycle [black, red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

data World = World { wPics :: [Picture]
                   , wCurrent :: Int
                   , wAll :: Bool
                   }

draw :: World -> Picture
draw w =
    if wAll w then Pictures $ wPics w
    else wPics w !! (wCurrent w)

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey key) Down _ _) w =
    case key of
      KeyRight -> w { wCurrent = (wCurrent w + 1) `mod` (length $ wPics w) }
      KeyLeft  -> w { wCurrent = (wCurrent w - 1) `mod` (length $ wPics w) }
      KeyUp    -> w { wAll = not $ wAll w }
      _        -> w
handleEvent _ w = w

step :: Float -> World -> World
step _ w = w

main :: IO ()
main = do
    args <- getArgs
    unless (null args) $ do
        pics <- sequence $ map (\(c, file) -> toPicture c <$> fromFile file) $ zip colors args
        let initialWorld = World pics 0 False
        play (InWindow "2D Point Cloud Viewer" (width, height) (10, 10))
             white
             60
             initialWorld
             draw
             handleEvent
             step

listToTuple :: [a] -> (a, a)
listToTuple (x:y:_) = (x, y)
listToTuple _ = undefined

tupleToPoint :: (String, String) -> Point
tupleToPoint (x, y) =
    (read x, read y)

drawPoint :: Color -> Point -> Picture
drawPoint c (x, y) =
    color c . translate x y $ circle 2

findFrame :: [Point] -> (Float, Float)
findFrame points =
    let xs = map (abs . fst) points
        ys = map (abs . snd) points
        dx = maximum xs - minimum xs
        dy = maximum ys - minimum ys
    in (maximum xs + dx * 0.1, maximum ys + dy * 0.1)

-- see: http://stackoverflow.com/questions/4520377/converting-window-coordinates-to-axis-coordinates-in-opengl
convertCoord :: (Float, Float) -> Point -> Point
convertCoord (xm, ym) (x, y) =
    ((fromIntegral width / 2) * (x / xm), (fromIntegral height / 2) * (y / ym))

