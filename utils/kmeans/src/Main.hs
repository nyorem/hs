module Main where

import KMeans
import Parser

import Control.Monad ( forM_, unless, when )
import qualified Data.Map as M
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as W
import System.Environment
import System.Exit
import System.IO
import System.Random

width, height :: Int
width = 800
height = 600

errorCallback :: W.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: W.KeyCallback
keyCallback window key _ action _ =
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

clusterIdToColour :: Int -> Color3 GLfloat
clusterIdToColour i = colours !! (i `mod` len)
    where colours = [ Color3 1 0 0, Color3 0 1 0, Color3 0 0 1
                    , Color3 1 1 0, Color3 0 1 1, Color3 1 0 1
                    , Color3 1 1 1
                    ]
          len = length colours

render :: W.Window -> Assoc -> IO ()
render w assoc = do
    (width', height') <- W.getFramebufferSize w
    viewport $= (Position 0 0, Size (fromIntegral width') (fromIntegral height'))

    clear [ColorBuffer]

    forM_ (M.toList assoc) $ \((x, y), Cluster { clusterId = i}) -> do
        renderPrimitive Points $ do
            color $ clusterIdToColour i
            vertex $ Vertex3 x y 0

usage :: String -> String
usage progName = unwords ["Usage:", progName, "nclusters filename"]

parseArgs :: String -> [String] -> (FilePath, Int)
parseArgs _ [] = ("", 2)
parseArgs _ [n] = ("", read n)
parseArgs _ [n, filename] = (filename, read n)
parseArgs progName _ = error $ usage progName

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    let (filename, nclusters) = parseArgs progName args
    g <- getStdGen
    points <- if null filename
              then return $ take 1000 $ randomPointsRs (-1, 1) (-1, 1) g
              else parseFile filename
    let clusters = kmeans nclusters 10 points g
    w <- initialize "KMeans"
    putStrLn $ "initial points: " ++ show points
    putStrLn $ "number of clusters: " ++ show nclusters
    mainLoop (render w clusters) w
    cleanup w

initialize :: String -> IO W.Window
initialize title = do
    W.setErrorCallback (Just errorCallback)
    successfulInit <- W.init

    if not successfulInit then exitFailure else do
        W.windowHint $ W.WindowHint'ContextVersionMajor 2
        W.windowHint $ W.WindowHint'ContextVersionMinor 1
        W.windowHint $ W.WindowHint'Resizable True

        mw <- W.createWindow width height title Nothing Nothing
        case mw of
            Nothing -> W.terminate >> exitFailure
            Just window -> do
                W.makeContextCurrent mw
                W.setKeyCallback window (Just keyCallback)
                return window

cleanup :: W.Window -> IO ()
cleanup win = do
    W.destroyWindow win
    W.terminate
    exitSuccess

mainLoop :: IO () -> W.Window -> IO ()
mainLoop draw w = do
    close <- W.windowShouldClose w
    unless close $ do
        draw
        W.swapBuffers w
        W.pollEvents
        mainLoop draw w

