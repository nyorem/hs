module Main where

import LSystem
import Examples

import Graphics.Display
import Graphics.Frame

import Control.Monad ( unless, when )
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as W
import System.Exit
import System.IO
import System.Environment ( getArgs )

width, height :: Int
width = 800
height = 600

errorCallback :: W.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: W.KeyCallback
keyCallback window key _ action _ =
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

render :: W.Window -> [Symbol] -> IO ()
render win ss = do
    (w, h) <- W.getFramebufferSize win
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

    clear [ColorBuffer]

    runRenderer renderGrammar (testRenderState ss)

testGrammar :: Grammar
testGrammar = dragonCurve

testRenderState :: [Symbol] -> RenderState
testRenderState ss = RenderState { grammar = testGrammar
                                 , currentFrame = translateFrame (0, -0.5) $ cartesianFrame
                                 , symbols = ss
                                 , varFuncs = dragonVarFunctions
                                 , constantFuncs = dragonConstantFunctions
                                 }

main :: IO ()
main = do
    args <- getArgs
    unless (null args) $ do
        let n = read (head args)
            ss = nthStep n testGrammar
        w <- initialize "L-system"
        mainLoop (render w ss) w
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

