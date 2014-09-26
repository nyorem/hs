{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad ( unless, when )
import Data.Maybe ( maybe, fromJust )
import Data.IORef
import FRP.Yampa
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as W
import System.Exit
import System.IO

type Pos = Double
type Vel = Double

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall y0 v0 =
    proc _ -> do
        v <- integral >>^ (+ v0) -< (-9.81)
        y <- integral >>^ (+ y0) -< v
        returnA -< (y, v)

bouncingBall :: Pos -> Vel -> SF () (Pos, Vel)
bouncingBall y0 v0 = switch (bb y0 v0) bswitch
    where
        bb y0' v0' = proc input -> do
            (pos, vel) <- fallingBall y0' v0' -< input
            event' <- edge -< pos <= 0
            returnA -< ((pos, vel), event' `tag` (pos, vel))
        bswitch (pos, vel)
            | abs vel < 1.0 = constant (0.0, 0.0)
            | otherwise     = bouncingBall pos (-vel * 0.6)

width, height :: Int
width = 800
height = 600

errorCallBack :: W.ErrorCallback
errorCallBack err desc = hPutStrLn stderr desc

keyCallback :: W.KeyCallback
keyCallback window key scancode action mods =
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

draw :: Pos -> IO ()
draw pos = do
    renderPrimitive Points $ do
        vertex $ (Vertex3 0 (realToFrac pos / 10) 0 :: Vertex3 GLfloat)

mainSF :: SF () (IO ())
mainSF = (bouncingBall 10.0 0.0) >>^ \ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> draw pos

main :: IO ()
main = do
    W.setErrorCallback (Just errorCallBack)
    successfulInit <- W.init

    bool successfulInit exitFailure $ do
        W.windowHint $ W.WindowHint'ContextVersionMajor 2
        W.windowHint $ W.WindowHint'ContextVersionMinor 1

        W.windowHint $ W.WindowHint'Resizable False

        mw <- W.createWindow width height "OpenGL" Nothing Nothing
        maybe' mw (W.terminate >> exitFailure) $ \window -> do
            W.makeContextCurrent mw
            W.setKeyCallback window (Just keyCallback)

            oldTime <- newIORef (0 :: Double)
            oldTime' <- unsafeGetTime
            writeIORef oldTime oldTime'

            pointSize $= 3.0

            rh <- reactInit (return ()) (\_ _ b -> b >> return False) mainSF
            mainLoop oldTime rh window

            W.destroyWindow window
            W.terminate
            exitSuccess

mainLoop :: IORef Double -> ReactHandle () (IO ()) -> W.Window -> IO ()
mainLoop oldTime rh window = unless' (W.windowShouldClose window) $ do
        clear [ColorBuffer]

        oldTime' <- readIORef oldTime
        newTime' <- unsafeGetTime
        let dt = newTime' - oldTime'
        _ <- react rh (dt, Nothing)
        writeIORef oldTime newTime'

        W.pollEvents
        W.swapBuffers window

        mainLoop oldTime rh window

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = maybe nothingRes f m

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

unsafeGetTime :: IO Double
unsafeGetTime = do
    time <- W.getTime
    return $ fromJust time

