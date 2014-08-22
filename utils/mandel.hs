-- http://yannesposito.com/Scratch/en/blog/Haskell-OpenGL-Mandelbrot/

module Main where

import Control.Monad ( unless, when )
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as W
import Unsafe.Coerce ( unsafeCoerce )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )

width, height :: GLfloat
width = 320
height = 320

coerce :: GLfloat -> Int
coerce f = round (unsafeCoerce f :: Float)

-- we define our own Complex data type
data Complex = Complex { re :: GLfloat, im :: GLfloat } deriving (Show, Eq)

instance Num Complex where
    fromInteger n = Complex (fromIntegral n) 0.0
    Complex x y + Complex x' y' = Complex (x + x') (y + y')
    Complex x y * Complex x' y' = Complex (x * x' - y * y') (x * y' + x' * y)
    negate (Complex x y) = Complex (-x) (-y)
    abs (Complex x y) = Complex (sqrt (x * x + y * y)) 0.0
    signum (Complex x y) = Complex (signum x) 0.0

complex :: GLfloat -> GLfloat -> Complex
complex = Complex

magnitude :: Complex -> GLfloat
magnitude = re . abs

-- Mandelbrot sequence
mandel :: Complex -> Complex -> Int -> Int
mandel c z 0 = 0
mandel c z n
    | magnitude z > 2 = n
    | otherwise = mandel c (z * z + c) (n - 1)

-- Rendering
mandelPixel :: GLfloat -> GLfloat -> Int
mandelPixel x y =
    let r = 2.0 * x / width
        i = 2.0 * y / height
    in
        mandel (complex r i) 0 64

allPoints :: [(GLfloat, GLfloat, Color3 GLfloat)]
allPoints = [ (x / width, y / height, colorFromValue $ mandelPixel x y)
            | x <- [ -width .. width ],
              y <- [ -height .. height ] ]

colorFromValue :: Int -> Color3 GLfloat
colorFromValue n =
    let t :: Int -> GLfloat
        t i = 0.5 + 0.5 * cos (fromIntegral i / 10)
    in
        Color3 (t n) (t (n + 5)) (t (n + 10))

render :: IO ()
render =
    renderPrimitive Points $ do
        mapM_ drawColoredPoint allPoints
            where drawColoredPoint (x, y, c) = do
                    color c
                    vertex $ Vertex3 x y 0

-- OpenGL stuff
bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

errorCallBack :: W.ErrorCallback
errorCallBack err desc = hPutStrLn stderr desc

keyCallback :: W.KeyCallback
keyCallback window key scancode action mods =
    when (key == W.Key'Escape && action == W.KeyState'Pressed) $
        W.setWindowShouldClose window True

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

main :: IO ()
main = do
    W.setErrorCallback (Just errorCallBack)
    successfulInit <- W.init

    bool successfulInit exitFailure $ do
        W.windowHint $ W.WindowHint'ContextVersionMajor 2
        W.windowHint $ W.WindowHint'ContextVersionMinor 1

        W.windowHint $ W.WindowHint'Resizable True

        mw <- W.createWindow (coerce width) (coerce height) "OpenGL" Nothing Nothing
        maybe' mw (W.terminate >> exitFailure) $ \window -> do
            W.makeContextCurrent mw
            W.setKeyCallback window (Just keyCallback)

            mainLoop window

            W.destroyWindow window
            W.terminate
            exitSuccess

mainLoop :: W.Window -> IO ()
mainLoop window = unless' (W.windowShouldClose window) $ do
        clear [ColorBuffer]
        render

        W.pollEvents
        W.swapBuffers window

        mainLoop window

