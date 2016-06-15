module Main where

import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

import Control.Monad ( forM, void )

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Numbers ( parseFloat )

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )

-- | Parses a xyz file.
parseFile3 :: String -> [(Double, Double, Double)]
parseFile3 cont =
    case parse parseXYZ "" cont of
        Left _ -> error "Error during parsing"
        Right xs -> xs

-- | Parses a xy file.
parseFile2 :: String -> [(Double, Double)]
parseFile2 cont =
    case parse parseXY "" cont of
        Left _ -> error "Error during parsing"
        Right xs -> xs

parseXYZ :: Parser [(Double, Double, Double)]
parseXYZ = line3 `endBy` newline

parseXY :: Parser [(Double, Double)]
parseXY = line2 `endBy` newline

line2 :: Parser (Double, Double)
line2 = do
    x <- parseFloat
    y <- parseFloat
    return (x, y)

line3 :: Parser (Double, Double, Double)
line3 = do
    x <- parseFloat
    y <- parseFloat
    z <- parseFloat
    return (x, y, z)

-- | Generate a noise value using a normal distribution.
genNoise :: Double -> Double -> IO Double
genNoise m sd = withSystemRandom . asGenIO . genContinous $ normalDistr m sd

-- | Convert a pair to an XY coordinates format.
convertToXY :: (Double, Double) -> String
convertToXY (x, y) = unwords [show x, show y]

-- | Convert a triple to an XYZ coordinates format.
convertToXYZ :: (Double, Double, Double) -> String
convertToXYZ (x, y, z) = unwords [show x, show y, show z]

main :: IO ()
main = do
    -- open the file
    (s':filename:m:sd:_) <- getArgs
    let s = read s' :: Integer
    cont <- readFile filename

    hPutStrLn stderr $ "mean: " ++ m ++ " and standard deviation: " ++ sd

    -- parse and noise the file
    if s == 2 then do
        let fileParsed = parseFile2 cont
        void $ forM fileParsed (\(x, y) -> do
            n <- genNoise (read m) (read sd)
            putStrLn $ convertToXY (x + n, y + n))
    else do
        let fileParsed = parseFile3 cont
        void $ forM fileParsed (\(x, y, z) -> do
            n <- genNoise (read m) (read sd)
            putStrLn $ convertToXYZ (x + n, y + n, z + n))

