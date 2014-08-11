import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

import Control.Monad ( forM )

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Numbers ( parseFloat )

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )

-- | Parses a xyz file.
parseFile :: String -> [(Double, Double, Double)]
parseFile cont =
    case parse parseXYZ "" cont of
        Left _ -> error "Error during parsing"
        Right xs -> xs

parseXYZ :: Parser [(Double, Double, Double)]
parseXYZ = line `endBy` newline

line :: Parser (Double, Double, Double)
line = do
    x <- parseFloat
    y <- parseFloat
    z <- parseFloat
    return (x, y, z)

-- | Generate a noise value using a normal distribution.
genNoise :: Double -> Double -> IO Double
genNoise m sd = withSystemRandom . asGenIO . genContinous $ normalDistr m sd

-- | Convert a triple to an XYZ coordinates format.
convertToXYZ :: (Double, Double, Double) -> String
convertToXYZ (x, y, z) = unwords [show x, show y, show z]

main :: IO ()
main = do
    -- open the file
    (filename:m:sd:_) <- getArgs
    cont <- readFile filename

    hPutStrLn stderr $ "mean: " ++ m ++ " and standard deviation: " ++ sd

    -- parse and noise the file
    let fileParsed = parseFile cont

    forM fileParsed (\(x, y, z) -> do
        n <- genNoise (read m) (read sd)
        putStrLn $ convertToXYZ (x + n, y + n, z + n))

    return ()

