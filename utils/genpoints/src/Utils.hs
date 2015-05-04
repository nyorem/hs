module Utils
    ( showCouple
    , showTriple
    , fst2
    , writeXY
    , writeXYZ
    ) where

import Types

showCouple :: (Show a) => (a, a) -> String
showCouple (x, y) =
    unwords [show x, show y]

showTriple :: (Show a) => (a, a, a) -> String
showTriple (x, y, z) =
    show x ++ " " ++ show y ++ " " ++ show z

fst2 :: (a, b, c) -> (a, b)
fst2 (a, b, _) =
    (a, b)

showXYs :: (Show a) => [(a, a)] -> String
showXYs =
    unlines . map showCouple

showXYZs :: (Show a) => [(a, a, a)] -> String
showXYZs =
    unlines . map showTriple

writeXY :: [Point2] -> FilePath -> IO ()
writeXY points path =
    writeFile path (showXYs points)

writeXYZ :: [Point3] -> FilePath -> IO ()
writeXYZ points path =
    writeFile path (showXYZs points)

