module Main where

import SRTFile
import System.Environment ( getArgs, getProgName )

usage :: String -> String
usage prog =
    "Usage: cat old | ./" ++ prog ++ " time > new "

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        prog <- getProgName
        putStrLn $ usage prog
    else do
        old <- getContents
        srt <- extractSRTFile old
        let dt = head args
            delayTime = parseTime $ if isTimeNegative dt then tail dt else dt
            newSrt = (if isTimeNegative dt then forwardSRTFile else delaySRTFile) delayTime srt
        print newSrt

