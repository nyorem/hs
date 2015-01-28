module Main where

import SRTFile
import System.Environment ( getArgs, getProgName )

usage :: String -> String
usage prog = unwords ["Usage:", "./" ++ prog, "old", "new", "time"]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then do
        prog <- getProgName
        putStrLn $ usage prog
    else do
        let (filename:newFilename:time:_) = args
        srt <- parseSRT filename
        let delayTime = parseTime $ if isTimeNegative time then tail time else time
            newSrt = (if isTimeNegative time then forwardSRTFile else delaySRTFile) delayTime srt
        writeFile newFilename (show newSrt)

