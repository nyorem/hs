module Main where

import SRTFile
import System.Environment ( getArgs, getProgName )

usage :: String -> String
usage prog = unwords ["Usage:", "./" ++ prog, "old", "new", "time"]

-- TODO:
-- -> simplify Main: remove conditions
-- -> abstracts addTimes and subTimes

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then do
        prog <- getProgName
        putStrLn $ usage prog
    else do
        let (filename:newFilename:time:_) = args
        srt <- parseSRT filename
        if head time == '-' then do
            let delayTime = constructTimeFromStr (tail time)
                newSrt    = applyToSRTLine (subTimeSRTLine delayTime) srt
            writeFile newFilename (show newSrt)
        else do
            let delayTime = constructTimeFromStr time
                newSrt    = applyToSRTLine (addTimeSRTLine delayTime) srt
            writeFile newFilename (show newSrt)

