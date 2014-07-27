module Main where

import System.Environment

-- processing files as strings is slow
-- a 'Char' does not have a fixed size
-- SOLUTION: bytestrings ~ lists whose each element is one byte in size and
-- they handle laziness differently

-- there are two forms of bytestrings: strict or lazy
-- --> strict: if we evaluate an element, the while string will be evaluated and of course there are no infinite bytestrings
-- --> lazy: stored in chunks (64 K), if we want to evaluate a byte then 64K will be evaluated

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- Word8: 8 bits integer -> 0 .. 255
-- pack: create a bytestring from a list
-- ex: B.pack [97 .. 122] == B.ByteString "abcdefghijklmnopqrstuvwxyz"
-- unpack: inverse of pack
-- conversion between strict and lazy bytestrings: fromChunks and toChunks
-- ex: B.fromChunks [S.pack [97], S.pack [98]] == B.ByteString "ab"
-- (:) == cons and cons' is the strict version (directly insert the new byte and not wait)
-- empty: creates an empty bytestring
-- pretty much all the functions that operate on lists have their equivalent for bytestrings
-- same things for IO

main :: IO ()
main = do
    (filename1:filename2:_) <- getArgs
    copyFile filename1 filename2
    return ()

-- copies a file using bytestrings
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
    contents <- B.readFile src
    B.writeFile dst contents
