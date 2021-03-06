module Main where

import JoinList
import Scrabble
import Sized
import JoinListBuffer
import Editor

main = runEditor editor $ unlinesJoinList
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
