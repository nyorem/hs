-- PROJECT EULER : problem 40
-- If dn represents the nth digit of the fractional part of the Champernowne's constnt, find the value of the following expression:
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

import Data.Char ( digitToInt )
champernowne :: String
champernowne = concatMap show [ 0 .. ]

res = product . map (\x -> digitToInt (champernowne !! x)) $ [ 10 ^ x | x <- [ 0 .. 6 ] ]

