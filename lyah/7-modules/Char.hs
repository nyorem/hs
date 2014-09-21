import Data.Char

-- Data.Char
-- functions that deal with characters

-- predicates: isSpace, isControl, isPrint, isOctDigit, isMark, isPunctuation, isSymbol, isAscii, isSeparator...

-- enumeration of types of characters: GeneralCategory
-- generalCategory :: Char -> GeneralCategory

-- conversion: toUpper, toLower, toTitle
-- digitToInt, intToDigit, ord, chr

encode :: Int -> String -> String
encode shift = map (chr . (+ shift) . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)
