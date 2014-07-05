-- PROJECT EULER : problem 17
-- Sum of all the letters use to write the numbers from 1 to 1000 ?

units :: Int -> String
units 0 = "zero"
units 1 = "one"
units 2 = "two"
units 3 = "three"
units 4 = "four"
units 5 = "five"
units 6 = "six"
units 7 = "seven"
units 8 = "eight"
units 9 = "nine"
units _ = "not a unit"

tens :: Int -> String
tens 1 = "ten"
tens 2 = "twenty"
tens 3 = "thirty"
tens 4 = "forty"
tens 5 = "fifty"
tens 6 = "sixty"
tens 7 = "seventy"
tens 8 = "eighty"
tens 9 = "ninety"
tens _ = "not a ten"

-- condition : n <= 1000
numberToStr :: Int -> String
numberToStr 11 = "eleven"
numberToStr 12 = "twelve"
numberToStr 13 = "thirteen"
numberToStr 14 = "fourteen"
numberToStr 15 = "fifteen"
numberToStr 16 = "sixteen"
numberToStr 17 = "seventeen"
numberToStr 18 = "eighteen"
numberToStr 19 = "nineteen"
numberToStr n
    | n <= 9 = units n
    | n <= 99 = tens t ++ (if u == 0 then "" else " " ++ numberToStr u)
    | n <= 999 = units h ++ " hundred" ++ (if nWithoutHundreds == 0 then "" else " and " ++ numberToStr nWithoutHundreds)
    | n == 1000 = "one thousand"
    | otherwise = "not handled"
    where u = n `mod` 10
          t = (n `div` 10) `mod` 10
          h = (n `div` 100) `mod` 10
          nWithoutHundreds = n - (h * 100)

-- 'length' of a number in terms of number of letters
lengthNumber :: Int -> Int
lengthNumber = sum . (map length) . (words . numberToStr)

sumNumbers :: Int -> Int
sumNumbers n = sum . (map lengthNumber) $ [1 .. n]

res = sumNumbers 1000
