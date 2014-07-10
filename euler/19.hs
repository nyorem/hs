-- PROJECT EULER : problem 19
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-- find the day corresponding to a date
-- http://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
day :: Int -> Int -> Int -> Int
day y m d =
    let c = (14 - m) `div` 12
        a = y - c
        m' = m + 12 * c - 2
        j = (d + a + a `div` 4 - a `div` 100 + a `div` 400 + (31 * m') `div` 12) in
        j `mod` 7

-- is a number divisible by an other one
dividedBy :: Int -> Int -> Bool
dividedBy a n = (a `mod` n) == 0

-- number of days of a month in a year
numberOfDays :: Int -> Int -> Int
numberOfDays m y
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise = if ((y `dividedBy` 4) && not (y `dividedBy` 100)) || (y `dividedBy` 400) then 29 else 28 -- leap year

-- years
years :: [(Int, Int, Int)]
years = [ (y, m, d) | y <- [1901 .. 2000], m <- [1 .. 12], d <- [1 .. numberOfDays m y] ]

res = length . filter (\(y, m, d) -> (day y m d) == 0 && (d == 1)) $ years
