-- PROJECT EULER : problem 35
-- How many circular primes are there below one million?

module Main where

-- rotations of a list
rotations :: (Eq a ) => [a] -> [[a]]
rotations [] = []
rotations xs = go ys [xs]
    where ys = tail xs ++ [head xs]
          go zs res
            | zs == xs = res
            | otherwise = go (tail zs ++ [head zs]) (zs : res)

-- rotations of a number
rotationsNumber :: Integer -> [Integer]
rotationsNumber n = map read $ rotations (show n)

-- primality test
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | n `mod` 2 == 0 = False
isPrime n = verifDiv 2 maxi
            where verifDiv k b
                      | k <= b = if n `mod` k == 0 then False else verifDiv (k + 1) b
                      | otherwise = True
                  maxi = round . sqrt . fromIntegral $ n

res = length $ filter (\n -> all isPrime (rotationsNumber n)) [ 1 .. 1000000 ]

main :: IO ()
main = print res

