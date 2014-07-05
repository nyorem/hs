module Main where

import System.Random

-- PROBLEM: Haskell is pure so each time a function is called with the same arguments then each time the result will be the same
-- SOLUTION: for randomness, we can't just call a function and returns a number, we need to also get the new seed

randomInt :: Int -> (Int, StdGen)
randomInt s =
    -- create a random generator (seed)
    let g = mkStdGen s in
        -- we can use type annotations to get different types of random numbers
        random g :: (Int , StdGen)

-- randoms generate an infinite number of random things based on a generator
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g =
    let (n, g') = random g in
    n:randoms' g'

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 g = ([], g)
finiteRandoms n g =
    let (first, newG) = random g
        (rest, finalG) = finiteRandoms (n-1) newG in
    (first : rest, finalG)

-- randomR returns a random thing in a range
-- randomR (1, 3) (mkStdGen 10) returns an int contained in [1, 3]

-- randomRs is the same except it is an infinite list of randoms

main :: IO ()
main = do
    -- getStdGen returns a generator : it is the same for each execution of the program
    g <- getStdGen
    -- random string
    print $ take 10 $ randomRs ('a', 'z') g

    g2 <- getStdGen
    print $ take 10 $ randomRs ('a', 'z') g2
    -- the two outputs will be the same

    -- we can do two things
    -- use an infinite stream
    let stream = randomRs ('a', 'z') g
        (s1, r1) = splitAt 10 stream
        (s2, _) = splitAt 10 r1 in
        print $ s1 ++ " " ++ s2
    -- generate a new generator
    gen' <- newStdGen
    print $ take 10 $ randomRs ('a', 'z') gen'
