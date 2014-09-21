module Main where

import System.Random
import Control.Monad ( forM_ )
import Data.Char ( toUpper )

type Word = String
type Guess = [Bool]

-- | Loads the dictionary.
loadDict :: FilePath -> IO [Word]
loadDict filename = readFile filename >>= return . lines

-- | Picks randomly a word in the dictionary.
pickWord :: [Word] -> StdGen -> Word
pickWord dict seed = dict !! r
    where (r, _) = randomR (0, length dict - 1) seed

-- | Updates the current word: reveals the letters corresponding to the given char.
updateCurrentWord :: Word -> Guess -> Char -> Guess
updateCurrentWord secretWord currentWord c = reverse $ go secretWord currentWord c []
    where go [] [] _ res = res
          go (x:xs) (y:ys) c res
            | y == False && x == c = go xs ys c (True : res)
            | otherwise = go xs ys c (y : res)

-- | Print the current word.
printCurrentWord :: Word -> Guess -> IO ()
printCurrentWord secretWord currentWord =
    forM_ (zip secretWord currentWord) (\(x, y) -> do
        if y == True
        then
            putChar x
        else
            putChar '*')
    >> putStrLn ""

-- | Determines if the player wins.
playerWins :: Guess -> Bool
playerWins = all (== True)

-- | One step of the game: ask for a character, updates the current guess and loops.
step :: Word -> Guess -> Int -> IO ()
step secretWord currentWord lim = do
    printCurrentWord secretWord currentWord
    char <- fmap toUpper getChar
    let newCurrentWord = updateCurrentWord secretWord currentWord char
    if playerWins newCurrentWord
    then
        putStrLn "\nYou won!"
    else if lim == 0
    then
        putStrLn "\nYou lost!"
    else
        putStrLn "" >> step secretWord newCurrentWord (lim - 1)

limit :: Int
limit = 10

main :: IO ()
main = do
    dict <- loadDict "dico.txt"
    gen <- newStdGen
    let secretWord = pickWord dict gen
        currentWord = replicate (length secretWord) False
    step secretWord currentWord (limit - 1)

