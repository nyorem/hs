module Main where

isPalindrome :: String -> String
isPalindrome str
    | str == (reverse str) = "palindrome"
    | otherwise = "not a palindrome"

tellIfPalindrome :: String -> String
tellIfPalindrome = unlines . map isPalindrome . lines

main = interact tellIfPalindrome
