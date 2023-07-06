-- PROJECT EULER : problem 36
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

-- convert a number in base 10 to base 2
convertBase2 :: Integer -> String
convertBase2 n
    | n == 0 = ""
    | otherwise = convertBase2 q ++ show r
        where (q, r) = (n `div` 2, n `mod` 2)

-- test if the string is a palindrome
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- test if the given number is a palindrome in base 10 and 2
palindromeIn2And10 :: Integer -> Bool
palindromeIn2And10 n = isPalindrome (show n) && isPalindrome (convertBase2 n)

res = sum $ filter palindromeIn2And10 [ 1 .. 1000000]

main :: IO ()
main = print res
