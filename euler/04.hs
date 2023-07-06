-- PROJECT EULER : problem 4
-- Find the largest palindrome made from the product of two 3-digit numbers.

-- generating products of all 3 digits pairs
products = [x * y | x <- [0..999], y <- [0..999], x <= y]

-- palindrome test on strings
palindromeStr :: String -> Bool
palindromeStr l = (l == reverse l)

-- palindrom test on numbers
palindrome :: Integer -> Bool
palindrome n = palindromeStr (show n)

res = maximum (filter palindrome products)

main :: IO ()
main = print res
