-- PROJECT EULER : problem 25
-- What is the first term in the Fibonacci sequence to contain 1000 digits?

-- fibonacci sequence
fibo :: Integer -> Integer
fibo n = fibaux 1 1 n
	where fibaux a b k
		| k == 1 = b
		| otherwise = fibaux (a+b) a (k-1)

-- number of digits
numDigits :: Integer -> Int
numDigits n = length $ show n

res = (length $ takeWhile (\n -> numDigits n < 1000) $ map fibo [1..]) + 1

main :: IO ()
main = print res
