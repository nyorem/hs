-- PROJECT EULER : problem 13
-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers (13.txt)

-- conversion string -> integer
readInt :: String -> Integer
readInt = read

-- sum of integers represented as strings
sumString :: [String] -> Integer
sumString = sum . map readInt

-- first ten digits of an integer
tenDigits :: Integer -> Integer
tenDigits n = readInt (take 10 (show n))

main = do
	contents <- readFile "13.txt"
	let linesOfFile = lines contents in
		print $ tenDigits $ sumString linesOfFile
