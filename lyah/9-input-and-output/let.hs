import Data.Char (toUpper)

main = do
	putStrLn "What is your first name ?"
	firstName <- getLine
	putStrLn "What is your last name ?"
	lastName <- getLine

	-- we can use let bindings without the in part in do block (like in list comprehension)
	-- WARNING: use '<-' when bindings IO actions and 'let' when using pure functions
	let bigFirstName = map toUpper firstName
	    bigLastName = map toUpper lastName

	putStrLn $ "You are " ++ bigFirstName ++ " " ++ bigLastName
