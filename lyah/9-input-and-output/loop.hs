main = do
	line <- getLine

	-- WARIING: an if must have the else part
	-- inside a do block, if statements have to be IO sth
	if null line then
		-- WARIING: return in hs is completely different from imperative languages
		-- the return is king of the opposite of '<-'
		-- it create an IO with a pure thing : return a <-> IO a (it wraps sth in a box)
		-- the encapsulated IO action does not do anything
		-- here, we have to do an IO action and we choose the most basic action which is unit ()
		return ()
	else
		-- we have to use a do block because we watnt to do multiple actions
		do
			putStrLn $ reverseWords line
			-- we can call main recursively
			main

-- reverse the words in a string
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
