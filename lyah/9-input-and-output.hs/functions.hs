putStr' :: String -> IO ()
putStr' "" = return ()
putStr' (x:xs) = do
	putChar x
	putStr xs

main = do
	-- putStrLn
	putStrLn $ "putStrLn prints a string and a newline"

	-- putStr
	putStr $ "putStr prints a strign without jumping to a newline at the end"
	putStrLn $  ""

	-- putChar
	putStr $ "putChar puts a simple character on the screen : "
	putChar 'a'
	putStrLn $ ""

	-- print
	putStr $ "print puts a thing that can be shown on the screen : "
	print $ 1
	print $ "salut with quotes"

	-- getChar
	c <- getChar
	if (c /= ' ') then
		do
			putChar c
			main
	else
		return ()
