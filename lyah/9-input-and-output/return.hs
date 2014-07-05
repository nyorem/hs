-- return statements does not end the program
main = do
	-- here foo is a String which content is "haha"
	-- it is equivalent to do 'let foo = "haha"'
	foo <- return "haha"
	putStrLn foo
	line <- getLine
	return 42
	putStrLn line
