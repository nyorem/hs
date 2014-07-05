-- simple IO program

-- do syntax is used to glue together multiple IO actions becuase by default we can only do ONE action
-- each step in a do block is an IO
-- the type of the action is the type of the last action which is IO () here
-- WARNING: by convention, we do not specify the type of main but it must be like main :: IO sth
-- IO actions have to be inside 'main'
main = do
	-- Here foo have type ()
	foo <- putStrLn "Hello, what's your name ?"

	-- getLine :: IO String
	-- it reads a string from an input stream
	-- it is an example of an impure function : if we execute this code twice, the result will not be the same
	-- <- means bind the RHS to the LHS : it extracts the sth from IO sth and affects it to the LHS (it extract the content of a box)
	-- WARNING: we can extract an information inside an IO action iff we are inside another IO action. If we want to manipulate impure data, we HAVE to handle it into an impure environment
	-- if we did name = getLine, we just make an alias of the getLine function into name
	name <- getLine

	putStrLn $ "Your name is " ++ name
	-- WARNING: in a do block the last action CANNOT be bound to anything

-- REVELATION: ghci execute putStrLn implicitly. When we give it an expression, it evaluates it, apply the show function and then call putStrLn function on the result
