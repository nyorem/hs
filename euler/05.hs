-- PROJECT EULER : problem 5
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- find the greatest natural (between 1 and n) that not divide x
-- if all naturals between 1 and n divide x then return 1
divisibleTo _ 1 = 1
divisibleTo x n
	| x `mod` n == 0 = divisibleTo x (n-1)
	| otherwise = n

-- f(n) = smallest natural that is dividable by all naturals between 1 and n
-- 2 cases :
-- * f(n) is dividable by (n+1) ==> f(n+1) = f(n)
-- + f(n) is not dividable by (n+1) ==> divisibleTo give the greater natural between 1 and n+1 that non divide f(n) (== d), then f(n) * d is dividable by all naturals between 1 and (n+1). So maybe f(n+1) = f(n) * d but d is not unique so we try again...
-- x is the current state of the product we are making
-- find x n = find f(n) by starting with 'x'
find x 1 = x
find x n
	| d == 1 = r
	| otherwise = find (x*d) n
	where r = find x (n-1)
	      d = divisibleTo r n

res = find 1 20
