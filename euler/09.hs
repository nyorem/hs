-- PROJECT EULER : problem 9
-- There exists exactly one Pythagorean triplet (a^2 + b^2 = c^2 where a < b < c) for which a + b + c = 1000.
-- Find the product abc.

pythagore s = [[a, b, c] | c <- [1..s], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == s]

res = product (head (pythagore 1000))
