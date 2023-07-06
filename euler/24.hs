-- PROJECT EULER : problem 24
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

import Data.List ( permutations, sort )

perm = sort . permutations $ "0123456789"
res = perm !! (1000000 - 1)

main :: IO ()
main = do
  let x = read res :: Integer
  print x
