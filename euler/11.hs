-- PROJECT EULER : problem 11
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid? (grid.txt)

import Data.Array

-- the grid in an array
grid :: String -> Array (Int, Int) Int
grid = listArray ( (1, 1), (20, 20) ) . map read . words

-- all possible movements
-- vertical
verticalRow :: Array (Int, Int) Int -> (Int, Int) -> Int -> Int -> [Int]
verticalRow g (x, y) deb fin = [ g ! (i, y) | i <- [x + deb .. x + fin],
                                 inRange (bounds g) (i, y) ]

-- horizontal
horizontalRow :: Array (Int, Int) Int -> (Int, Int) -> Int -> Int -> [Int]
horizontalRow g (x, y) deb fin = [ g ! (x, j) | j <- [y + deb .. y + fin],
                                   inRange (bounds g) (x, j) ]

-- diagonals \
diagonalRow1 :: Array (Int, Int) Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [Int]
diagonalRow1 g (x, y) (debX, finX) (debY, finY) = [ g ! (i, j) |
                                                    i <- [x + debX .. x + finX ],
                                                    j <- [y + debY .. y + finY],
                                                    i - x == j - y &&
                                                    inRange (bounds g) (i, j) ]

-- diagonals /
diagonalRow2 :: Array (Int, Int) Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [Int]
diagonalRow2 g (x, y) (debX, finX) (debY, finY) = [ g ! (i, j) |
                                                    i <- [x + debX .. x + finX ],
                                                    j <- [y + debY .. y + finY],
                                                    i - x == -(j - y) &&
                                                    inRange (bounds g) (i, j) ]

-- maximum of all movements
movements :: Array (Int, Int) Int -> (Int, Int) -> Int
movements g (x, y) = maximum [ product ( verticalRow g (x, y) 0 3                 ),
                               product ( verticalRow g (x, y) (-3) 0              ),
                               product ( horizontalRow g (x, y) 0 3               ),
                               product ( horizontalRow g (x, y) (-3) 0            ),
                               product ( diagonalRow1 g (x, y) (0, 3) (0, 3)       ),
                               product ( diagonalRow1 g (x, y) ((-3), 0) ((-3), 0) ),
                               product ( diagonalRow2 g (x, y) (0, 3) ((-3), 0)    ),
                               product ( diagonalRow2 g (x, y) ((-3), 0) (0, 3)    ) ]

-- maximum of the grid
findMaximum :: Array (Int, Int) Int -> Int
findMaximum g = maximum $ map (movements g) [ (i, j) | i <- [ 1 .. 20 ], j <- [ 1 .. 20 ] ]

main :: IO ()
main = do
    input <- readFile "grid.txt"
    let g = grid input
    print $ findMaximum g

