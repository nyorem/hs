module Main where

-- types
-- Node: either two roads (edges) or one (for the last)
data Node = Node Road Road | EndNode Road
-- Road: cost + origin of the road = Node
data Road = Road Int Node
-- maybe too complex, let's see if we can make this simpler

-- Section: A and B roads + crossroad C
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
-- RoadSystem : list of roads
type RoadSystem = [Section]

-- path representation: label + cost (Int)
data Label =  A | B | C deriving (Show)
type Path = [(Label, Int)]

-- cost of a path
costPath :: Path -> Int
costPath = sum . (map snd)

-- example of RoadSystem
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- one step of the algorithm
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = costPath pathA
        priceB = costPath pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + c + a
        newPathToA = if forwardPriceToA <= crossPriceToA then (A, a):pathA
                     else (C, c):(B, b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB then (B, b):pathB
                     else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

-- compute optimal path
optimalPath :: RoadSystem -> Path
optimalPath rs =
    let (bestPathA, bestPathB) = foldl roadStep ([], []) rs
    in if costPath bestPathA <= costPath bestPathB then
         reverse bestPathA
       else
         reverse bestPathB

-- split a list into groups of n elements
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main :: IO ()
main = do
    -- road system as input
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a, b, c] -> Section a b c) threes
        bestPath = optimalPath roadSystem
        pathString = concat $ map (show . fst) bestPath
        pathPrice = costPath bestPath
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Its price is: " ++ show pathPrice
