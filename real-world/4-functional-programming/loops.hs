import Data.Char ( digitToInt, isSpace )

-- | Converts a string to an int.
asInt :: String
      -> Int
asInt str = loop 0 str
    where loop acc [] = acc
          loop acc (x:xs) =
              let acc' = acc * 10 + digitToInt x in
                  loop acc' xs

-- | Better way to convert a string to an int.
asIntFold :: String -> Int
asIntFold "" = 0
asIntFold str@(c:cs)
    | c == '-' = negate . asIntFoldAux $ cs
    | otherwise = asIntFoldAux str
        where asIntFoldAux = foldl (\acc c -> 10 * acc + convertToInt c) 0
              convertToInt c
                | c `elem` "0123456789" = digitToInt c
                | otherwise = error "not a digit"

-- | Squares every element in an array.
squareList :: (Num a) => [a] -> [a]
squareList = map (\x -> x * x)

-- | Concatenates a list of lists.
myConcat :: [[a]] -> [a]
myConcat = foldl (++) []

-- | Implementations of takeWhile.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []
takeWhile' _ _ = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f = foldr (\x acc -> if f x then x : acc else []) []

-- | Implementation of groupBy.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr step []
    where step x [] = [[x]]
          step x acc@(y:ys)
            | f x (head y) = (x:y) : ys
            | otherwise = [x] : acc

-- | Some implementations of Prelude functions.
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> acc || f x) False

cycle' :: [a] -> [a]
cycle' xs = foldr (:) (cycle' xs) xs

words' :: String -> [String]
words' str = foldr step [] (dropWhile isSpace str)
    where step c [] = [[c]]
          step c acc@(y:ys)
            | isSpace c = [] : acc
            | otherwise = (c:y) : ys

unlines' :: [String] -> String
unlines' = foldr (\acc str -> acc ++ "\n" ++ str) ""

