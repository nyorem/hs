import Control.Monad
import Control.Monad.Writer

-- every monad can act as a functor but it does not necessarily have a Functor
-- instance (so a fmap function) --> liftM :: (a -> b) -> m a -> m b
-- liftM f m = m >>= return . f
-- it lifts a function to a monadic context
-- ex: lifftM (+1) (Just 3)
--     runWriter $ liftM not $ writer (True, "log") --> applies not to True (the a from Writer w a)

-- idem for applicatives with the <*> and ap functions
-- ap :: m (a -> b) -> m a -> m b
-- ap mf m = do
--   f <- mf
--   x <- m
--   return (f x)
-- there also exists a liftM2 (likfe liftA2) function

-- join: flatten something like m (m a)
-- the flattening property is unique to monads.
-- join :: (Monad m) => m (m a) -> m a
-- for lists: join = concat
-- ex: runWriter $ join $ writer (writer (1, "aaa"), "bbb") = (1, "bbbaaa")
--     runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0] = push 1, 2 and then 10
-- implementation:
-- join mm = do
--    m <- mm
--    m
-- IMPORTANT: m >>= f = join (fmap f m)

-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
-- filterM _ [] = return []
-- filterM p (x:xs) = do
--    b <- p x
--    y <- filterM p xs
--    if b then return (x:y) else return y
keepSmall :: Int -> Writer [String] Bool
keepSmall n
    | n < 4 = do
        tell ["Keeping: " ++ show n]
        return True
    | otherwise = do
        tell [show n ++ " is too large, throwing it away!"]
        return False
-- ex: mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [1, 4, 5, 3]

-- powerset: set of all subsets of a list
powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True, False])

-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-- foldM _ a [] = return a
-- foldM f a (x:xs) = f a x >>= \fax -> foldM f fax xs
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)
-- ex: foldM binSmalls 0 [1, 2, 10]

-- old rpn (chapter 10): does not check if the input is correct
oldSolveRPN :: String -> Float
oldSolveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:xs) "*" = (x * y) : xs
          foldingFunction (x:y:xs) "+" = (x + y) : xs
          foldingFunction (x:y:xs) "-" = (y - x) : xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs n = read n : xs

foldingFunction :: [Double] -- ^ Stack
                -> String -- ^ New element
                -> Maybe [Double] -- ^ Maybe the new stack
foldingFunction (x:y:xs) "*" = return $ (x * y) : xs
foldingFunction (x:y:xs) "+" = return $ (x + y) : xs
foldingFunction (x:y:xs) "-" = return $ (y - x) : xs
foldingFunction xs n = liftM (:xs) (readMaybe n)

-- | Maybe reads something from a string.
readMaybe :: (Read a) => String -> Maybe a
readMaybe str =
    case reads str of
        [(x, "")] -> Just x
        _         -> Nothing

-- good rpn with error handle
solveRPN :: String -> Maybe Double
solveRPN str = do
    [result] <- foldM foldingFunction [] (words str)
    return result

-- monadic functions composition with (<=<) and (>=>)
-- normal function composition: foldr (.) id [functions] $ x
-- monadic function composition: foldr (<=<) return [functions] $ x
-- ex: for knight moves (chapter 12)
-- all the possibles movements with x steps
-- inMany :: Int -> KnightPos -> [KnightPos]
-- inMany x start = return start >>= foldr (<=<) (replicate x moveKnight)
-- canReachIn :: Int -> KnightPos -> KnightPos -> Bool
-- canReachIn x start end = end `elem` inMany x start

