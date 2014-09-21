import Data.Ratio
import Control.Applicative
import Data.List ( all, sortBy, groupBy )
import Data.Ord ( comparing )

-- type for representing non determinism with probabilities:
-- -> list of pairs (x, p)
-- ex: [(3, 1 % 2), (5, 1 % 4), (9, 1 % 4)]
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show)

testProb :: Prob Int
testProb = Prob $ [(3, 1 % 2), (5, 1 % 4), (9, 1 % 4)]

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)
    ]

-- Functor instance
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- Applicative instance
instance Applicative Prob where
    pure x = Prob [(x, 1 % 1)]
    Prob fs <*> Prob xs = Prob $ [(f x, p * pp) | (f, pp) <- fs, (x, p) <- xs]

-- Monad instance
-- flattens a Probability list of Probability lists
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerXs, p) = map (\(x, r) -> (x, p * r)) innerXs

-- join probabilities with the same outcomes
flattenOutcomes :: (Ord a) => Prob a -> Prob a
flattenOutcomes (Prob xs) =
     Prob . concat . map sumProb . groupBy (\(x, _) (y, _) -> x == y) . sortBy (comparing fst) $ xs
         where sumProb ys = [(fst . head $ ys, sum $ map snd ys)]

instance Monad Prob where
    return x = Prob [(x, 1 % 1)]
    p >>= f = flatten (fmap f p)
    fail _ = Prob []

-- example of Prob usage
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

-- if we throw these two coins, what are the probability of all of them landing tails ?
flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (== Tails) [a, b, c])
-- answer = 9 % 40

