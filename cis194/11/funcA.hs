import Control.Applicative

-- some functions to use in an Applicative context

-- | Discards the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
x *> y = y

-- | Maps an applicative function.
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA _ [] = pure []
mapA f (x:xs) = liftA2 (:) (f x) (mapA f xs)

-- | Transforms a list of Applicative into an Applicative of a list.
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = liftA2 (:) x (sequenceA xs)

-- | Replicates an applicative.
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 _ = pure []
replicateA n x = liftA2 (:) x (replicateA (n - 1) x)

