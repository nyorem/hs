module Main where

-- instances are defined in Control.Monad.Instances
-- example of a data type that does not obey the functor laws but can be made into the Functor typeclass ('C' = 'Counter')
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust x y) = CJust (x + 1) (f y)

main :: IO ()
main = do
    -- IO is a functor
    -- fmap f action will create an IO action whose result will be f (result of `action`)
    reversedLine <- fmap reverse getLine
    putStrLn reversedLine

    -- (->) r is a functor
    -- explanation: r -> a <=> (->) r a
    -- fmap is function composition: fmap f . g = f `fmap` g = f . g where f and g are functions
    let f = fmap (* 3) (+ 100) -- f x = (100 + x) * 3
    print $ f 1

    -- fmap LIFTS (promotes) a function to a more abstract one
    -- by using fmpa we promote a (a -> b) function to a (f a -> f b) fonction

    -- functor laws:
    -- 1 : fmap id = id
    -- 2 : fmap (f . g) = fmap f . fmap g

    -- counter-example: CMaybe does not verify 1
    -- fmap id (CJust 0 1) = CJust 1 1 /= CJust 0 1
