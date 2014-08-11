import Control.Monad.State
import System.Random

-- the State Monad is for dealing with stateful computations
-- a stateful computation has the following type: s -> (a, s)
-- where 's' is the type of the state and 'a' the result of the computation

-- we can think as assignment as a stateful computation:
-- we take a set of all the assigned variables and updates it with the newly
-- assigned variable and we return the corresponding expression

-- Stack representation without State
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x:xs)

-- pop and push x are stateful computations

-- example of stack manipulation: stateful computation
-- tedious code because we have to pass the stack in each line
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), stack') = push 4 stack
    (a, stack'') = pop stack'
    in pop stack''

-- State Monad
-- newtype State s a = State {runState :: s -> (a, s)}
-- instance Monad (State s) where
--     return x = State $ \s -> (x, s)
--     (Stack h) >>= f = State $ \s -> let (a, newState) = h s
--                                         (State g) = f a
--                                     in g newState

-- pop and push using State
pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs)

push' :: Int -> State Stack ()
push' x = state $ \xs -> ((), x:xs)

stackManip' :: State Stack Int
stackManip' = do
    push' 4
    a <- pop'
    pop'

-- ex: runState stackManip' [3, 4, 5]

-- Monad.State is part of the MonadState typeclass
-- it provides: get, put
-- presents the current state as the result
-- get = State $ \s -> (s, s)
-- puts a new state
-- put newState = State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1, 2, 3]
    then
        put [2, 3, 4]
    else
        put [3, 4, 5]

-- actually, we can implement randomness using State because
-- the type of random is (RandomGen g, Random a) => g -> (a, g)
-- so, retrieving random numbers is a stateful computation
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

