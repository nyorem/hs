import Control.Monad

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
