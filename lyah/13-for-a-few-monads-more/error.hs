import Control.Monad.Except

-- the Either e a data type can be used like Maybe but with
-- some added information: if there is an error then it will
-- return a Left e (e will describe the failure), else a Right a
-- is returned.
-- so there is a Monad instance for Either
-- instance (Error e) => Monad (Either e) where
--    return = Right
--    (Right a) >>= f = f a
--    (Left err) >>= _ = Left err
--    fail msg = Left (strMsg msg)
-- e must be an instance of the Error typeclass (strMsg converts
-- an Error to a String): String is an instance of Error

-- same problem as 12 but here we use the Either monad to trace the number
-- of birds on each side of the pole when Pierre falls
type Birds = Int
type Pole = (Birds, Birds) -- left / right birds

pierreFalls :: Birds -> Birds -> Bool
pierreFalls left right = abs (left - right) > 3

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | pierreFalls (left + n) (right) = Left $ "left: " ++show left ++", right: " ++ show right
    | otherwise = Right (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | pierreFalls left (right + n) = Left $ "left: " ++show left ++", right: " ++ show right
    | otherwise = Right (left, right + n)

-- ex: return (0, 0) >>= landLeft 1 >>= landRight 2 >>= landRight 2

