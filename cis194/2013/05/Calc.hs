{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExprT
import Parser
import qualified StackVM as S

import Control.Applicative
import qualified Data.Map as M

-- ex 3
class Expr a where
    -- litteral
    lit :: Integer -> a

    -- addition
    add :: a -> a -> a

    -- multiplication
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- | Constrains the type to ExprT.
reify :: ExprT -> ExprT
reify = id

-- ex 1
-- | An evaluator for ExprT.
eval :: ExprT -- ^ The expression to evaluate
     -> Integer -- ^ The resulting evaluated expression
eval (Lit i) = i
eval (Add e e') = (eval e) + (eval e')
eval (Mul e e') = (eval e) * (eval e')

-- ex 2
-- other way to do it: evalStr = fmap eval . parseExp Lit Add Mul
-- or: evalStr = eval <$> parseExp Lit Add Mul
-- | Parses and evaluates an expression.
evalStr :: String        -- ^ The string to evaluate
        -> Maybe Integer -- ^ Maybe the result
evalStr str =
    case parseExp Lit Add Mul str of
        Just expr -> Just (eval expr)
        Nothing -> Nothing

-- ex 4
-- Integer
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

-- Bool
instance Expr Bool where
    lit = (> 0)
    add = (&&)
    mul = (||)

-- MinMax
newtype MinMax = MinMax Integer deriving (Show, Eq)
instance Expr MinMax where
    lit i = MinMax i
    add (MinMax i) (MinMax i') = MinMax (min i i')
    mul (MinMax i) (MinMax i') = MinMax (max i i')

-- Mod7
newtype Mod7 = Mod7 Integer deriving (Show, Eq)
instance Expr Mod7 where
    lit i = Mod7 (i `mod` 7)
    add (Mod7 i) (Mod7 i') = Mod7 ((i + i') `mod` 7)
    mul (Mod7 i) (Mod7 i') = Mod7 ((i * i') `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- ex 5
instance Expr S.Program where
    lit i = [S.PushI i]
    add p1 p2 = p1 ++ p2 ++ [S.Add]
    mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String
        -> Maybe S.Program
compile = parseExp lit add mul

exec :: String
     -> Maybe (Either String S.StackVal)
exec str = S.stackVM <$> compile str

-- ex 6
class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit i = (\_ -> Just i)
    add e1 e2 = (\m -> case (e1 m, e2 m) of
                                (Just x, Just y) -> Just (x + y)
                                _ -> Nothing)
    mul e1 e2 = (\m -> case (e1 m, e2 m) of
                                (Just x, Just y) -> Just (x * y)
                                _ -> Nothing)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

