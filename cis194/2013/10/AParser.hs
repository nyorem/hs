{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- ex 1
-- | Applies a function to only the first component of a pair.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

-- Functor instance for Parser
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser g) = Parser h
    -- f :: a -> b
    -- g :: String -> Maybe (a, String)
    -- h :: String -> Maybe (b, String)
        where h = fmap (first f) . g

-- ex 2
-- Applicative instance for Parser
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser (\xs -> Just (x, xs))

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2 = Parser $ \s ->
        case runParser p1 s of
            Nothing -> Nothing
            Just (f, s') -> runParser (f <$> p2) s'

-- ex 3
-- | Expects 'a' followes by 'b'.
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- | Same thing but returns () in case of success.
abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

-- | Expects 'integer integer'
intPair :: Parser [Integer]
intPair = (\x y z -> [x, z]) <$> posInt <*> char ' ' <*> posInt

-- ex 4
-- Alternative instance for Parser
instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (const Nothing)

    -- <|> :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser (\xs -> runParser p1 xs <|> runParser p2 xs)

-- ex 5
-- | Expects an integer or an uppercase character.
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)

