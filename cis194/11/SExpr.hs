{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char ( isSpace,
                   isAlpha,
                   isAlphaNum )

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- | Parses zero or more things.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- | Parses one or more things.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- | Parses zero or more whitespaces.
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- | Parses an identifier.
-- An identifier is an alphabetic character followed by zero or more
-- alphanumeric characters.
ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- | Parses S expressions.
parseSExpr :: Parser SExpr
parseSExpr = spaces *>
            (
                A <$> parseAtom <|>
                    (
                        Comb <$> (char '(' *> (oneOrMore parseSExpr) <* char ')')
                    )
            )
            <* spaces
                -- Parses an atom.
                where parseAtom :: Parser Atom
                      parseAtom = (N <$> posInt) <|> (I <$> ident)

