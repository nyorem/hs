module Scrabble where

import Data.Monoid
import Data.Char ( toLower )

-- ex 3
newtype Score = Score {getScore :: Int} deriving (Show)

-- Monoid instance for Score ~ Sum Int
instance Monoid Score where
    mempty = Score 0
    (Score x) `mappend` (Score y) = Score (x + y)

-- | Scrabble score of a character
-- See: http://www.thepixiepit.co.uk/scrabble/rules.html
score :: Char  -- ^ A character
      -> Score -- ^ The corresponding Scrabble score
score c
    | cc `elem` "aeilnorstu" = Score 1
    | cc `elem` "dg" = Score 2
    | cc `elem` "bcmp" = Score 3
    | cc `elem` "fhvwy" = Score 4
    | cc `elem` "k" = Score 5
    | cc `elem` "jx" = Score 8
    | cc `elem` "qz" = Score 10
    | otherwise = Score 0
        where cc = toLower c

-- | Scrabble score of a word.
scoreString :: String -- ^ A string
            -> Score  -- ^ The corresponding Scrabble score
scoreString = mconcat . map score

