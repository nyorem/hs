{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

import Data.List ( sortBy )
import Data.Function ( on )
import Control.Monad ( unless )

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- ex 2

-- | Simulate the roll of n dice.
dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

-- | Maximum number of units that an attacker can use.
attackerUnits :: Army -> Army
attackerUnits army
    | army > 3 = 3
    | otherwise = army - 1

-- | Maximum number of units that an defender can use.
defenderUnits :: Army -> Army
defenderUnits army
    | army > 2 = 2
    | otherwise = army - 1

-- | Sorts a lists of dice in decreasing order.
sortDice :: [DieValue] -> [DieValue]
sortDice = reverse . sortBy (compare `on` unDV)

-- | Simulates a single battle of Risk.
battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
    -- Gets the number of units used by both sides
    let aUnits = attackerUnits $ attackers b
        dUnits = defenderUnits $ defenders b

    -- Rolls and sorts the dice
    aDice <- fmap sortDice $ dice aUnits
    dDice <- fmap sortDice $ dice dUnits

    -- Matches up in pairs
    let dicePairs = zip aDice dDice
        lostDUnits = length $ filter (\(a, b) -> a > b) dicePairs
        lostAUnits = (length dicePairs) - lostDUnits

    -- Computes the new battlefied
    return $ Battlefield ((attackers b) - lostAUnits) ((defenders b) - lostDUnits)

-- ex 3
-- | Predicate to tell when an invasion is over.
noMoreBattle :: Battlefield -> Bool
noMoreBattle b = (attackers b) <= 2 || (defenders b) == 0

-- | Keeps battle until there are no defenders remaining or fewer than two attackers.
invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
    bb <- battle b
    if noMoreBattle bb then return bb
    else invade bb

-- ex 4

