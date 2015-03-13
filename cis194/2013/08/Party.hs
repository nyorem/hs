{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Data.Monoid
import Data.List ( sortBy )
import Data.Function ( on )

import Employee

-- ex 1
-- | Add an employee to a guest list.
-- Pre-condition: the employee is not already in the guest list and
--                none of his superiors are in the list.
glCons :: Employee  -- ^ The employee to add
       -> GuestList -- ^ The input guest list
       -> GuestList -- ^ The output guest list
glCons e (GL es f) = GL (e : es) (f + empFun e)

-- Monoid instance for GuestList
-- mappend concatenate the employees and sum the funs
instance Monoid GuestList where
    mempty = GL [] 0
    (GL es1 f1) `mappend` (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

-- | Comparison of two gueslists : the more fun the better.
moreFun :: GuestList -- ^ First GuestList
        -> GuestList -- ^ Second GuestList
        -> GuestList -- ^ The GuestList with the greater amount of fun
moreFun gl1 gl2 =
    case gl1 `compare` gl2 of
        LT -> gl2
        GT -> gl1
        EQ -> gl1

-- ex 2
-- | Fold for Data.Trees.
treeFold :: (a -> [b] -> b) -- ^ The folding function
         -> Tree a          -- ^ The tree to fold
         -> b               -- ^ The folding result
treeFold f t = f lab (map (treeFold f) forests)
    where lab = rootLabel t
          forests = subForest t

-- ex 3
-- | Given an employee (the boss) and two GuestList representing
-- the best GuestLists (the greater amount of fun) with the boss of the
-- left and right subtrees, it will returns the best GuestLists with and without
-- the boss passed as an argument.
nextLevel :: Employee                 -- ^ The boss
          -> [(GuestList, GuestList)] -- ^ Current lists
          -> (GuestList, GuestList)   -- ^ Updated lists
nextLevel e glPairs = (with, without)
    where with = glCons e (mconcat $ map snd glPairs)
          without = mconcat $ map (uncurry moreFun) glPairs

-- ex 4
-- | Finds the GuestList with the greatest amount of fun
-- in a compnay represented by a Tree.
maxFun :: Tree Employee -- ^ A company
       -> GuestList     -- ^ The best GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- | Gets the first name of an employee.
-- We suppose that the name of an employee
-- follows the pattern: 'first_name last_name'
firstname :: Employee -- ^ An employee
          -> String   -- ^ The corresponding first name
firstname = head . words . empName

-- | Convert a GuestList to a String (not the same as Show).
-- A GuestList is represented by the total amount of fun and
-- the sorted (by first name) lists of employees.
showGL :: GuestList -- ^ A GuestList
       -> String    -- ^ The corresponding string representation
showGL (GL es f) =
    unwords ["Total fun:", show f, "\n"] ++ unlines (map empName sortedEmpNames)
          where sortedEmpNames = sortBy (compare `on` firstname) es

