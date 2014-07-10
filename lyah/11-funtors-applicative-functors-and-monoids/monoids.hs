module Main where

import Data.Monoid

-- monoid: binary function which serves as an associative intern law
--         neutral element
-- in Haskell, a monoid is typeclass which can be applied to only concrete types
-- it defines three functions:
-- -> mempty = neutral element
-- -> mappend = binary function
-- -> mconcat = foldr mappend mempty (reduces monoid values to a single one)
-- Monoid laws:
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

main :: IO ()
main = do
    -- examples of monoids:
    -- lists: mempty = [] and mappend = (++)

    -- numbers: two ways : (0, (+)) and (1, (*))
    -- two monoids (and two types) : Sum and Product
    -- getProduct . concat . map Product == product

    -- booleans: two ways (False, (||)) and (True, (&&))
    -- two monoids: Any and All
    -- getAny . concat . map Any == or

    -- Ordering

    return ()
