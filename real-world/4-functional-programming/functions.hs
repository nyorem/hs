-- split a list using a predicate
-- splits each time the predicate is false
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f (x:xs) =
    let (deb, fin) = break f (x:xs) in
        let (_, fin') = break (not . f) fin in
            deb : splitWith f fin'

-- transpose a list of lists
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose l = heads : transpose tails
    where tuples = [(x, xs) | (x:xs) <- l]
          heads = map fst tuples
          tails = map snd tuples
