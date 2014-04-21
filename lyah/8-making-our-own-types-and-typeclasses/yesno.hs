-- defining a typeclass
-- yesno : things that can be considered as true or false (like empty strings for e.g)
class YesNo a where
	yesno :: a -> Bool

-- now defining some instances
instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id
	-- id is the identity function

instance YesNo (Maybe a) where
	yesno Nothing = False
	yesno (Just _) = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesNoVal yesResult noResult = if yesno yesNoVal then yesResult else noResult
