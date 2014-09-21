-- set: 'list' but each element is unique and ordered
import qualified Data.Set as S

-- find comon characters ?
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

-- fromList : conversion between a list and a set / toList
set1 = S.fromList text1
set2 = S.fromList text2

-- intersection / union / difference
common = set1 `S.intersection` set2
in1ButNotIn2 = set1 `S.difference` set2

-- insert / delete / size / singleton / null / member / empty

-- subsets: isSubsetOf

-- map and filter

-- toList . fromList == nub but nub requires Eq wherees Set requires Ord
-- first one faster on big lists but does not preserve order

