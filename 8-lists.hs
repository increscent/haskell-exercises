removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:[]) = [x]
removeDuplicates full@(x:y:_)
	| x == y = removeDuplicates rest
	| otherwise = x:(removeDuplicates rest)
	where rest = tail full
