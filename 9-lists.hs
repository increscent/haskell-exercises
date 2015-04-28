pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack full@(x:_) = let	consecutive_elements = consecutive full
			rest = drop (length consecutive_elements + 1) full
		in [x:consecutive_elements] ++ (pack rest)


consecutive :: (Eq a) => [a] -> [a]
consecutive [] = []
consecutive (x:[]) = []
consecutive full@(x:y:_)
	| x == y = 
		let rest = tail full
		in x:(consecutive rest)
	| otherwise = []
