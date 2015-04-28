runLengthEncoding :: (Eq a) => [a] -> [(Int, a)]
runLengthEncoding (x:xs) =
	let	count = length (takeWhile (== x) xs)
		rest = drop count xs
	in	[(count + 1, x)] ++ runLengthEncoding rest
runLengthEncoding [] = []
