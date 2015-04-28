secondToLast :: [a] -> a
secondToLast [] = error "not enough items"
secondToLast (x:[]) = error "not enough items"
secondToLast (x:y:[]) = x
secondToLast (_:x) = secondToLast x

secondToLast' :: [a] -> a
secondToLast' x = if (length x >= 2 )
	then (x !! (length x - 2))
	else error "not enough items"
