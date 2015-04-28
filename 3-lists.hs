nth :: [a] -> Int -> a
nth x y = if (y < 0 || y >= (length x))
	then error ("index out of range: " ++ (show y))
	else x !! y
