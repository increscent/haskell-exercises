myLength :: [a] -> Int
myLength [] = 0			-- no elements
myLength (x:[]) = 1		-- one element
myLength (_:x) = myLength x + 1	-- multiple elements
