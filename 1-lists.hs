myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' [] = error "empty list"
myLast' x = x !! (length x - 1)
