myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:[]) = [x]
myReverse x = (last x):(myReverse (init x))
