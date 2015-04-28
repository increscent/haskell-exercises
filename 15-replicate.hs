replicate' :: Int -> [a] -> [a]
replicate' n [] = []
replicate' n (x:xs) = (replicate n x) ++ (replicate' n xs)
