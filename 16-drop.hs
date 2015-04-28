dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery list n = take (n - 1) list ++ dropEvery (drop n list) n
