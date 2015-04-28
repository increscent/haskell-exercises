slice' :: [a] -> Int -> Int -> [a]
slice' [] n m = []
slice' list@(x:xs) n m
  | m == 0 = []
  | n <= 1 = x:slice' xs n (m - 1)
  | otherwise = slice' xs (n - 1) (m - 1)
