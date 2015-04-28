removeAt :: Int -> [a] -> (a, [a])
removeAt n [] = error "Can't remove from an empty list"
removeAt n list@(x:xs)
  | n >= length list || n < 0 = error "out of range"
  | n == 0 = (x, xs)
  | otherwise = (fst next, x:snd next)
  where next = removeAt (n - 1) xs
