split' :: [a] -> Int -> ([a], [a])
split' [] n = ([], [])
split' list 0 = ([], list)
split' (x:xs) n =
  let next = split' xs (n - 1)
  in (x:fst next, snd next)
