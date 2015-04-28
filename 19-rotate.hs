rotate :: [a] -> Int -> [a]
rotate list n =
  let len = length list
      offset  = if (n < 0) then (len - (mod (abs n) len)) else (mod n len)
  in drop offset list ++ take offset list
