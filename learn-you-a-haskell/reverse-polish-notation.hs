main = do
  line <- getLine
  if (null line)
    then
      return ()
    else do
      putStrLn $ show $ rpn line
      main

rpn :: String -> Float
rpn expression = compute (words expression) []

compute :: [String] -> [Float] -> Float
compute [] stack = head stack
compute (x:xs) stack = case x `elem` ["+", "-", "*", "/"] of
  True -> compute xs $ ((getFunction x) second first):rest
  False -> compute xs $ (read x):stack
  where getFunction "+" = (+)
        getFunction "-" = (-)
        getFunction "*" = (*)
        getFunction "/" = (/)
        first = head stack
        second = head $ tail stack
        rest = drop 2 stack
