import Control.Monad
import Control.Applicative
import Data.List

type Moves = [(Int, Int, Char)]
type Board = [[Char]]

boardSize = 3

main = do play 'x' []

play :: Char -> Moves -> IO ()
play player moves = do
                      putStrLn $ player:": enter the coordinates of your move (comma delimited): "
                      (x, y) <- (\(x, y) -> (read x :: Int, read (tail y) :: Int)) . break (== ',') <$> getLine
                      case (turn player (x, y) moves) of
                        Nothing -> do
                          putStrLn "Try again"
                          play player moves
                        Just newMoves -> do
                          putStrLn $ listBoard $ showBoard newMoves
                          if (checkWin player newMoves)
                            then (putStrLn (player:" Wins!"))
                            else (play (nextPlayer player) newMoves)

nextPlayer :: Char -> Char
nextPlayer 'x' = 'o'
nextPlayer 'o' = 'x'

showBoard :: Moves -> Board
showBoard moves = map (\row -> map (\col -> generateCell (col, row)) [0..limit]) [0..limit]
  where limit = boardSize - 1
        generateCell (x, y) =
          case (boardElem (x, y) moves) of
            Nothing -> '-'
            Just a -> a 

listBoard :: Board -> String
listBoard board = concat $ intersperse "\n" $ map (intersperse ' ') board

turn :: Char -> (Int, Int) -> Moves -> Maybe Moves
turn player (x, y) moves =
    let limit = boardSize - 1
    -- check if coordinate is out of range
    in  if (x > limit || y > limit)
      then (Nothing)
      else (
        -- check if coordinate has already been used
        case (boardElem (x, y) moves) of
          Nothing -> Just ((x, y, player):moves)
          _       -> Nothing
      )

winCoords :: [[(Int, Int)]]
winCoords =
  let limit = boardSize - 1
      horizontal = map (\x -> zip (repeat x) [0..2]) [0..limit]
      vertical = map (\y -> zip [0..2] (repeat y)) [0..limit]
      diagonal = map (\xs -> zip xs [0..limit]) [[0..limit],[limit,(limit - 1)..0]]
  in  horizontal ++ vertical ++ diagonal

checkWin :: Char -> Moves -> Bool
checkWin player moves =
  let playerMoves = map (\(x, y, _) -> (x, y)) $ filter (\(_, _, p) -> p == player) moves
  in  foldl (\acc xs -> acc || foldl (\acc x -> acc && x `elem` playerMoves) True xs) False winCoords

boardElem :: (Int, Int) -> Moves -> Maybe Char
boardElem (x, y) moves = foldl (boardMatch (x, y)) Nothing moves

boardMatch :: (Int, Int) -> Maybe Char -> (Int, Int, Char) -> Maybe Char
boardMatch (x, y) Nothing (row, col, player) = if ((row, col) == (x, y)) then (Just player) else (Nothing)
boardMatch _ match _ = match
