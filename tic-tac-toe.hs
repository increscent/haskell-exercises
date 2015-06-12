import qualified Data.Sequence as S
import qualified Data.Foldable as F

type Board = S.Seq (S.Seq Int)

-- 0 = empty
-- 1 = x
-- 2 = x


-- Starting board:
-- 0 0 0
-- 0 0 0
-- 0 0 0
--
bOARD_SIZE :: Int
bOARD_SIZE = 3

startingBoard :: Board
startingBoard = S.replicate bOARD_SIZE $ S.replicate bOARD_SIZE 0

turn :: Int -> (Int, Int) -> Board -> IO (Maybe Board)
turn player (x, y) board
  | x > size || y > size = return Nothing -- out of range
  | value > 0 = return Nothing -- already set
  | otherwise = return $ Just (S.update x newRow board)
  where size = S.length board - 1
        value = S.index (S.index board x) y
        newRow = S.update y player (S.index board x)

boardRows :: Board -> [[Int]]
boardRows = F.toList . fmap F.toList

--boardRow :: Int -> Board -> [Int]
--boardRow 0 (row:rows) = row
--boardRow x (row:rows) = boardRow (x - 1) rows
--
--boardCol :: Int -> Board -> [Int]
--boardCol y board = (everyNth (y, length board)) $ concat board
--
--boardDiag :: Bool -> Board -> [Int]
--boardDiag topToBottom board =
--  let (startY, modifier) = if (topToBottom) then (0, (+1)) else (2, (subtract 1))
--  in  fst $ foldl (\(acc, y) row -> (acc ++ [(row !! y)], modifier y)) ([], startY) board
--
--everyNth :: (Int, Int) -> [Int] -> [Int]
--everyNth _ [] = []
--everyNth (start, n) (x:xs)
--  | start == 0 = x:everyNth (n - 1, n) xs
--  | otherwise = everyNth (start - 1, n) xs
