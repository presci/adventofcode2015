import SudUtils

import qualified Data.Vector as V
import Data.Maybe
import Data.List (sort, group)
-- / end initialization code


solve :: Board -> Board
solve vec = V.imap update vec
    where
        update :: Int -> V.Vector Cell -> V.Vector Cell
        update rw vec1 = V.imap (updateCell rw ) vec1
        updateCell :: Int -> Int -> Cell -> Cell
        updateCell _ _ a@(Cell _) = a
        updateCell rw col (Cells x) = case filter (`notElem` getskiplist (rw,col) vec ) x of
            [x] -> Cell x
            a -> Cells a


getCellAt::(Int, Int) -> Board -> Maybe Cell
getCellAt (x, y) board = do
    row <- board V.!? x
    row V.!? y

solve02 :: Board -> Board
solve02 board = let emptycell =  findNextEmptyCell board
    in helper emptycell
    where
        helper :: Maybe (Int, Int) -> Board
        helper (Just coord@(x, y)) = case getCellAt coord board of
                                    (Just (Cells xss)) -> isValid coord xss board
                                    _ -> board
        helper _ = board
        isValid::(Int, Int) -> [Int] -> Board -> Board
        isValid _ [] brd = brd
        isValid a@(x1, y1) (x:xss) brd = if x `notElem` getskiplist a brd then solve02 (brd V.// [(x1, (brd V.! x1) V.// [(y1, Cell x)])]) else isValid a xss brd

test05::Board
test05 = conv2 . V.fromList $ map V.fromList [[8,2,5,4,7,1,3,9,6],
                                              [0,0,0,3,2,6,5,7,8],
                                              [0,0,0,9,8,5,2,4,1],
                                              [0,0,0,7,4,3,8,0,2],
                                              [0,0,0,5,9,8,4,0,7],  -- remove 9 here and it fails
                                              [0,0,0,6,0,2,0,3,5],
                                              [2,6,3,1,5,0,0,8,0],
                                              [9,4,8,2,0,7,0,0,3],
                                              [0,0,0,8,0,4,0,2,9]]
main::IO()
main = do
    let a =   solve $ test05
    print a
    print "---------"
    let g =  solve02 a
    print g
