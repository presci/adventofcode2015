import SudUtils
import qualified Data.Vector as V
import Data.Maybe
import Data.List (sort, group, foldl')
import Data.Maybe (Maybe(Nothing))
import Data.Int (Int)

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

solve02 :: Board -> Maybe Board
solve02 board = case findNextEmptyCell board of
        Just a -> case getCellAt a board of
            Just (Cells xss) -> helper a xss board
            _ -> error "done"
        _ -> Just board
    where
        helper :: (Int, Int) -> [Int] -> Board -> Maybe Board
        helper a xss brd = foldl' (tryValue a )  (Just brd) xss
        tryValue:: (Int, Int) -> Maybe Board -> Int -> Maybe Board
        tryValue a bd@(Just brd) val = if val `notElem` getskiplist a brd then solve02 (newbrd a brd val) else bd
        tryValue _ _ _ = Nothing
        newbrd ::(Int, Int) -> Board -> Int -> Board
        newbrd (x, y) brd val = brd V.// [(x, (brd V.! x) V.// [(y, Cell val)])]

test02:: Maybe Int -> Int -> Maybe Int
test02 (Just x) arg0 = if  arg0  >= x then Just arg0 else Nothing
test02 _ arg0 = Just arg0

test05::Board
test05 = conv2 . V.fromList $ map V.fromList [[0,0,0,0,0,6,0,0,8], -- 0,0,0,0,0,6,0,0,8  149536278
                                              [0,0,0,7,8,0,1,0,0], -- 0,0,0,7,8,0,1,0,0  526789134
                                              [0,0,3,0,0,0,0,0,5], -- 0,0,3,0,0,0,0,0,5  783241965
                                              [0,0,1,4,0,0,6,9,2], -- 0,0,1,4,0,0,6,9,2  851473692
                                              [2,3,7,6,0,5,0,0,0], -- 0,0,0,6,0,5,0,0,0  237695841
                                              [6,9,4,8,1,2,7,5,3], -- 6,9,4,0,0,2,7,0,0  694812753
                                              [4,6,8,1,5,7,3,2,9], -- 4,0,0,0,0,0,3,0,0  468157329
                                              [3,7,5,9,2,8,4,1,6]] -- 3,0,0,9,0,0,0,0,0  375928416



main::IO()
main = do
    let a =  solve02 . solve $ test05
    print a
    print "---------"
