module SudUtils(module SudUtils) where


import qualified Data.Vector as V
import Data.Maybe
import Data.List (sort, group)


-- sudoku
data Cell = Cell Int | Cells [Int] deriving Show
type Board = V.Vector (V.Vector Cell)

conv2 :: V.Vector (V.Vector Int) -> Board
conv2 = V.map conv1

conv1 :: V.Vector Int -> V.Vector Cell
conv1 = V.map (\x -> if x ==0 then Cells [1,2,3,4,5,6,7,8,9] else Cell x )

isCell :: Cell -> Bool
isCell (Cell _) = True
isCell _ = False

isCells :: Cell -> Bool
isCells (Cells _) = True
isCells _ = False



getskiplist :: (Int, Int) -> Board -> [Int]
getskiplist a@(x,y) vec = let rw = getrow x vec
                              col = getcol y vec
                              sqr = getsquare a vec
                            in map head . group . sort $ concat [rw, col, sqr]


getsquare :: (Int,Int) -> Board -> [Int]
getsquare (rw,col) vec = getvalues . filter isCell $ mapMaybe get sdsqr
    where
    m = 3 * (rw `div` 3)
    n = 3 * (col `div` 3)
    sdsqr:: [(Int, Int)]
    sdsqr = let x = 3 * (rw `div` 3)
                y = 3 * (col `div` 3)
                in surroundingsquare (x, y)
    get::(Int, Int) -> Maybe Cell
    get (x,y) = do
        row <- vec V.!? x
        row V.!? y
    getvalues :: [Cell] -> [Int]
    getvalues = getvalues' []
        where
            getvalues' a [] = a
            getvalues' a ((Cell x ) : xss) = getvalues' (x: a) xss
            getValues' a (_: xss) = getvalues' a xss
    surroundingsquare :: (Int, Int) -> [(Int,Int)]
    surroundingsquare (m,n) = map (\(x, y) -> (m+ x, n + y)) [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)]




getrow :: Int -> V.Vector (V.Vector Cell) -> [Int]
getrow k vec = case vec V.!? k of
    Just x ->  getSingleCell x
    _ -> []

getcol :: Int -> V.Vector (V.Vector Cell) -> [Int]
getcol k vec = getSingleCell $ V.map (V.! k) vec


getSingleCell :: V.Vector Cell -> [Int]
getSingleCell v = getSingleCell' [] (V.toList v)
    where
        getSingleCell' ::[Int] -> [Cell] -> [Int]
        getSingleCell' acc [] = acc
        getSingleCell' acc ((Cell x): xss) = getSingleCell' (x:acc) xss
        getSingleCell' acc (_: xss) = getSingleCell' acc xss


findNextEmptyCell :: Board -> Maybe (Int, Int)
findNextEmptyCell = V.ifoldl' findCell Nothing
    where
        findCell :: Maybe (Int, Int) -> Int -> V.Vector Cell -> Maybe (Int, Int)
        findCell a@(Just x) _  _ = a
        findCell _ rw row = case V.findIndex isCells row of
            Just col -> Just (rw, col)
            _ -> Nothing
