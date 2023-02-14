module Main where
import Data.List (group, intercalate)

-- length $ head . drop 40 $ iterate  makesq "1113122113"

makesq :: String -> String
makesq xs =  intercalate "" $ makesq1 $  group xs
    where
        makesq1 :: [String] -> [String]
        makesq1 [] = []
        makesq1 (x:xs) = merge1 [head x] (show $ length x) : makesq1 xs

intercalate1 :: [String] -> String
intercalate1 = intercalate ""


merge1::String -> String -> String
merge1 = foldr (:)
