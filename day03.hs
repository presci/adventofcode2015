import Data.List (sort, group)
-- (r/l, u/d)
findhouse ::[Char] -> [(Int, Int)] -> [(Int, Int)]
findhouse [] a = a
findhouse ('>':xs) a@((m, n):_) = findhouse xs ( (m + 1, n) : a)
findhouse ('<':xs) a@((m, n):_) = findhouse xs ( (m - 1, n) : a)
findhouse ('v':xs) a@((m, n):_) = findhouse xs ( (m, n + 1) : a)
findhouse ('^':xs) a@((m, n):_) = findhouse xs ( (m, n - 1) : a)
findhouse (_:xs ) a = findhouse xs a



santa ::[Char] -> [(Int, Int)]-> [(Int, Int)] -> [(Int, Int)]
santa [] a b = merge a b
santa ('>':xs) a@((m, n):_) b = santa xs b ( (m + 1, n) : a)
santa ('<':xs) a@((m, n):_) b = santa xs b ( (m - 1, n) : a)
santa ('v':xs) a@((m, n):_) b = santa xs b ( (m, n + 1) : a)
santa ('^':xs) a@((m, n):_) b = santa xs b ( (m, n - 1) : a)
santa (_:xs ) a b = santa xs a b




merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge [] ys = ys
merge (x:xs) ys = x: merge ys xs

main::IO()
main = do
    contents <- readFile "sample.txt"
    let l = length . map head . group . sort $ findhouse contents [(0,0)]
    print l
    let j = length . map head . group . sort $ santa contents [(0,0)] [(0,0)]
    print j
