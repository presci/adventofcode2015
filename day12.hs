parsefile ::String -> String
parsefile [] = []
parsefile ('-':x:xs) = if x `elem` "0123456789" then '-':parsefile (x:xs) else ' ':parsefile (x:xs)
parsefile (x:xs) = if x `elem` "0123456789" then x:parsefile xs else ' ':parsefile xs



main::IO()
main = do
    content <- readFile "sample.txt"
    let g = foldr ((+) . (read :: String -> Int)) 0 . words $ parsefile content
    print g
