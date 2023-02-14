import qualified Data.List as L
import Distribution.PackageDescription (CondTree(condTreeComponents))

getArea :: [Int] -> Int
getArea [_,_] = 0
getArea [_] = 0
getArea [] = 0
getArea (l:w:h:_) = (2 * a) + (2 * b) + (2 * c) + d
    where   a = w * h
            b = l * h
            c = l * w
            d = minimum [a, b, c]

ribbons :: [Int] -> Int
ribbons [_,_] =0
ribbons [_] =0
ribbons [] = 0
ribbons k = findlength (L.sort k)
    where
        findlength :: [Int] -> Int
        findlength (x:y:z:_) = x + x + y + y + (x * y * z)
        findlength _ = 0

parse :: [String] -> [Int]
parse =  map (read :: String -> Int)


split :: String  -> [String]
split [] = [""]
split (c:cs)
    | c == 'x' = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs





main::IO()
main = do
    contents <- lines <$> readFile "sample.txt"
    let k = sum . map (getArea . parse . split )  $  contents
    print k
    let l = sum . map (ribbons . parse . split ) $  contents
    print l
