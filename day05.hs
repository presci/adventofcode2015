import Text.XHtml (base)
import Data.Time.Format.ISO8601 (yearFormat)
-- day 5

test01 :: [Char] -> [Char] -> Int -> Bool -> Bool
test01 [] _ a k =  a > 2 &&  k
test01 ('b':_) ('a':_) _ _  =  False
test01 ('d':_) ('c':_)  _ _  =  False
test01 ('q':_) ('p':_) _ _ =  False
test01 ('y':_) ('x':_) _ _ =  False
test01 (x:xs) b@(y:ys) ct vd = test01 xs (x:b) ctx vdx
    where
        ctx = if x `elem` "aieou" then ct + 1 else ct
        vdx =  if not vd then x == y else vd
test01 (x:xs) _ _ _ = test01 xs [x] l False
    where
        l = if x `elem` "aieou" then 1 else 0

day50 :: [Char] -> Bool
day50 x = test01 x [] 0 False


test03 :: [Char] -> Bool -> Bool  -> Bool
test03 [] _ _ = False
test03 _  True True = True
test03 (x:y:xss) dou ata = test03 (y:xss) (doux xss) atax -- (doux xss)
    where
        doux :: [Char] -> Bool
        doux [] = False
        doux (v:vxx)= dou || (==) x v && (/=) x y
        atax = ata || findxy xss
        findxy :: [Char] -> Bool
        findxy (yx:yy:yxx) = x == yx && y == yy || findxy (yy:yxx)
        findxy _ = False
test03 _ _ _= False

test02 :: [Char] -> Bool
test02 xss = test03 xss False False


main :: IO()
main = do
    contents <- readFile "sample.txt"
    let k =   length . filter (== True) . map test02 $ lines contents
    print k
