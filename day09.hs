import Text.Parsec
import Data.List (group, sort, permutations)
import Data.ByteString (hPutStrLn)
import Distribution.Simple.Utils (xargs)
import System.Directory.Internal.Prelude (exitFailure)
data Cities = Cities String String Int deriving (Show)
{-- sample.txt file
Faerun to Norrath = 129
Faerun to Tristram = 58
Faerun to AlphaCentauri = 13
Faerun to Arbre = 24
Faerun to Snowdin = 60
Faerun to Tambi = 71
Faerun to Straylight = 67
Norrath to Tristram = 142
Norrath to AlphaCentauri = 15
Norrath to Arbre = 135
Norrath to Snowdin = 75
Norrath to Tambi = 82
Norrath to Straylight = 54
Tristram to AlphaCentauri = 118
Tristram to Arbre = 122
Tristram to Snowdin = 103
Tristram to Tambi = 49
Tristram to Straylight = 97
AlphaCentauri to Arbre = 116
AlphaCentauri to Snowdin = 12
AlphaCentauri to Tambi = 18
AlphaCentauri to Straylight = 91
Arbre to Snowdin = 129
Arbre to Tambi = 53
Arbre to Straylight = 40
Snowdin to Tambi = 15
Snowdin to Straylight = 99
Tambi to Straylight = 70
--}
-- parses the string to Cities
p::Parsec String () Cities
p = do
    fromCity <- many1 letter
    spaces
    string "to"
    spaces
    toCity <- many1 letter
    spaces
    char '='
    spaces
    distance <- many1 digit
    return $ Cities fromCity toCity (read distance::Int)

-- gets the right values
rights :: [Either a b] -> [b]
rights [] = []
rights (Right a: xs) = a: rights xs
rights (_: xs) = rights xs

-- get the unique Cities
uniqCities:: [Cities] -> [String]
uniqCities [] = []
uniqCities xv = map head . group .  sort $ uniqCities2 xv
    where
        uniqCities2 :: [Cities] -> [String]
        uniqCities2 [] = []
        uniqCities2 (Cities a b _:xs) = a:b:uniqCities2 xs

-- Gets the cost between the cities
getCityCost :: [Cities] -> String -> String -> Maybe Int
getCityCost [] _ _ = Nothing
getCityCost (Cities a b c: xss) g v =
    if a == g && b == v || a == v && b == g
        then Just c
        else getCityCost xss g v

-- Gets the cost between list of cities
getCitiesCost :: [Cities] -> [String] -> [Maybe Int]
getCitiesCost cities (x:b:xss) = getCityCost cities x b : getCitiesCost cities (b:xss)
getCitiesCost _ _ = []

-- sum1 adds all maybe
sum1 ::[Maybe Int] -> Int
sum1 [] = 0
sum1 (Just a : xss) = a + sum1 xss
sum1 (_:xss) = 0 + sum1 xss


main::IO()
main = do
    content <- lines <$> getContents
    let cities =  rights $  map (parse p "" )  content
    let perms = permutations . uniqCities $ cities
    -- get minimum value
    print .  minimum $ map (sum1 . getCitiesCost cities) perms
    -- get maximum value
    print .  maximum $ map (sum1 . getCitiesCost cities) perms
