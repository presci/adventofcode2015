
import Text.Parsec
import Data.Text.Internal.Builder.Int.Digits (digits)
data Reindeer = Reindeer String Int Int Int deriving (Show)

rounds::  Int -> Reindeer -> Int
rounds  cy (Reindeer name distance secs rest)= sum . take cy . cycle $ replicate secs distance ++ replicate rest 0



parseDeer :: Parsec String () Reindeer
parseDeer = do
    name <- many1 letter
    string " can fly "
    distance <- many1 digit
    string " km/s for "
    secs <- many1 digit
    string " seconds, but then must rest for "
    rest <- many1 digit
    string " seconds."
    return $ Reindeer name (read distance::Int) (read secs::Int) (read rest::Int)


rights::[Either a b] -> [b]
rights [] = []
rights (Right v:xss) = v : rights xss
rights (_: xss) = rights xss

main::IO()
main = do
    puzzle <- lines <$> readFile "sample.txt"
    print . maximum . map ( rounds 2503) .  rights $ map (parse parseDeer "") puzzle
