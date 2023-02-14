import Language.Haskell.TH (javaScript)


parse::[Char] -> [Int]
parse [] = [0]
parse ('\\':'x':_:_:xss) = 1 : parse xss
parse ('\\':'\\':xss) = 1: parse xss
parse ('\\':'"':xss) = 1: parse xss
parse ('"':xss) = parse xss
parse (_:xss) = 1 : parse xss

parse02::[Char] -> [Int]
parse02 [] = [0]
parse02 ('\\':'x':_:_:xss) = 1 : parse02 xss
parse02 ('\\':'\\':xss) = 1: parse02 xss
parse02 ('\\':'"':xss) = 1: parse02 xss
parse02 ('"':xss) = parse02 xss
parse02 (_:xss) = 1 : parse02 xss

main::IO()
main = do
    content <- readFile "sample.txt"
    let j = lines content
    let l =  sum . map (sum . parse) $  j
    let k = sum . map length $  j
    print ((-) k l)
