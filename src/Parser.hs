module Parser (parser) where
import Data.Char
import Utils
import AST

parser :: [String] -> Markdown 
parser (s:xs) = case firstChar s of 
    Just '#' -> (parseHeader s):(parser xs)
    Just '*' -> let (list, rest) = getWhileList (s:xs)
                    in if length(list) == 0 
                        then (parseRest s):(parser xs)
                        else (parseList list):(parser rest)
    Just x -> (parseRest s):(parser xs)
    Nothing -> parser xs
parser _ = []

parseHeader :: String -> Block
parseHeader x = 
    let n = countChar x '#'
        sps = countChar (drop n x) ' '
    in 
        if n < 7 && sps > 0
        then Header (n, drop n x)
        else
            parseRest x

parseList :: [String] -> Block
parseList x = List x

parseRest :: String -> Block
parseRest s = Paragraph ([Normal s])