module Parser (parser) where
import Data.Char
import Utils
import AST

parser :: [String] -> Markdown 
parser (s:xs) = case firstChar s of 
    Just '#' -> case (parseHeader s) of 
        Just x -> x:(parser xs)
        Nothing -> let (x, y) = getWhileParagraph (s:xs)
                    in (parseParagraph x):(parser y)
    Just '*' -> let (list, rest) = getWhileList (s:xs)
                    in if length(list) == 0 
                        then parseRest (s:xs)
                        else 
                            (parseList list):(parser rest)
    Just x -> (parseRest (s:xs))
    Nothing -> parser xs
parser _ = []

parseHeader :: String -> Maybe Block
parseHeader x = 
    let n = countChar x '#'
        sps = countChar (drop n x) ' '
    in 
        if n < 7 && sps > 0
        then Just (Header (n, drop n x))
        else
            Nothing

parseList :: [String] -> Block
parseList x = List x

---parserRest :: [String] -> Markdown 
parseRest s = 
    let (x, y) = getWhileParagraph s 
    in (parseParagraph x) : (parser y)

---parseRest :: String -> Block
---parseRest s = Paragraph ([Normal s])

parseParagraph :: [String] -> Block
parseParagraph x = Paragraph (map (\x -> Normal x) x)