module Parser (parser) where
import Data.Char
import Data.List
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

parseRest s = 
    let (x, y) = getWhileParagraph s 
    in (parseParagraph x) : (parser y)

parseParagraph :: [String] -> Block
parseParagraph x = Paragraph (concat (map (\x -> parseParagraph2 x) x))

parseParagraph2 :: String -> [Text]
parseParagraph2 x = parseParagraph2' x ""

parseParagraph2' "" acc = parseParagraph3' (reverse acc) ""
parseParagraph2' x acc = 
    case parseBold x of 
        (Just x, y) -> ((parseParagraph3' (reverse acc) "") ++ (x:(parseParagraph2' y "")))
        (Nothing, _) -> 
            let h = head x 
                r = tail x 
            in 
                parseParagraph2' r (h:acc)

parseParagraph3' "" acc = [Normal (reverse acc)]
parseParagraph3' x acc = 
    case parseItalic x of 
        (Just x, y) -> ((Normal (reverse acc)):(x:(parseParagraph3' y "")))
        (Nothing, _) -> 
            let h = head x 
                r = tail x 
            in 
                parseParagraph3' r (h:acc)

parseBold :: String -> (Maybe Text, String)
parseBold s = case stripPrefix "**" s of 
    Just "" -> (Nothing, s)
    Just (' ':x) -> (Nothing, s)
    Just x -> getBold x 
    Nothing -> (Nothing, s)


getBold :: String -> (Maybe Text, String)
getBold x = getBold' x ""

getBold' "" s = (Nothing, reverse s)
getBold' " " s = (Nothing, reverse s)
getBold' (' ':x) s = let  h = head x 
                          r = tail x 
                    in
                        getBold' r (h:(' ':s))
getBold' x s = 
    case stripPrefix "**" x of 
        Just a -> (Just (Bold (reverse s)), a)
        Nothing -> let  h = head x 
                        r = tail x 
                    in
                        getBold' r (h:s)

parseItalic :: String -> (Maybe Text, String)
parseItalic s = case stripPrefix "*" s of 
    Just "" -> (Nothing, s)
    Just (' ':x) -> (Nothing, s)
    Just x -> getItalic x 
    Nothing -> (Nothing, s)


getItalic :: String -> (Maybe Text, String)
getItalic x = getItalic' x ""

getItalic' "" s = (Nothing, reverse s)
getItalic' " " s = (Nothing, reverse s)
getItalic' (' ':x) s = let  h = head x 
                            r = tail x 
                    in
                        getItalic' r (h:(' ':s))
getItalic' x s = 
    case stripPrefix "*" x of 
        Just a -> (Just (Italic (reverse s)), a)
        Nothing -> let  h = head x 
                        r = tail x 
                    in
                        getItalic' r (h:s)



