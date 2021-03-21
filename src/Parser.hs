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


parsePrettyParagraph :: (String -> Maybe String) -> (String -> Text) -> String -> (Maybe Text, String)
parsePrettyParagraph f g s = case f s of 
    Just "" -> (Nothing, s)
    Just (' ':x) -> (Nothing, s)
    Just x -> getPrettyParagraph x f g
    Nothing -> (Nothing, s)

getPrettyParagraph :: String -> (String -> Maybe String) -> (String -> Text) -> (Maybe Text, String)
getPrettyParagraph x f g = getPrettyParagraph' f g x ""

getPrettyParagraph' f g "" s = (Nothing, reverse s)
getPrettyParagraph' f g " " s = (Nothing, reverse s)
getPrettyParagraph' f g (' ':x) s = let h = head x 
                                        r = tail x
                    in
                        getPrettyParagraph' f g r (h:(' ':s))
getPrettyParagraph' f g x s = 
    case f x of 
        Just a -> (Just (g (reverse s)), a)
        Nothing -> let  h = head x 
                        r = tail x 
                    in
                        getPrettyParagraph' f g r (h:s)

parseBold :: String -> (Maybe Text, String)
parseBold = parsePrettyParagraph (stripPrefix "**") (\x -> Bold x)

parseItalic :: String -> (Maybe Text, String)
parseItalic = parsePrettyParagraph (stripPrefix "*") (\x -> Italic x)
