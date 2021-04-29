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

header :: Int -> String -> Block 
header n s = Header (n, s)

list :: [String] -> Block
list s = List s 

paragraph :: [Text] -> Block
paragraph s = Paragraph s

bold :: String -> Text 
bold s = Bold s 

italic :: String -> Text 
italic s = Italic s

normal :: String -> Text 
normal s = Normal s

combineList :: Block -> Block -> Block
combineList (List x) (List y) = List (x ++ y)
combineList _ _ = Unit

combineParagraph :: [Text] -> [Text] -> Block 
combineParagraph x y =  Paragraph (x ++ y)


parseHeader :: String -> Maybe Block
parseHeader x = 
    let h = parseHeaderPrim x
    in 
        case h of 
            Unit -> Nothing
            y -> Just y

parseHeaderPrim :: String -> Block
parseHeaderPrim x = 
    let n = countChar x '#'
        sps = countChar (drop n x) ' '
    in 
        if n < 7 && sps > 0
        then header n (drop n x)
        else
            Unit

parseList :: [String] -> Block
parseList x = list x

parseRest s = 
    let (x, y) = getWhileParagraph s 
    in (parseParagraph x) : (parser y)

parseParagraph :: [String] -> Block
parseParagraph x = paragraph (concat (map (\x -> parseParagraph2 x) x))

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

parseParagraph3' "" acc = [normal (reverse acc)]
parseParagraph3' x acc = 
    case parseItalic x of 
        (Just x, y) -> ((normal (reverse acc)):(x:(parseParagraph3' y "")))
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
parseBold = parsePrettyParagraph (stripPrefix "**") (\x -> bold x)

parseItalic :: String -> (Maybe Text, String)
parseItalic = parsePrettyParagraph (stripPrefix "*") (\x -> italic x)
