module Utils where 

firstChar :: String -> Maybe Char
firstChar (c:str) = Just c
firstChar [] = Nothing

countChar :: String -> Char -> Int
countChar s c = case firstChar s of 
    Just x -> if x == c 
        then 1 + (countChar (tail s) c)
        else 0
    _ -> 0

getWhileList :: [String] -> ([String], [String])
getWhileList x = getWhileList' x ([], [])

getWhileList' :: [String] -> ([String], [String]) -> ([String], [String])
getWhileList' (l:ls) (x,y) = case l of    
    ('*':(' ':xs)) -> getWhileList' ls (xs:x, y)
    "" -> getWhileList' ls (x, y)
    _ -> (reverse x, l:ls)
getWhileList' _ (x,y) = (reverse x, [])

getWhileParagraph :: [String] -> ([String], [String])
getWhileParagraph x = getWhileParagraph' x ([], [])

getWhileParagraph' :: [String] -> ([String], [String]) -> ([String], [String])
getWhileParagraph' (x:xs) (a, b) = 
    case firstChar x of 
        Just _ -> getWhileParagraph' xs (x:a, b)
        Nothing -> (reverse a, x:xs)
getWhileParagraph' _ (a, b) = (reverse a, b)
