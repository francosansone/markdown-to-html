module HTMLGenerator where 
import AST

generateHTML :: Markdown -> String
generateHTML l = concat (map generateHTML1 l)

generateHTML1 :: Block -> String
generateHTML1 (Header (n, s)) = "<h" ++ show n ++ ">" ++ s ++ "</h>"
generateHTML1 (List l) = "<ul>" ++ (generateList l) ++ "</ul>"
generateHTML1 (Paragraph t) = "<p>" ++ (generateParagraph t) ++ "</p>"
generateHTML1 Unit = "" 

generateList :: [String] -> String 
generateList s = concat (map (\x -> "<li>" ++ x ++ "</li>") s)

generateParagraph :: [Text] -> String
generateParagraph t = concat (map (\x -> generateParagraph1 x) t)

generateParagraph1 :: Text -> String 
generateParagraph1 (Bold s) = "<strong>" ++ s ++ "</strong>"
generateParagraph1 (Italic s) = "<em>" ++ s ++ "</em>"
generateParagraph1 (Normal s) = s