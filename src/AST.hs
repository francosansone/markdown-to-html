module AST where 

type Markdown = [Block]

data Block = Header (Int, String)
           | List [String]
           | Paragraph [Text]
           | Unit
   deriving Show
           
data Text = Bold String
          | Italic String
          | Normal String
   deriving Show