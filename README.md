# Parser Mardown a HTML

Trabajo final de la materia ALP de LCC

El proyecto consiste en un parser de un Markdown reducido a HTML.

Para el mismo se definió un DSL, usando básicamente deep embedding, dado que el trabajo es realizado por la función de ejecución, el tipo no es más que una representación de la estructura del códio Markdown. 

Podemos Representar:

* Headers
* Listas desordenadas
* Párrafos con letra normal, negrita e itálica.


El tipo que definimos es 

```
    type Markdown = [Block]

    data Block = Header (Int, String)

           | List [String]

           | Paragraph [Text]

    deriving Show
           
    data Text = Bold String

          | Italic String

          | Normal String

    deriving Show

```

Y una serie de funciones primitivas

```
header :: Int -> String -> Block 

list :: [String] -> Block

paragraph :: [Text] -> Block

bold :: String -> Text 

italic :: String -> Text 

normal :: String -> Text 
```

Y funciones derivadas

```
combineList :: Block -> Block -> Block

combineParagraph :: [Text] -> [Text] -> Block 
```

La función 

```
parser :: [String] -> Markdown 
```

se encarga de seleccionarlas y combinarlas oportunamente.

El tipo Mardown funciona como lenguaje intermedio. La conversión de un archivo markdown a nuestro tipo nos permitiría construir todo tipo de generadores, por ejemplo, HTML, Látex, etc.

Para compilar el mismo, corremos el script compile.sh, para correr el ejemplo en la carpeta sample, corremos el script run.sh. La salida estará en sample/sample.html. Corriendo parser *input_file* *output_file* podemos probar otros archivos.

Referencias usadas durante el desarrollo de este trabajo:

* https://www.markdownguide.org/basic-syntax/
* https://hoogle.haskell.org/