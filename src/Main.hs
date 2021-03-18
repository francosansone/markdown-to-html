module Main where


import System.Environment (getArgs)
import Utils
import Parser             (parser)


main :: IO ()
main = do args <- getArgs
          if null args then putStrLn ("Error: falta indicar como argumento "
                                      ++ "la ruta del archivo a ejecutar.")
                       else do s <- readFile (head args)
                               print( parser (lines s))
                        
                                
                        

-- Ejecuta un programa a partir de su archivo fuente
--run :: [Char] -> IO ()
--run ifile = do s <- readFile ifile
               --print (eval (parser s))
