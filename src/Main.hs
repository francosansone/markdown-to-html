module Main where


import System.Environment (getArgs)
import Utils
import Parser             (parser)
import HTMLGenerator


main :: IO ()
main = do 
        args <- getArgs
        case args of 
                [] -> putStrLn ("Error: falta indicar como argumento "
                                      ++ "la ruta del archivo de entrada y de salida.")
                filename:[] -> putStrLn ("Error: falta indicar como argumento "
                                      ++ "la ruta del archivo de salida.")
                input:[output] -> do s <- readFile input
                                     writeFile output (generateHTML( parser (lines s)))
                _ -> putStrLn ("Error: cantidad incorrecta de argumentos.")
