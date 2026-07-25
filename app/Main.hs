module Main where

import System.Environment (getArgs)

import Console
import FileReader

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--file", archivo] -> procesarArchivo archivo
    ["-f", archivo] -> procesarArchivo archivo
    ["--help"] -> mostrarAyuda
    ["-h"] -> mostrarAyuda
    [] -> modoInteractivo
    _ -> do
      putStrLn "Error: Argumentos invalidos"
      mostrarAyuda

mostrarAyuda :: IO ()
mostrarAyuda = do
  putStrLn "=== Evaluador de Expresiones Matematicas ==="
  putStrLn ""
  putStrLn "Uso:"
  putStrLn "  ALP2025-LCC              - Modo interactivo"
  putStrLn "  ALP2025-LCC -f archivo   - Leer desde archivo"
  putStrLn "  ALP2025-LCC --file archivo"
  putStrLn "  ALP2025-LCC --help       - Mostrar esta ayuda"
  putStrLn ""
  putStrLn "Formato del archivo:"
  putStrLn "  expresion @ valor_x"
  putStrLn "  -- Los comentarios comienzan con --"
  putStrLn ""
  putStrLn "Ejemplo:"
  putStrLn "  sin(x) + x^2 @ 1.5"
  putStrLn "  log(x) * cos(x) @ 2.0"
  putStrLn ""
  putStrLn "Funciones soportadas:"
  putStrLn "  Trigonometricas: sin, cos, tan"
  putStrLn "  Hiperbolicas: sinh, cosh, tanh, arsinh, arcosh, artanh"
  putStrLn "  Otras: exp, log"
  putStrLn "  Constantes: pi, e"
