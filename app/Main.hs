module Main where

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Expr
import Parser
import Evaluator (Dual(..), EvalResult, ErrorType(..), eval, evalDual)
import FileReader
import Control.Monad

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
      putStrLn "Error: Argumentos inválidos"
      mostrarAyuda

mostrarAyuda :: IO ()
mostrarAyuda = do
  putStrLn "=== Evaluador de Expresiones Matemáticas ==="
  putStrLn ""
  putStrLn "Uso:"
  putStrLn "  ALP2025-LCC              - Modo interactivo"
  putStrLn "  ALP2025-LCC -f archivo   - Leer desde archivo"
  putStrLn "  ALP2025-LCC --file archivo"
  putStrLn "  ALP2025-LCC --help       - Mostrar esta ayuda"
  putStrLn ""
  putStrLn "Formato del archivo:"
  putStrLn "  expresión @ valor_x"
  putStrLn "  -- Los comentarios comienzan con --"
  putStrLn ""
  putStrLn "Ejemplo:"
  putStrLn "  sin(x) + x^2 @ 1.5"
  putStrLn "  log(x) * cos(x) @ 2.0"
  putStrLn ""
  putStrLn "Funciones soportadas:"
  putStrLn "  Trigonométricas: sin, cos, tan"
  putStrLn "  Hiperbólicas: sinh, cosh, tanh, arsinh, arcosh, artanh"
  putStrLn "  Otras: exp, log"
  putStrLn "  Constantes: pi, e"

modoInteractivo :: IO ()
modoInteractivo = do
  putStrLn "=== Evaluador de Expresiones con Derivadas ==="
  putStrLn "Escribe una expresión matemática:"
  putStrLn "Por ejemplo: sin(x) + x^2"
  putStrLn "Escribe 'salir' o 'quit' para finalizar."
  loop
  where
    loop = do
      putStr ">>> "
      hFlush stdout
      input <- getLine
      let normalizado = map toLower $ trim input
      unless (normalizado `elem` ["salir", "quit", "exit"]) $ do
        unless (null normalizado) $ procesarEntrada input
        loop
    
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c

procesarEntrada :: String -> IO ()
procesarEntrada input = do
  let parsed = parse parseExpr "" input
  either 
    (\err -> putStrLn $ "Error de parsing: " ++ show err)
    (\expr -> do
      putStrLn $ "AST generado: " ++ show expr
      evaluarEntrada expr)
    parsed

evaluarEntrada :: Expr -> IO ()
evaluarEntrada expr = do
  putStr "Ingrese el valor de x: "
  hFlush stdout
  xInput <- getLine
  case safeRead xInput of
    Nothing -> putStrLn "Por favor ingrese un valor numérico válido para x."
    Just x  -> do
      let evalResult = eval expr x
          dualResult = evalDual expr x
      mostrarResultados x evalResult dualResult

mostrarResultados :: Double -> EvalResult -> Either ErrorType Dual -> IO ()
mostrarResultados x evalResult dualResult = do
  either 
    (\err -> putStrLn $ "Error al evaluar la expresión: " ++ show err)
    (\val -> putStrLn $ "Valor f(" ++ show x ++ "): " ++ show val)
    evalResult
  
  either
    (\err -> putStrLn $ "Error al calcular derivada: " ++ show err)
    (\(Dual _ d) -> putStrLn $ "Derivada f'(" ++ show x ++ "): " ++ show d)
    dualResult

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing