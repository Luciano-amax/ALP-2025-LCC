module Main where

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Expr
import Parser
import Evaluator (Dual(..), EvalResult, ErrorType(..), eval, evalDual)
import FileReader

-- Función principal
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--file", archivo] -> procesarArchivo archivo
    ["-f", archivo] -> procesarArchivo archivo
    [] -> modoInteractivo
    _ -> mostrarAyuda

-- Mostrar ayuda
mostrarAyuda :: IO ()
mostrarAyuda = do
  putStrLn "=== Evaluador de Expresiones Matemáticas ==="
  putStrLn ""
  putStrLn "Uso:"
  putStrLn "  ALP2025-LCC              - Modo interactivo"
  putStrLn "  ALP2025-LCC -f archivo   - Leer desde archivo"
  putStrLn "  ALP2025-LCC --file archivo"
  putStrLn ""
  putStrLn "Formato del archivo:"
  putStrLn "  expresión @ valor_x"
  putStrLn ""
  putStrLn "Ejemplo:"
  putStrLn "  sin(x) + x^2 @ 1.5"
  putStrLn "  log(x) * cos(x) @ 2.0"

-- Modo interactivo (código existente)
modoInteractivo :: IO ()
modoInteractivo = do
  putStrLn "=== Evaluador de Expresiones con Derivadas ==="
  putStrLn "Escribe una expresión matemática:"
  putStrLn "Por ejemplo: sin(x) + x^2"
  putStrLn "Escribe 'salir' para finalizar."
  loop
  where
    loop = do
      putStr ">>> "
      hFlush stdout
      input <- getLine
      if input == "salir"
        then putStrLn "¡Adiós!"
        else do
          procesarEntrada input
          loop

-- Procesar la entrada del usuario (código existente)
procesarEntrada :: String -> IO ()
procesarEntrada input = do
  let parsed = parse parseExpr "" input
  case parsed of
    Left err -> putStrLn $ "Error de parsing: " ++ show err
    Right expr -> do
      putStrLn $ "AST generado:  " ++ show expr
      evaluarEntrada expr

-- Evaluar la expresión en x = 1 y calcular derivada (código existente)
evaluarEntrada :: Expr -> IO ()
evaluarEntrada expr = do
  putStr "Ingrese el valor de x: "
  hFlush stdout
  xInput <- getLine
  let mx = safeRead xInput
  case mx of
    Nothing -> putStrLn "Por favor ingrese un valor numérico válido para x."
    Just x  -> do
      let evalResult = eval expr x
      let dualResult = evalDual expr x
      mostrarResultados evalResult dualResult

-- Mostrar los resultados de la evaluación (código existente)
mostrarResultados :: EvalResult -> Either ErrorType Dual -> IO ()
mostrarResultados (Left err) _ = putStrLn $ "Error al evaluar la expresión: " ++ show err
mostrarResultados (Right primalVal) (Right (Dual _ derivVal)) = do
  putStrLn $ "Valor f(x): " ++ show primalVal
  putStrLn $ "Derivada f'(x): " ++ show derivVal
mostrarResultados _ (Left err) = putStrLn $ "Error al calcular derivada: " ++ show err

-- Función auxiliar para leer números de forma segura (código existente)
safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing