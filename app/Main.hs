module Main where

import System.IO (hFlush, stdout)
import Text.Parsec (parse)      -- Para usar el parser
import Expr                     -- Importar el AST
import Parser                   -- Importar nuestro parser para expresiones
import Evaluator                -- Importar el evaluador

-- Función principal (bucle interactivo)
main :: IO ()
main = do
  putStrLn "=== Evaluador de Expresiones con Derivadas ==="
  putStrLn "Escribe una expresión matemática:"
  putStrLn "Por ejemplo: sin(x) + x^2"
  putStrLn "Escribe 'salir' para finalizar."
  loop
  where
    loop = do
      putStr ">>> "           -- Indicador CLI
      hFlush stdout           -- Asegurar que el indicador se imprima
      input <- getLine        -- Leer input del usuario
      if input == "salir"
        then putStrLn "¡Adiós!"
        else do
          procesarEntrada input
          loop

-- Procesar la entrada del usuario
procesarEntrada :: String -> IO ()
procesarEntrada input = do
  let parsed = parse parseExpr "" input   -- Intentar construir el AST
  case parsed of
    Left err -> putStrLn $ "Error de parsing: " ++ show err
    Right expr -> do
      putStrLn $ "AST generado: " ++ show expr
      evaluarEntrada expr

-- Evaluar la expresión en x = 1 y calcular derivada
evaluarEntrada :: Expr -> IO ()
evaluarEntrada expr = do
  -- Solicitar el valor de x al usuario
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

-- Mostrar los resultados de la evaluación
mostrarResultados :: EvalResult -> Either ErrorType Dual -> IO ()
mostrarResultados (Left err) _ = putStrLn $ "Error al evaluar la expresión: " ++ show err
mostrarResultados (Right primal) (Right (Dual _ deriv)) = do
  putStrLn $ "Valor f(x): " ++ show primal
  putStrLn $ "Derivada f'(x): " ++ show deriv
mostrarResultados _ (Left err) = putStrLn $ "Error al calcular derivada: " ++ show err

-- Función auxiliar para leer números de forma segura
safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing