module Main where

import System.IO
import System.Environment
import Text.Parsec
import Expr
import Parser
import Evaluator
import FileReader
import PrettyPrinter
import Control.Monad
import Data.Char (isSpace, toLower)

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
  putStrLn "================================================"
  putStrLn "  Evaluador de Expresiones con Derivadas"
  putStrLn "================================================"
  putStrLn ""
  putStrLn "Escribe una expresion matematica (ej: sin(x) + x^2)"
  putStrLn "Comandos: 'salir', 'quit' o 'exit' para finalizar"
  putStrLn ""
  loop
  where
    loop = do
      putStr ">>> "
      hFlush stdout
      input <- getLine
      let normalizado = map toLower $ strip input
      unless (normalizado `elem` ["salir", "quit", "exit"]) $ do
        unless (null normalizado) $ procesarEntrada input
        loop
    
    -- Más eficiente evitando doble reverse
    strip = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd p = reverse . dropWhile p . reverse

procesarEntrada :: String -> IO ()
procesarEntrada input = do
  let parsed = parse parseExpr "" input
  either 
    (\err -> do
      putStrLn "[ERROR] Parsing fallido"
      putStrLn $ "   " ++ show err
      putStrLn "")
    (\expr -> do
      let optimizada = optimize expr
      putStrLn "+--------------------------------------------+"
      putStrLn $ "| Expresion:  " ++ prettyPrint expr
      when (expr /= optimizada) $
        putStrLn $ "| Optimizada: " ++ prettyPrint optimizada
      putStrLn "+--------------------------------------------+"
      evaluarEntrada optimizada)
    parsed

evaluarEntrada :: Expr -> IO ()
evaluarEntrada expr = do
  putStr ">> Ingrese el valor de x: "
  hFlush stdout
  xInput <- getLine
  case parsearValor xInput of
    Nothing -> do
      putStrLn "[ERROR] Valor invalido. Ingrese un numero o expresion constante."
      putStrLn ""
    Just x  -> do
      let evalResult = eval expr x
          dualResult = evalDual expr x
      mostrarResultados x evalResult dualResult
      putStrLn ""

-- Parsea un valor que puede ser un número o una expresión constante (pi, e, 2*pi, etc.)
parsearValor :: String -> Maybe Double
parsearValor s = 
  case safeRead s of
    Just x -> Just x
    Nothing -> case parse parseExpr "" s of
      Right expr -> case eval expr 0 of  -- Evaluar en x=0 para obtener constantes
        Right val -> Just val
        Left _ -> Nothing
      Left _ -> Nothing

-- Normaliza -0.0 a 0.0 para evitar doble representacion
--(Revisar teoria de IEEE754 -> ARqui)
normalizarCero :: Double -> Double
normalizarCero x = if x == 0 then 0 else x

mostrarResultados :: Double -> EvalResult -> Either ErrorType Dual -> IO ()
mostrarResultados x evalResult dualResult = do
  let xStr = if x == fromInteger (round x) then show (round x :: Integer) else show x
  putStrLn "+--- Resultados -----------------------------+"
  either 
    (\err -> putStrLn $ "| [X] Error: " ++ show err)
    (\val -> putStrLn $ "| f(" ++ xStr ++ ") = " ++ show (normalizarCero val))
    evalResult
  
  either
    (\err -> putStrLn $ "| [X] Error derivada: " ++ show err)
    (\(Dual _ d) -> putStrLn $ "| f'(" ++ xStr ++ ") = " ++ show (normalizarCero d))
    dualResult
  putStrLn "+--------------------------------------------+"

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing