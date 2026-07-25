module Console
  ( modoInteractivo
  ) where

import Control.Monad
import System.IO (hFlush, stdout)
import Text.Parsec (parse)

import DSL

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
      let normalizado = map toLowerAscii $ trim input
      unless (normalizado `elem` ["salir", "quit", "exit"]) $ do
        unless (null normalizado) $ procesarEntrada input
        loop

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

-- Parsea un valor numerico o una expresion constante.
parsearValor :: String -> Maybe Double
parsearValor s =
  case safeRead s of
    Just x -> Just x
    Nothing -> case parse parseExpr "" s of
      Right expr -> case eval expr 0 of
        Right val -> Just val
        Left _ -> Nothing
      Left _ -> Nothing

-- Normaliza -0.0 a 0.0 para que la salida no muestre dos ceros.
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

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

toLowerAscii :: Char -> Char
toLowerAscii c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c
