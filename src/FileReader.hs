module FileReader (procesarArchivo, parsearLinea) where

import Text.Parsec (parse)
import Expr
import Parser
import Evaluator (Dual(..), eval, evalDual)

-- Tipo para representar una línea parseada
data LineaEvaluacion = LineaEvaluacion
  { expresion ::  Expr
  , valorX :: Double
  } deriving (Show)

-- Parsear una línea del formato "expresión @ valor"
parsearLinea :: String -> Either String LineaEvaluacion
parsearLinea linea =
  case break (== '@') linea of
    (exprStr, '@':xStr) -> do
      -- Parsear la expresión
      expr <- case parse parseExpr "" (trim exprStr) of
        Left err -> Left $ "Error de parsing: " ++ show err
        Right e -> Right e
      -- Parsear el valor de x
      x <- case reads (trim xStr) of
        [(val, "")] -> Right val
        _ -> Left $ "Valor de x inválido: " ++ xStr
      return $ LineaEvaluacion expr x
    _ -> Left "Formato inválido.  Use:  expresión @ valor"
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Evaluar una línea y mostrar resultados
evaluarLinea :: Int -> String -> IO ()
evaluarLinea lineNum linea = do
  putStrLn $ "\n--- Línea " ++ show lineNum ++ " ---"
  putStrLn $ "Input: " ++ linea
  case parsearLinea linea of
    Left err -> putStrLn $ "[X] Error: " ++ err
    Right (LineaEvaluacion expr x) -> do
      putStrLn $ "Expresion: " ++ show expr
      putStrLn $ "Evaluando en x = " ++ show x
      
      -- Evaluar valor
      case eval expr x of
        Left err -> putStrLn $ "[X] Error al evaluar: " ++ show err
        Right val -> putStrLn $ "[OK] f(" ++ show x ++ ") = " ++ show val
      
      -- Evaluar derivada
      case evalDual expr x of
        Left err -> putStrLn $ "[X] Error al calcular derivada: " ++ show err
        Right (Dual _ deriv') -> putStrLn $ "[OK] f'(" ++ show x ++ ") = " ++ show deriv'

-- Procesar un archivo completo
procesarArchivo :: FilePath -> IO ()
procesarArchivo archivo = do
  contenido <- readFile archivo
  let lineas = filter (not . null) $ map trim $ lines contenido
  putStrLn $ "=== Procesando archivo: " ++ archivo ++ " ==="
  putStrLn $ "Total de expresiones: " ++ show (length lineas)
  mapM_ (uncurry evaluarLinea) (zip [1..] lineas)
  putStrLn "\n=== Proceso completado ==="
  where
    trim = reverse . dropWhile (`elem` " \t\r") . reverse . dropWhile (`elem` " \t\r")