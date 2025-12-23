module FileReader (procesarArchivo, parsearLinea, EvaluacionCompleta(..)) where

import Text.Parsec (parse)
import Expr
import Parser
import Evaluator (Dual(..), eval, evalDual, ErrorType)
import Control.Monad
import Data.List (isPrefixOf)

-- Tipo para representar el resultado completo de una evaluación
data EvaluacionCompleta = EvaluacionCompleta
  { exprEval :: Expr  , exprOptimizada :: Expr  , valorEval :: Double
  , resultadoValor :: Either ErrorType Double
  , resultadoDerivada :: Either ErrorType Dual
  } deriving (Show)

-- Tipo para representar una línea parseada
data LineaEvaluacion = LineaEvaluacion
  { expresion ::  Expr
  , valorX :: Double
  } deriving (Show)

-- Parsear una línea del formato "expresión @ valor"
-- Usa la mónada Either para composición elegante de errores
parsearLinea :: String -> Either String LineaEvaluacion
parsearLinea linea =
  case break (== '@') linea of
    (exprStr, '@':xStr) -> do
      -- Parsear la expresión usando do-notation para Either
      expr <- case parse parseExpr "" (trim exprStr) of
        Left err -> Left $ "Error de parsing: " ++ show err
        Right e -> Right e
      -- Parsear el valor de x con validación
      x <- parseDouble (trim xStr)
      pure $ LineaEvaluacion expr x
    _ -> Left "Formato inválido. Use: expresión @ valor"
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    parseDouble s = case reads s of
      [(val, "")] -> Right val
      _ -> Left $ "Valor de x inválido: " ++ s

-- Evaluar una expresión completa retornando todos los resultados
-- Aplica optimización algebraica antes de evaluar
evaluarCompleto :: Expr -> Double -> EvaluacionCompleta
evaluarCompleto expr x = 
  let exprOpt = optimize expr
  in EvaluacionCompleta
    { exprEval = expr
    , exprOptimizada = exprOpt
    , valorEval = x
    , resultadoValor = eval exprOpt x
    , resultadoDerivada = evalDual exprOpt x
    }

-- Verificar si una línea es un comentario o vacía
esLineaValida :: String -> Bool
esLineaValida s = 
  let trimmed = dropWhile (`elem` " \t") s
  in not (null trimmed) && not ("--" `isPrefixOf` trimmed)

-- Evaluar una línea y mostrar resultados
-- Usa pattern matching para manejo claro de casos
evaluarLinea :: Int -> String -> IO ()
evaluarLinea lineNum linea = do
  putStrLn $ "\n--- Línea " ++ show lineNum ++ " ---"
  putStrLn $ "Input: " ++ linea
  case parsearLinea linea of
    Left err -> putStrLn $ "[X] Error: " ++ err
    Right (LineaEvaluacion expr x) -> do
      let resultado = evaluarCompleto expr x
      putStrLn $ "Expresion: " ++ show (exprEval resultado)
      -- Mostrar optimización si cambió
      when (exprEval resultado /= exprOptimizada resultado) $
        putStrLn $ "Optimizada: " ++ show (exprOptimizada resultado)
      putStrLn $ "Evaluando en x = " ++ show (valorEval resultado)
      
      -- Mostrar valor usando Either como Functor
      either 
        (\err -> putStrLn $ "[X] Error al evaluar: " ++ show err)
        (\val -> putStrLn $ "[OK] f(" ++ show x ++ ") = " ++ show val)
        (resultadoValor resultado)
      
      -- Mostrar derivada
      either
        (\err -> putStrLn $ "[X] Error al calcular derivada: " ++ show err)
        (\(Dual _ d) -> putStrLn $ "[OK] f'(" ++ show x ++ ") = " ++ show d)
        (resultadoDerivada resultado)

-- Procesar un archivo completo con manejo de errores mejorado
procesarArchivo :: FilePath -> IO ()
procesarArchivo archivo = do
  contenido <- readFile archivo
  let lineas = filter esLineaValida $ lines contenido
      numLineas = length lineas
  
  putStrLn $ "=== Procesando archivo: " ++ archivo ++ " ==="
  putStrLn $ "Total de expresiones: " ++ show numLineas
  
  when (numLineas == 0) $
    putStrLn "Advertencia: No se encontraron expresiones válidas"
  
  -- Usar mapM_ para efectos IO de forma más funcional
  mapM_ (uncurry evaluarLinea) (zip [1..] lineas)
  putStrLn "\n=== Proceso completado ==="