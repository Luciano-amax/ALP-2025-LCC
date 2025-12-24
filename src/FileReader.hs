module FileReader (procesarArchivo, parsearLinea, EvaluacionCompleta(..)) where

import Text.Parsec
import Expr
import Parser
import Evaluator
import Control.Monad

data EvaluacionCompleta = EvaluacionCompleta
  { exprEval :: Expr  , exprOptimizada :: Expr  , valorEval :: Double
  , resultadoValor :: Either ErrorType Double
  , resultadoDerivada :: Either ErrorType Dual
  } deriving (Show)

data LineaEvaluacion = LineaEvaluacion
  { expresion ::  Expr
  , valorX :: Double
  } deriving (Show)

-- Parsea líneas con formato: expresión @ valor
parsearLinea :: String -> Either String LineaEvaluacion
parsearLinea linea =
  case break (== '@') linea of
    (exprStr, '@':xStr) -> do
      expr <- case parse parseExpr "" (trim exprStr) of
        Left err -> Left $ "Error de parsing: " ++ show err
        Right e -> Right e
      x <- parseDouble (trim xStr)
      pure $ LineaEvaluacion expr x
    _ -> Left "Formato inválido. Use: expresión @ valor"
  where
    trim s = let s' = dropWhile (== ' ') s in reverse (dropWhile (== ' ') (reverse s'))
    parseDouble s = case reads s of
      [(val, "")] -> Right val
      _ -> Left $ "Valor de x inválido: " ++ s


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

esLineaValida :: String -> Bool
esLineaValida s = case dropWhile (`elem` " \t") s of
  [] -> False
  '-':'-':_ -> False
  _ -> True


evaluarLinea :: Int -> String -> IO ()
evaluarLinea lineNum linea = do
  putStrLn $ "\n--- Línea " ++ show lineNum ++ " ---"
  putStrLn $ "Input: " ++ linea
  case parsearLinea linea of
    Left err -> putStrLn $ "[X] Error: " ++ err
    Right (LineaEvaluacion expr x) -> do
      let resultado = evaluarCompleto expr x
      putStrLn $ "Expresion: " ++ show (exprEval resultado)
      when (exprEval resultado /= exprOptimizada resultado) $
        putStrLn $ "Optimizada: " ++ show (exprOptimizada resultado)
      putStrLn $ "Evaluando en x = " ++ show (valorEval resultado)
      
      either 
        (\err -> putStrLn $ "[X] Error al evaluar: " ++ show err)
        (\val -> putStrLn $ "[OK] f(" ++ show x ++ ") = " ++ show val)
        (resultadoValor resultado)
      
      either
        (\err -> putStrLn $ "[X] Error al calcular derivada: " ++ show err)
        (\(Dual _ d) -> putStrLn $ "[OK] f'(" ++ show x ++ ") = " ++ show d)
        (resultadoDerivada resultado)


procesarArchivo :: FilePath -> IO ()
procesarArchivo archivo = do
  contenido <- readFile archivo
  let lineas = filter esLineaValida $ lines contenido
      numLineas = length lineas
  
  putStrLn $ "=== Procesando archivo: " ++ archivo ++ " ==="
  putStrLn $ "Total de expresiones: " ++ show numLineas
  
  when (numLineas == 0) $
    putStrLn "Advertencia: No se encontraron expresiones válidas"
  
  mapM_ (uncurry evaluarLinea) (zip [1..] lineas)
  putStrLn "\n=== Proceso completado ==="