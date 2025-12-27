module FileReader (procesarArchivo, parsearLinea, EvaluacionCompleta(..)) where

import Text.Parsec
import Expr
import Parser
import Evaluator
import PrettyPrinter
import Control.Monad
import Data.List (isInfixOf)
import Data.Char (isSpace)

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
-- Simplemente para archivos - Main usa un parseo interno (Ver Conveniencia)
parsearLinea :: String -> Either String LineaEvaluacion
parsearLinea linea =
  let lineaLimpia = limpiarComentarios linea
  in case break (== '@') lineaLimpia of
    (exprStr, '@':xStr) -> do
      expr <- case parse parseExpr "" (strip exprStr) of
        Left err -> Left $ "Error de parsing: " ++ show err
        Right e -> Right e
      x <- parseDouble (strip xStr)
      pure $ LineaEvaluacion expr x
    _ -> Left "Formato inválido. Use: expresión @ valor"
  where
    -- Más eficiente usando dropWhile en ambas direcciones
    strip = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd p = reverse . dropWhile p . reverse
    
    parseDouble s = case reads s of
      [(val, "")] -> Right val
      _ -> Left $ "Valor de x inválido: " ++ s
    
    -- Elimina comentarios de línea (--) pero respeta números negativos
    limpiarComentarios [] = []
    limpiarComentarios ('-':'-':_) = ""  -- Comentario encontrado
    limpiarComentarios (c:cs) = c : limpiarComentarios cs


-- Normaliza -0.0 a 0.0 para una mejor presentación
normalizarCero :: Double -> Double
normalizarCero x = if x == 0 then 0 else x

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
  '-':'-':_ -> False  -- Líneas que empiezan con comentario
  '{':'-':_ -> False  -- Inicio de comentario multilinea
  _ -> True

-- Filtra comentarios multilinea de una lista de líneas
-- Optimizado usando isInfixOf de Data.List en lugar de implementación manual
filtrarComentariosMultilinea :: [String] -> [String]
filtrarComentariosMultilinea = go False
  where
    go _ [] = []
    go True (l:ls) = 
      if "-}" `isInfixOf` l
        then go False ls
        else go True ls
    go False (l:ls)
      | "{-" `isInfixOf` l =
          if "-}" `isInfixOf` l
            then l : go False ls  -- comentario de una sola línea {- ... -}
            else go True ls
      | otherwise = l : go False ls


evaluarLinea :: Int -> String -> IO ()
evaluarLinea lineNum linea = do
  putStrLn ""
  putStrLn  "==============================================="
  putStrLn $ " Expresion #" ++ show lineNum
  putStrLn  "==============================================="
  putStrLn $ "  Input: " ++ linea
  case parsearLinea linea of
    Left err -> do
      putStrLn $ "  [ERROR] " ++ err
      putStrLn ""
    Right (LineaEvaluacion expr x) -> do
      let resultado = evaluarCompleto expr x
          xStr = if x == fromInteger (round x) then show (round x :: Integer) else show x
      putStrLn $ "  Expresion:  " ++ prettyPrint (exprEval resultado)
      when (exprEval resultado /= exprOptimizada resultado) $
        putStrLn $ "  Optimizada: " ++ prettyPrint (exprOptimizada resultado)
      putStrLn $ "  Evaluando en x = " ++ xStr
      putStrLn "  ---------------------------------------------"
      
      either 
        (\err -> putStrLn $ "  [X] Error al evaluar: " ++ show err)
        (\val -> putStrLn $ "  [OK] f(" ++ xStr ++ ") = " ++ show (normalizarCero val))
        (resultadoValor resultado)
      
      either
        (\err -> putStrLn $ "  [X] Error al calcular derivada: " ++ show err)
        (\(Dual _ d) -> putStrLn $ "  [OK] f'(" ++ xStr ++ ") = " ++ show (normalizarCero d))
        (resultadoDerivada resultado)


procesarArchivo :: FilePath -> IO ()
procesarArchivo archivo = do
  contenido <- readFile archivo
  let lineasFiltradas = filtrarComentariosMultilinea (lines contenido)
      lineas = filter esLineaValida lineasFiltradas
      numLineas = length lineas
  
  putStrLn ""
  putStrLn "==============================================="
  putStrLn "         PROCESAMIENTO DE ARCHIVO"
  putStrLn "==============================================="
  putStrLn $ "  Archivo: " ++ archivo
  putStrLn $ "  Total de expresiones: " ++ show numLineas
  putStrLn ""
  
  when (numLineas == 0) $ do
    putStrLn "  [!] Advertencia: No se encontraron expresiones validas"
    putStrLn ""
  
  mapM_ (uncurry evaluarLinea) (zip [1..] lineas)
  putStrLn ""
  putStrLn "==============================================="
  putStrLn "         [OK] PROCESO COMPLETADO"
  putStrLn "==============================================="
  putStrLn ""