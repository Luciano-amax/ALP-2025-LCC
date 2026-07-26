module FileReader
  ( procesarArchivo
  , parsearLinea
  , parsearContenido
  , EvaluacionCompleta(..)
  , LineaEvaluacion(..)
  ) where

import Text.Parsec
import DSL
import Control.Monad
import Data.List (mapAccumL)
import Data.Char (isSpace)

data EvaluacionCompleta = EvaluacionCompleta
  { exprEval :: Expr
  , exprOptimizada :: Expr
  , valorEval :: Double
  , resultadoValor :: Either ErrorType Double
  , resultadoDerivada :: Either ErrorType Dual
  } deriving (Show)

data LineaEvaluacion = LineaEvaluacion
  { expresion :: Expr
  , valorX :: Double
  } deriving (Show)

-- Parsea una linea del formato "expresion @ valor", limpiando comentarios inline.
parsearLinea :: String -> Either String LineaEvaluacion
parsearLinea linea =
  let (sinBloques, _) = quitarBloques False linea
      lineaLimpia = limpiarComentarios sinBloques
  in case break (== '@') lineaLimpia of
    (exprStr, '@':xStr) -> do
      expr <- parseExprArchivo (strip exprStr)
      x <- parseValor (strip xStr)
      return $ LineaEvaluacion expr x
    _ -> Left "Formato invalido. Use: expresion @ valor"
  where
    strip = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd p = reverse . dropWhile p . reverse

    parseExprArchivo s = case parse parseExpr "" s of
      Left err -> Left $ "Error de parsing: " ++ show err
      Right expr -> Right expr

    -- El valor de x puede ser un numero o una expresion constante como pi/2.
    parseValor s = case reads s of
      [(val, "")] -> Right val
      _ -> case parse parseExpr "" s of
        Right expr -> case evalWithEnv [] expr of
          Right val -> Right val
          Left err -> Left $ "Valor de x invalido: " ++ mostrarError err
        Left err -> Left $ "Valor de x invalido: " ++ show err

    limpiarComentarios [] = []
    limpiarComentarios ('-':'-':_) = ""
    limpiarComentarios (c:cs) = c : limpiarComentarios cs

parsearContenido :: String -> [Either String LineaEvaluacion]
parsearContenido contenido = map parsearLinea (lineasEvaluables contenido)

-- Evita mostrar -0.0, que confunde aunque sea el mismo valor numerico.
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
  '-':'-':_ -> False
  '{':'-':_ -> False
  _ -> True

filtrarComentariosMultilinea :: [String] -> [String]
filtrarComentariosMultilinea =
  snd . mapAccumL quitarLinea False
  where
    quitarLinea enBloque linea =
      let (lineaLimpia, enBloque') = quitarBloques enBloque linea
      in (enBloque', lineaLimpia)

-- Devuelve la linea sin bloques y si quedo abierto un comentario multilinea.
quitarBloques :: Bool -> String -> (String, Bool)
quitarBloques enBloque [] = ([], enBloque)
quitarBloques True ('-':'}':resto) = quitarBloques False resto
quitarBloques True (_:resto) = quitarBloques True resto
quitarBloques False ('{':'-':resto) = quitarBloques True resto
quitarBloques False (c:resto) =
  let (restoLimpio, enBloque') = quitarBloques False resto
  in (c : restoLimpio, enBloque')

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
        (\err -> putStrLn $ "  [X] Error al evaluar: " ++ mostrarError err)
        (\val -> putStrLn $ "  [OK] f(" ++ xStr ++ ") = " ++ show (normalizarCero val))
        (resultadoValor resultado)

      either
        (\err -> putStrLn $ "  [X] Error al calcular derivada: " ++ mostrarError err)
        (\(Dual _ d) -> putStrLn $ "  [OK] f'(" ++ xStr ++ ") = " ++ show (normalizarCero d))
        (resultadoDerivada resultado)

procesarArchivo :: FilePath -> IO ()
procesarArchivo archivo = do
  contenido <- readFile archivo
  let lineas = lineasEvaluables contenido
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

lineasEvaluables :: String -> [String]
lineasEvaluables contenido =
  let lineasFiltradas = filtrarComentariosMultilinea (lines contenido)
  in filter esLineaValida lineasFiltradas
