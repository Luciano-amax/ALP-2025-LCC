module Main (main) where

import Expr                   -- Importamos el AST
import Evaluator              -- Importamos el Evaluador
import Text.Parsec (parse)    -- Para usar el Parser
import Parser                 -- Nuestro módulo de parsing
import Test.HUnit             -- Biblioteca para pruebas unitarias

-- Función auxiliar para evalDual con simplificación del resultado (por pruebas)
evalDual' :: Expr -> Double -> Either ErrorType (Double, Double)
evalDual' expr x = case evalDual expr x of
  Right (Dual p d) -> Right (p, d)
  Left err         -> Left err

-- Caso de prueba 1: Evaluación básica (sin derivadas)
case1 :: Test
case1 = TestCase $ do
  let expr = Add (Lit 3) (Mul (Var "x") (Var "x"))  -- f(x) = 3 + x^2
  let result = eval expr 2
  assertEqual "Evaluación básica de f(x) = 3 + x^2" (Right 7.0) result

-- Caso de prueba 2: Evaluación dual (valor y derivada) con un número dual
case2 :: Test
case2 = TestCase $ do
  let expr = Add (Lit 3) (Mul (Var "x") (Var "x"))  -- f(x) = 3 + x^2
  let result = evalDual' expr 2
  assertEqual "Evaluación dual de f(x) = 3 + x^2" (Right (7.0, 4.0)) result

-- Caso de prueba 3: Errores, como división por cero
case3 :: Test
case3 = TestCase $ do
  let expr = Div (Lit 1) (Sub (Var "x") (Var "x"))  -- f(x) = 1 / (x - x)
  let result = eval expr 2
  assertEqual "Errores en división por cero" (Left DivideByZero) result

-- Caso de prueba 4: Evaluación de funciones unarias
case4 :: Test
case4 = TestCase $ do
  let expr = Sin (Var "x")  -- f(x) = sin(x)
  let result = evalDual' expr (pi / 2)
  assertEqual "Evaluación de sin(pi/2)" (Right (1.0, 0.0)) result

-- Caso de prueba 5: Parsing y evaluación combinados
case5 :: Test
case5 = TestCase $ do
  let input = "3 + x^2"
  let result = case parse parseExpr "" input of
                 Right expr -> eval expr 2
                 Left _     -> Left $ UndefinedVariable "Parsing error"
  assertEqual "Parsing y evaluación de 3 + x^2" (Right 7.0) result

-- Suite con todos los casos de prueba
tests :: Test
tests = TestList [ case1, case2, case3, case4, case5 ]

-- Ejecutar las pruebas
main :: IO ()
main = do
  putStrLn "Corriendo pruebas del evaluador:"
  runTestTT tests
  return ()