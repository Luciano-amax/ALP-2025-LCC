module Main (main) where

import Expr                   -- Importamos el AST
import Evaluator              -- Importamos el Evaluador (incluye eval y evalDual)
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

-- Caso de prueba 6: Validar derivada de sin(x^2)
case6 :: Test
case6 = TestCase $ do
  let expr = Sin (Pow (Var "x") (Lit 2))  -- f(x) = sin(x^2)
  let result = evalDual' expr 1           -- f(1), f'(1)
  assertEqual "Validar derivada de f(x) = sin(x^2)" (Right (sin 1, 2 * cos 1)) result

-- Caso de prueba 7: Parsing erróneo (error de sintaxis)
case7 :: Test
case7 = TestCase $ do
  let input = "(3 + * x)"  -- Expresión con error de sintaxis
  let result = parse parseExpr "" input
  assertBool "Parsing erróneo (parsing error esperado)" (case result of Left _ -> True; _ -> False)

-- Caso de prueba 8: Derivada de una función compuesta
case8 :: Test
case8 = TestCase $ do
  let expr = Mul (Sin (Var "x")) (Pow (Var "x") (Lit 3)) -- f(x) = sin(x) * x^3
  let result = evalDual' expr 2 -- Evaluar f(2) y f'(2)
  let f_x = sin 2 * 8
  let f'_x = cos 2 * 8 + sin 2 * 12
  assertEqual "Derivada de f(x) = sin(x) * x^3" (Right (f_x, f'_x)) result

-- Caso de prueba 9: Derivada con constantes como π
case9 :: Test
case9 = TestCase $ do
  let expr = Mul (Lit pi) (Var "x")  -- f(x) = π * x
  let result = evalDual' expr 3
  assertEqual "f(x) = π * x y su derivada" (Right (pi * 3, pi)) result

-- Suite de Pruebas
tests :: Test
tests = TestList [ case1, case2, case3, case4, case5, case6, case7, case8, case9 ]

-- Ejecutar las pruebas
main :: IO ()
main = do
  putStrLn "Corriendo pruebas del evaluador:"
  runTestTT tests
  return ()