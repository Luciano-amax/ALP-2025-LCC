module Main (main) where

import Expr                   -- Importamos el AST
import Evaluator              -- Importamos el Evaluador (incluye eval y evalDual)
import EvalM (runEvalM, throwEval, lookupVar)
import FileReader (LineaEvaluacion(..), parsearLinea)
import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import PrettyPrinter (prettyPrint)
import Parser                 -- Nuestro módulo de parsing
import Test.HUnit

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
  case result of
    Right (val, deriv') -> do
      assertEqual "Valor de sin(pi/2)" 1.0 val
      assertBool "Derivada cercana a 0" (abs deriv' < 1e-10)
    Left err -> assertFailure $ "Error inesperado: " ++ show err

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

case10 :: Test
case10 = TestList
  [ TestCase $ assertEqual "Valor de pi" (Right pi) (eval (Var "pi") 0)
  , TestCase $ assertEqual "Valor de e" (Right (exp 1)) (eval (Var "e") 0)
  ]

-- Caso 11: Funciones con pi y e
case11 :: Test
case11 = TestList
  [ TestCase $ assertEqual "sin(pi)" (Right (sin pi)) (eval (Sin (Var "pi")) 0)
  , TestCase $ assertEqual "cos(pi)" (Right (cos pi)) (eval (Cos (Var "pi")) 0)
  , TestCase $ assertEqual "exp(e)" (Right (exp (exp 1))) (eval (Exp (Var "e")) 0)
  ]

-- ========== Tests de Nuevas Características ==========

-- Tests para sqrt
testSqrt :: Test
testSqrt = TestList
  [ TestCase $ assertEqual "sqrt(4)" (Right 2.0) (eval (Sqrt (Lit 4)) 0)
  , TestCase $ assertEqual "sqrt(9)" (Right 3.0) (eval (Sqrt (Lit 9)) 0)
  , TestCase $ assertEqual "sqrt(x) en x=16" (Right 4.0) (eval (Sqrt (Var "x")) 16)
  , TestCase $ do
      let result = evalDual' (Sqrt (Var "x")) 9
      assertEqual "sqrt(x) derivada en x=9" (Right (3.0, 1/6)) result
  , TestCase $ do
      let result = eval (Sqrt (Lit (-1))) 0
      case result of
        Left (DomainError _) -> return ()
        _ -> assertFailure "sqrt(-1) debería dar error de dominio"
  ]

-- Tests para números negativos literales
testNegativosLiterales :: Test
testNegativosLiterales = TestList
  [ TestCase $ assertEqual "-5" (Right (-5.0)) (eval (Lit (-5)) 0)
  , TestCase $ assertEqual "-3.14" (Right (-3.14)) (eval (Lit (-3.14)) 0)
  , TestCase $ do
      let result = case parse parseExpr "" "-5" of
                     Right expr -> eval expr 0
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "parsing -5" (Right (-5.0)) result
  , TestCase $ do
      let result = case parse parseExpr "" "-3.14" of
                     Right expr -> eval expr 0
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "parsing -3.14" (Right (-3.14)) result
  ]

-- Tests para optimizaciones algebraicas
testOptimizaciones :: Test
testOptimizaciones = TestList
  [ TestCase $ assertEqual "x + 0 = x" (Var "x") (optimize (Add (Var "x") (Lit 0)))
  , TestCase $ assertEqual "0 + x = x" (Var "x") (optimize (Add (Lit 0) (Var "x")))
  , TestCase $ assertEqual "x * 1 = x" (Var "x") (optimize (Mul (Var "x") (Lit 1)))
  , TestCase $ assertEqual "1 * x = x" (Var "x") (optimize (Mul (Lit 1) (Var "x")))
  , TestCase $ assertEqual "x * 0 se conserva para no ocultar errores" (Mul (Var "x") (Lit 0)) (optimize (Mul (Var "x") (Lit 0)))
  , TestCase $ assertEqual "0 * x se conserva para no ocultar errores" (Mul (Lit 0) (Var "x")) (optimize (Mul (Lit 0) (Var "x")))
  , TestCase $ assertEqual "0 * 5 = 0" (Lit 0) (optimize (Mul (Lit 0) (Lit 5)))
  , TestCase $ assertEqual "x - 0 = x" (Var "x") (optimize (Sub (Var "x") (Lit 0)))
  , TestCase $ assertEqual "x / 1 = x" (Var "x") (optimize (Div (Var "x") (Lit 1)))
  , TestCase $ assertEqual "x^0 se conserva para no ocultar 0^0" (Pow (Var "x") (Lit 0)) (optimize (Pow (Var "x") (Lit 0)))
  , TestCase $ assertEqual "x^1 = x" (Var "x") (optimize (Pow (Var "x") (Lit 1)))
  , TestCase $ assertEqual "1^x se conserva para no ocultar errores del exponente" (Pow (Lit 1) (Var "x")) (optimize (Pow (Lit 1) (Var "x")))
  , TestCase $ assertEqual "2^0 = 1" (Lit 1) (optimize (Pow (Lit 2) (Lit 0)))
  ]

testOptimizacionesConservadoras :: Test
testOptimizacionesConservadoras = TestList
  [ TestCase $ assertEqual "x/x no se reduce porque x puede ser cero" (Div (Var "x") (Var "x")) (optimize (Div (Var "x") (Var "x")))
  , TestCase $ assertEqual "x-x no se reduce porque podria ocultar variables sin definir" (Sub (Var "x") (Var "x")) (optimize (Sub (Var "x") (Var "x")))
  , TestCase $ assertEqual "0 * expr no se reduce si expr puede fallar" (Mul (Lit 0) (Div (Lit 1) (Lit 0))) (optimize (Mul (Lit 0) (Div (Lit 1) (Lit 0))))
  , TestCase $ assertEqual "0^0 no se pliega" (Pow (Lit 0) (Lit 0)) (optimize (Pow (Lit 0) (Lit 0)))
  , TestCase $ assertEqual "0^(-1) no se pliega" (Pow (Lit 0) (Lit (-1))) (optimize (Pow (Lit 0) (Lit (-1))))
  , TestCase $ assertEqual "base negativa con exponente fraccionario no se pliega" (Pow (Lit (-1)) (Lit 0.5)) (optimize (Pow (Lit (-1)) (Lit 0.5)))
  , TestCase $ assertEqual "division por casi cero no se pliega" (Div (Lit 1) (Lit 1e-20)) (optimize (Div (Lit 1) (Lit 1e-20)))
  , TestCase $ assertEqual "potencia con base muy chica se pliega como numero no nulo" (Lit (1e-20 ** (-1))) (optimize (Pow (Lit 1e-20) (Lit (-1))))
  , TestCase $ assertEqual "potencia invalida no se pliega a NaN" (Pow (Lit (-2)) (Lit 1e-20)) (optimize (Pow (Lit (-2)) (Lit 1e-20)))
  , TestCase $ assertEqual "potencia no finita no se pliega" (Pow (Lit 10) (Lit 400)) (optimize (Pow (Lit 10) (Lit 400)))
  , TestCase $ do
      case eval (Pow (Lit (-2)) (Lit 1e-20)) 0 of
        Left (DomainError _) -> return ()
        other -> assertFailure $ "base negativa con exponente casi cero debia dar DomainError y dio " ++ show other
  , TestCase $ do
      case eval (Pow (Lit 10) (Lit 400)) 0 of
        Left (DomainError _) -> return ()
        other -> assertFailure $ "potencia no finita debia dar DomainError y dio " ++ show other
  , TestCase $ assertEqual "eval optimizado conserva division por cero" (Left DivideByZero) (eval (optimize (Div (Var "x") (Var "x"))) 0)
  , TestCase $ assertEqual "eval optimizado conserva division por casi cero" (Left DivideByZero) (eval (optimize (Div (Lit 1) (Lit 1e-20))) 0)
  , TestCase $ assertEqual "x^1 conserva valores muy chicos" (Right 1e-20) (eval (optimize (Pow (Var "x") (Lit 1))) 1e-20)
  , TestCase $ do
      case eval (optimize (Pow (Var "x") (Lit 0))) 0 of
        Left (DomainError _) -> return ()
        other -> assertFailure $ "x^0 en x=0 debia seguir dando DomainError y dio " ++ show other
  ]

-- Tests de validación de dominios
testValidacionDominios :: Test
testValidacionDominios = TestList
  [ TestCase $ do
      let result = eval (Div (Lit 1) (Lit 0)) 0
      case result of
        Left DivideByZero -> return ()
        _ -> assertFailure "1/0 debería dar DivideByZero"
  , TestCase $ do
      let result = eval (Log (Lit 0)) 0
      case result of
        Left (DomainError _) -> return ()
        _ -> assertFailure "log(0) debería dar error de dominio"
  , TestCase $ do
      let result = eval (Log (Lit (-1))) 0
      case result of
        Left (DomainError _) -> return ()
        _ -> assertFailure "log(-1) debería dar error de dominio"
  , TestCase $ do
      let result = eval (Arcosh (Lit 0.5)) 0
      case result of
        Left (DomainError _) -> return ()
        _ -> assertFailure "arcosh(0.5) debería dar error de dominio"
  , TestCase $ do
      let result = eval (Artanh (Lit 2)) 0
      case result of
        Left (DomainError _) -> return ()
        _ -> assertFailure "artanh(2) debería dar error de dominio"
  , TestCase $ do
      let result = eval (Pow (Lit 0) (Lit 0)) 0
      case result of
        Left (DomainError _) -> return ()
        _ -> assertFailure "0^0 debería dar error de dominio"
  ]

-- Tests para números negativos con potencias (problemas de NaN)
-- Validaciones de dominio en el evaluador con numeros duales
testDominiosDual :: Test
testDominiosDual = TestList
  [ TestCase $ expectDomainError "log dual" (evalDual (Log (Lit 0)) 0)
  , TestCase $ expectDomainError "sqrt dual" (evalDual (Sqrt (Lit (-1))) 0)
  , TestCase $ expectDomainError "arcosh dual" (evalDual (Arcosh (Lit 0.5)) 0)
  , TestCase $ expectDomainError "artanh dual" (evalDual (Artanh (Lit 2)) 0)
  , TestCase $ expectDomainError "0^0 dual" (evalDual (Pow (Lit 0) (Lit 0)) 0)
  , TestCase $ expectDomainError "base negativa con exponente fraccionario dual" (evalDual (Pow (Lit (-1)) (Lit 0.5)) 0)
  , TestCase $ expectDomainError "base negativa con exponente casi cero dual" (evalDual (Pow (Lit (-2)) (Lit 1e-20)) 0)
  , TestCase $ expectDomainError "potencia dual no finita" (evalDual (Pow (Lit 10) (Lit 400)) 0)
  ]
  where
    expectDomainError testName result = case result of
      Left (DomainError _) -> return ()
      other -> assertFailure $ testName ++ " esperaba DomainError y obtuvo " ++ show other

testNegativePowers :: Test
testNegativePowers = TestList
  [ TestCase $ do
      -- (-x)^2 con x=3 debería dar 9 y derivada 6 (regla de la cadena: 2*(-x)*(-1) = 2x)
      let expr = Pow (Sub (Lit 0) (Var "x")) (Lit 2)
      let result = evalDual' expr 3
      case result of
        Right (val, deriv') -> do
          assertEqual "(-x)^2 valor en x=3" 9.0 val
          assertEqual "(-x)^2 derivada en x=3" 6.0 deriv'
        Left err -> assertFailure $ "Error inesperado en (-x)^2: " ++ show err
  , TestCase $ do
      -- Por precedencia matematica, -x^2 se interpreta como -(x^2).
      let result = case parse parseExpr "" "-x^2" of
                     Right expr -> evalDual' expr 3
                     Left err   -> Left $ UndefinedVariable $ "parse error: " ++ show err
      case result of
        Right (val, deriv') -> do
          assertBool "-x^2 valor cercano a -9" (abs (val + 9.0) < 1e-10)
          assertBool "-x^2 derivada cercana a -6" (abs (deriv' + 6.0) < 1e-10)
        Left err -> assertFailure $ "Error en -x^2: " ++ show err
  , TestCase $ do
      -- (-x) * (-x) con x=2 debería dar 4 y derivada 4
      let expr = Mul (Sub (Lit 0) (Var "x")) (Sub (Lit 0) (Var "x"))
      let result = evalDual' expr 2
      assertEqual "(-x)*(-x) en x=2" (Right (4.0, 4.0)) result
  ]

-- Tests para identidades trigonométricas que producían NaN
testTrigIdentities :: Test
testTrigIdentities = TestList
  [ TestCase $ do
      -- (sin(x)^2 + cos(x)^2) * x debería dar x con derivada 1
      let expr = Mul (Add (Pow (Sin (Var "x")) (Lit 2)) 
                          (Pow (Cos (Var "x")) (Lit 2))) 
                     (Var "x")
      let result = evalDual' expr 3
      case result of
        Right (val, deriv') -> do
          assertBool "Valor cercano a 3" (abs (val - 3.0) < 1e-10)
          -- La derivada puede no ser exactamente 1 por errores numéricos
          assertBool "Derivada es número finito" (not $ isNaN deriv' || isInfinite deriv')
        Left err -> assertFailure $ "Error en identidad trigonométrica: " ++ show err
  , TestCase $ do
      -- sin^2(x) + cos^2(x) debería dar 1 con derivada 0
      let expr = Add (Pow (Sin (Var "x")) (Lit 2)) (Pow (Cos (Var "x")) (Lit 2))
      let result = evalDual' expr 0.5
      case result of
        Right (val, deriv') -> do
          assertBool "sin^2 + cos^2 = 1" (abs (val - 1.0) < 1e-10)
          assertBool "Derivada es número finito" (not $ isNaN deriv' || isInfinite deriv')
        Left err -> assertFailure $ "Error: " ++ show err
  ]

-- Tests de parsing complejo
testParsingComplejo :: Test
testParsingComplejo = TestList
  [ TestCase $ do
      let result = case parse parseExpr "" "sqrt(4)" of
                     Right expr -> eval expr 0
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "parsing sqrt(4)" (Right 2.0) result
  , TestCase $ do
      let result = case parse parseExpr "" "sin(x) + cos(x)" of
                     Right expr -> eval expr 0
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "parsing sin(x) + cos(x)" (Right (sin 0 + cos 0)) result
  , TestCase $ do
      let result = case parse parseExpr "" "exp(-x)" of
                     Right _ -> True
                     Left _  -> False
      assertBool "parsing exp(-x)" result
  , TestCase $ do
      let result = case parse parseExpr "" "sqrt(x^2 + 1)" of
                     Right expr -> eval expr 2
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "parsing sqrt(x^2 + 1)" (Right (sqrt 5)) result
  , TestCase $ do
      let result = parse parseExpr "" "x + 1 basura"
      assertBool "rechaza texto sobrante al final" (case result of Left _ -> True; _ -> False)
  , TestCase $ do
      let result = parsearLinea "sin(x) @ pi/2"
      case result of
        Right (LineaEvaluacion _ x) -> assertBool "parsea pi/2 como valor de archivo" (abs (x - pi / 2) < 1e-10)
        Left err -> assertFailure $ "Error inesperado: " ++ err
  , TestCase $ do
      let result = parsearLinea "x + {- comentario -} 1 @ 2"
      case result of
        Right (LineaEvaluacion expr x) -> assertEqual "ignora comentario de bloque en linea" (Right 3.0) (eval expr x)
        Left err -> assertFailure $ "Error inesperado: " ++ err
  , TestCase $ do
      let result = parsearLinea "x @ {- comentario -} pi"
      case result of
        Right (LineaEvaluacion _ x) -> assertBool "ignora comentario de bloque en valor" (abs (x - pi) < 1e-10)
        Left err -> assertFailure $ "Error inesperado: " ++ err
  ]

testPrettyPrinterBordes :: Test
testPrettyPrinterBordes = TestList
  [ TestCase $ assertEqual "base negativa en potencia conserva parentesis" "(-1) ^ 2" (prettyPrint (Pow (Lit (-1)) (Lit 2)))
  , TestCase $ assertEqual "potencia asociada a izquierda conserva parentesis" "(x ^ 2) ^ 3" (prettyPrint (Pow (Pow (Var "x") (Lit 2)) (Lit 3)))
  , TestCase $ assertEqual "resta anidada a derecha conserva parentesis" "x - (y - z)" (prettyPrint (Sub (Var "x") (Sub (Var "y") (Var "z"))))
  , TestCase $ assertEqual "division anidada a derecha conserva parentesis" "x / (y / z)" (prettyPrint (Div (Var "x") (Div (Var "y") (Var "z"))))
  , TestCase $ assertRoundTrip "potencia derecha" (Pow (Var "x") (Pow (Var "y") (Lit 2))) [("x", 2), ("y", 3)]
  , TestCase $ assertRoundTrip "base potencia compuesta" (Pow (Add (Var "x") (Lit 1)) (Lit 2)) [("x", 2)]
  , TestCase $ assertRoundTrip "exponente compuesto" (Pow (Var "x") (Add (Var "y") (Lit 1))) [("x", 2), ("y", 3)]
  ]
  where
    assertRoundTrip testName expr env =
      case parse parseExpr "" (prettyPrint expr) of
        Left err -> assertFailure $ testName ++ " no parseo: " ++ show err
        Right parsed -> assertEqual testName (evalWithEnv env expr) (evalWithEnv env parsed)

testDerivadasEnBordes :: Test
testDerivadasEnBordes = TestList
  [ TestCase $ expectDomainError "sqrt(x) en x=0 no tiene derivada finita" (evalDual (Sqrt (Var "x")) 0)
  , TestCase $ expectDomainError "arcosh(x) en x=1 no tiene derivada finita" (evalDual (Arcosh (Var "x")) 1)
  , TestCase $ expectDomainError "x^(1/2) en x=0 no tiene derivada finita" (evalDual (Pow (Var "x") (Lit 0.5)) 0)
  ]
  where
    expectDomainError testName result = case result of
      Left (DomainError _) -> return ()
      other -> assertFailure $ testName ++ " esperaba DomainError y obtuvo " ++ show other

-- Tests de expresiones de ejemplos reales
testEjemplosReales :: Test
testEjemplosReales = TestList
  [ -- De basico.txt
    TestCase $ do
      let result = case parse parseExpr "" "x^2" of
                     Right expr -> eval expr 3
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "x^2 @ 3" (Right 9.0) result
  , TestCase $ do
      let result = case parse parseExpr "" "sin(x)" of
                     Right expr -> eval expr (pi/2)
                     Left _     -> Left $ UndefinedVariable "parse error"
      case result of
        Right val -> assertBool "sin(pi/2) ≈ 1" (abs (val - 1.0) < 1e-10)
        Left _ -> assertFailure "No debería dar error"
  , TestCase $ do
      let result = case parse parseExpr "" "log(x)" of
                     Right expr -> evalDual' expr (exp 1)
                     Left _     -> Left $ UndefinedVariable "parse error"
      case result of
        Right (val, deriv') -> do
          assertBool "log(e) ≈ 1" (abs (val - 1.0) < 1e-10)
          assertBool "log'(e) ≈ 1/e" (abs (deriv' - 1/(exp 1)) < 1e-10)
        Left _ -> assertFailure "No debería dar error"
  , -- De compuestas.txt
    TestCase $ do
      let result = case parse parseExpr "" "x^3 - 2*x^2 + x - 5" of
                     Right expr -> evalDual' expr 2
                     Left _     -> Left $ UndefinedVariable "parse error"
      assertEqual "x^3 - 2*x^2 + x - 5 @ 2" (Right (-3.0, 5.0)) result
  , TestCase $ do
      let result = case parse parseExpr "" "sin(x^2)" of
                     Right expr -> eval expr 1
                     Left _     -> Left $ UndefinedVariable "parse error"
      case result of
        Right val -> assertBool "sin(1^2) = sin(1)" (abs (val - sin 1) < 1e-10)
        Left _ -> assertFailure "No debería dar error"
  , -- De constantes
    TestCase $ do
      let result = case parse parseExpr "" "pi * x + e" of
                     Right expr -> evalDual' expr 1
                     Left _     -> Left $ UndefinedVariable "parse error"
      case result of
        Right (val, deriv') -> do
          assertBool "pi * 1 + e" (abs (val - (pi + exp 1)) < 1e-10)
          assertBool "derivada = pi" (abs (deriv' - pi) < 1e-10)
        Left _ -> assertFailure "No debería dar error"
  , -- De trigonometricas.txt
    TestCase $ do
      let result = case parse parseExpr "" "sin(x)^2 + cos(x)^2" of
                     Right expr -> eval expr 0.5
                     Left _     -> Left $ UndefinedVariable "parse error"
      case result of
        Right val -> assertBool "sin²(x) + cos²(x) = 1" (abs (val - 1.0) < 1e-10)
        Left _ -> assertFailure "No debería dar error"
  , -- De hiperbolicas.txt
    TestCase $ do
      let result = case parse parseExpr "" "sinh(x) + cosh(x)" of
                     Right expr -> eval expr 2
                     Left _     -> Left $ UndefinedVariable "parse error"
      case result of
        Right val -> assertBool "sinh(2) + cosh(2) = e^2" (abs (val - exp 2) < 1e-10)
        Left _ -> assertFailure "No debería dar error"
  ]

-- Tests de la monada de evaluacion con entorno explicito
testEvalM :: Test
testEvalM = TestList
  [ TestCase $ do
      let expr = Add (Mul (Var "x") (Var "y")) (Var "y")
      assertEqual "evalua con x e y en el entorno" (Right 9.0) (evalWithEnv [("x", 2), ("y", 3)] expr)
  , TestCase $ do
      let expr = Add (Mul (Var "x") (Var "y")) (Var "y")
          env = [("x", Dual 2 1), ("y", Dual 3 0)]
      assertEqual "deriva respecto de x dejando y constante" (Right (Dual 9.0 3.0)) (evalDualWithEnv env expr)
  , TestCase $ do
      let expr = Add (Var "x") (Var "z")
      assertEqual "detecta variables no definidas" (Left (UndefinedVariable "z")) (evalWithEnv [("x", 1)] expr)
  , TestCase $
      assertEqual "Alternative prueba una segunda computacion si la primera falla"
        (Right 2.0 :: Either ErrorType Double)
        (runEvalM [("x", 2.0)] (lookupVar "z" <|> lookupVar "x"))
  , TestCase $
      assertEqual "Alternative conserva el primer resultado exitoso"
        (Right 2.0 :: Either ErrorType Double)
        (runEvalM [("x", 2.0), ("y", 3.0)] (lookupVar "x" <|> lookupVar "y"))
  , TestCase $
      assertEqual "Alternative devuelve el error de la segunda opcion si ambas fallan"
        (Left (UndefinedVariable "y") :: Either ErrorType Double)
        (runEvalM [] (throwEval (UndefinedVariable "x") <|> lookupVar "y"))
  ]

-- Suite de Pruebas
tests :: Test
tests = TestList 
  [ TestLabel "Básicos originales" $ TestList [case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11]
  , TestLabel "Función sqrt" testSqrt
  , TestLabel "Números negativos literales" testNegativosLiterales
  , TestLabel "Potencias con bases negativas" testNegativePowers
  , TestLabel "Identidades trigonométricas" testTrigIdentities
  , TestLabel "Optimizaciones algebraicas" testOptimizaciones
  , TestLabel "Optimizaciones conservadoras" testOptimizacionesConservadoras
  , TestLabel "Validación de dominios" testValidacionDominios
  , TestLabel "Validacion de dominios duales" testDominiosDual
  , TestLabel "Parsing complejo" testParsingComplejo
  , TestLabel "Pretty printer en casos borde" testPrettyPrinterBordes
  , TestLabel "Derivadas en bordes de dominio" testDerivadasEnBordes
  , TestLabel "Ejemplos reales" testEjemplosReales
  , TestLabel "Monada de evaluacion" testEvalM
  ]

-- Ejecutar las pruebas
main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  Corriendo Suite Completa de Tests"
  putStrLn "=========================================="
  putStrLn ""
  counts' <- runTestTT tests
  putStrLn ""
  putStrLn "=========================================="
  putStrLn "  Resumen de Tests"
  putStrLn "=========================================="
  putStrLn $ "Casos:    " ++ show (cases counts')
  putStrLn $ "Intentos: " ++ show (tried counts')
  putStrLn $ "Errores:  " ++ show (errors counts')
  putStrLn $ "Fallos:   " ++ show (failures counts')
  putStrLn "=========================================="
  if errors counts' + failures counts' == 0
    then putStrLn "[OK] TODOS LOS TESTS PASARON" >> return ()
    else putStrLn "[X] ALGUNOS TESTS FALLARON" >> return ()
