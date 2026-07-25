module Main where

import Evaluator
import Expr

expr :: Expr
expr = Add (Mul (Var "x") (Var "y")) (Var "y")

main :: IO ()
main = do
  -- Evaluacion con entorno explicito: x = 2, y = 3.
  print $ evalWithEnv [("x", 2), ("y", 3)] expr

  -- Derivacion respecto de x: y queda como constante.
  print $ evalDualWithEnv [("x", Dual 2 1), ("y", Dual 3 0)] expr
