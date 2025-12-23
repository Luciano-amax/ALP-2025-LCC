module Expr where

-- Representación abstracta de expresiones matemáticas (AST)
data Expr
  = Lit Double
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Sin Expr
  | Cos Expr
  | Tan Expr
  | Sinh Expr
  | Cosh Expr
  | Tanh Expr
  | Arsinh Expr
  | Arcosh Expr
  | Artanh Expr
  | Log Expr               -- Logaritmo natural
  | Exp Expr               -- Exponencial (e^x)
  | Sqrt Expr              -- Raíz cuadrada
  deriving (Eq, Show)

-- Función de optimización algebraica
optimize :: Expr -> Expr
optimize expr = case expr of
  -- Identidades aditivas: x + 0 = x, 0 + x = x
  Add (Lit 0) e -> optimize e
  Add e (Lit 0) -> optimize e
  
  -- Identidades multiplicativas: x * 1 = x, 1 * x = x
  Mul (Lit 1) e -> optimize e
  Mul e (Lit 1) -> optimize e
  
  -- Anulación: x * 0 = 0, 0 * x = 0
  Mul (Lit 0) _ -> Lit 0
  Mul _ (Lit 0) -> Lit 0
  
  -- Identidad sustractiva: x - 0 = x
  Sub e (Lit 0) -> optimize e
  
  -- Auto-resta: x - x = 0 (solo si son la misma expresión)
  Sub e1 e2 | e1 == e2 -> Lit 0
  
  -- Identidad de división: x / 1 = x
  Div e (Lit 1) -> optimize e
  
  -- Auto-división: x / x = 1 (solo si son la misma expresión)
  Div e1 e2 | e1 == e2 -> Lit 1
  
  -- Potencias especiales: x^0 = 1, x^1 = x
  Pow _ (Lit 0) -> Lit 1
  Pow e (Lit 1) -> optimize e
  
  -- Potencia de 1: 1^x = 1
  Pow (Lit 1) _ -> Lit 1
  
  -- Evaluar constantes
  Add (Lit a) (Lit b) -> Lit (a + b)
  Sub (Lit a) (Lit b) -> Lit (a - b)
  Mul (Lit a) (Lit b) -> Lit (a * b)
  Div (Lit a) (Lit b) | b /= 0 -> Lit (a / b)
  Pow (Lit a) (Lit b) -> Lit (a ** b)
  
  -- Recursión en subexpresiones
  Add e1 e2 -> let e1' = optimize e1
                   e2' = optimize e2
               in if e1 == e1' && e2 == e2' 
                  then Add e1' e2'
                  else optimize (Add e1' e2')
                  
  Sub e1 e2 -> let e1' = optimize e1
                   e2' = optimize e2
               in if e1 == e1' && e2 == e2'
                  then Sub e1' e2'
                  else optimize (Sub e1' e2')
                  
  Mul e1 e2 -> let e1' = optimize e1
                   e2' = optimize e2
               in if e1 == e1' && e2 == e2'
                  then Mul e1' e2'
                  else optimize (Mul e1' e2')
                  
  Div e1 e2 -> let e1' = optimize e1
                   e2' = optimize e2
               in if e1 == e1' && e2 == e2'
                  then Div e1' e2'
                  else optimize (Div e1' e2')
                  
  Pow e1 e2 -> let e1' = optimize e1
                   e2' = optimize e2
               in if e1 == e1' && e2 == e2'
                  then Pow e1' e2'
                  else optimize (Pow e1' e2')
  
  -- Funciones unarias
  Sin e -> Sin (optimize e)
  Cos e -> Cos (optimize e)
  Tan e -> Tan (optimize e)
  Sinh e -> Sinh (optimize e)
  Cosh e -> Cosh (optimize e)
  Tanh e -> Tanh (optimize e)
  Arsinh e -> Arsinh (optimize e)
  Arcosh e -> Arcosh (optimize e)
  Artanh e -> Artanh (optimize e)
  Log e -> Log (optimize e)
  Exp e -> Exp (optimize e)
  Sqrt e -> Sqrt (optimize e)
  
  -- Literales y variables no se optimizan
  _ -> expr