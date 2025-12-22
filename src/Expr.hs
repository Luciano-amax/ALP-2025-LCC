module Expr where

-- Representación abstracta de expresiones matemáticas (AST)
data Expr
  = Lit Double              -- Constante numérica (e.g., 3.0)
  | Var String              -- Variable (e.g., "x")
  | Add Expr Expr           -- Suma (e.g., x + y)
  | Sub Expr Expr           -- Resta (e.g., x - y)
  | Mul Expr Expr           -- Multiplicación (e.g., x * y)
  | Div Expr Expr           -- División (e.g., x / y)
  | Pow Expr Expr           -- Exponenciación (e.g., x^y)
  | Sin Expr                -- Seno (e.g., sin(x))
  | Cos Expr                -- Coseno (e.g., cos(x))
  | Tan Expr                -- Tangente (e.g., tan(x))
  | Sinh Expr               -- **Seno hiperbólico**
  | Cosh Expr               -- **Coseno hiperbólico**
  | Tanh Expr               -- **Tangente hiperbólica**
  | Arsinh Expr             -- **Seno hiperbólico inverso**
  | Arcosh Expr             -- **Coseno hiperbólico inverso**
  | Artanh Expr             -- **Tangente hiperbólica inversa**
  deriving (Eq, Show)