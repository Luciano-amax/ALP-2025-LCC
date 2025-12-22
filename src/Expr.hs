module Expr where

-- Representación abstracta de expresiones matemáticas (AST)
data Expr
  = Lit Double             -- Constantes literales, como 3.0
  | Var String             -- Variables, como "x"
  | Add Expr Expr          -- Suma: expr1 + expr2
  | Sub Expr Expr          -- Resta: expr1 - expr2
  | Mul Expr Expr          -- Multiplicación: expr1 * expr2
  | Div Expr Expr          -- División: expr1 / expr2
  | Pow Expr Expr          -- Potencia: expr1 ^ expr2
  | Sin Expr               -- Función seno: sin(x)
  | Cos Expr               -- Función coseno: cos(x)
  | Tan Expr               -- Función tangente: tan(x)
  | Exp Expr               -- Función exponencial: exp(x)
  | Log Expr               -- Función logaritmo natural: log(x)
  deriving (Show, Eq)