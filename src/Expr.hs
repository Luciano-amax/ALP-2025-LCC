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
  deriving (Eq, Show)