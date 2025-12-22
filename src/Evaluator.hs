module Evaluator (eval, evalDual, ErrorType(..)) where

import Expr                                -- Importamos el AST

-- Errores manejados durante la evaluación
data ErrorType
  = DivideByZero                          -- Error de división entre 0
  | UndefinedVariable String              -- Variable indefinida
  | DomainError String                    -- Errores de dominio matemático
  deriving (Show, Eq)

-- Tipo para manejar errores en contexto de evaluación
type EvalResult = Either ErrorType Double

-- Evaluación básica: valor primal de una expresión
eval :: Expr -> Double -> EvalResult
eval (Lit x) _ = return x                  -- Literal con valor directo
eval (Var v) x
  | v == "x"  = return x                   -- Única variable válida
  | otherwise = Left $ UndefinedVariable v
eval (Add e1 e2) x = (+) <$> eval e1 x <*> eval e2 x
eval (Sub e1 e2) x = (-) <$> eval e1 x <*> eval e2 x
eval (Mul e1 e2) x = (*) <$> eval e1 x <*> eval e2 x
eval (Div e1 e2) x = do
  v1 <- eval e1 x
  v2 <- eval e2 x
  if v2 == 0
    then Left DivideByZero
    else return (v1 / v2)
eval (Pow e1 e2) x = do
  base <- eval e1 x
  expn <- eval e2 x
  if base < 0 && not (expn == fromIntegral (round expn))
    then Left $ DomainError "Negative base with fractional exponent"
    else return (base ** expn)
eval (Sin e) x = sin <$> eval e x
eval (Cos e) x = cos <$> eval e x
eval (Tan e) x = do
  v <- eval e x
  if cos v == 0
    then Left $ DomainError "tan undefined at this input"
    else return (tan v)
eval (Log e) x = do
  v <- eval e x
  if v <= 0
    then Left $ DomainError "Log domain error"
    else return (log v)
eval (Exp e) x = exp <$> eval e x
eval (Sinh e) x = fmap sinh (eval e x)                          -- f(x) = sinh(g(x))
eval (Cosh e) x = fmap cosh (eval e x)                          -- f(x) = cosh(g(x))
eval (Tanh e) x = fmap tanh (eval e x)                          -- f(x) = tanh(g(x))
eval (Arsinh e) x = fmap asinh (eval e x)                       -- f(x) = arsinh(g(x))
eval (Arcosh e) x = do                                          
  value <- eval e x
  if value < 1 then Left InvalidDomain                         -- Debe cumplirse x >= 1
  else Right (acosh value)                                     -- f(x) = arcosh(g(x))
eval (Artanh e) x = do                                          
  value <- eval e x
  if abs value >= 1 then Left InvalidDomain                    -- Dominio: -1 < x < 1
  else Right (atanh value)                                     -- f(x) = artanh(g(x))


-- Números duales: Almacenan valor primal y derivada
data Dual = Dual { primal :: Double, deriv :: Double }
  deriving (Show, Eq)

-- Evaluación dual: calcular valor y derivada simultáneamente
evalDual :: Expr -> Double -> Either ErrorType Dual
evalDual (Lit x) _ = return $ Dual x 0
evalDual (Var v) x
  | v == "x"  = return $ Dual x 1           -- Primal x, derivada 1
  | otherwise = Left $ UndefinedVariable v
evalDual (Add e1 e2) x = do
  Dual p1 d1 <- evalDual e1 x
  Dual p2 d2 <- evalDual e2 x
  return $ Dual (p1 + p2) (d1 + d2)
evalDual (Sub e1 e2) x = do
  Dual p1 d1 <- evalDual e1 x
  Dual p2 d2 <- evalDual e2 x
  return $ Dual (p1 - p2) (d1 - d2)
evalDual (Mul e1 e2) x = do
  Dual p1 d1 <- evalDual e1 x
  Dual p2 d2 <- evalDual e2 x
  return $ Dual (p1 * p2) (p1 * d2 + p2 * d1)
evalDual (Div e1 e2) x = do
  Dual p1 d1 <- evalDual e1 x
  Dual p2 d2 <- evalDual e2 x
  if p2 == 0
    then Left DivideByZero
    else return $ Dual (p1 / p2) ((p2 * d1 - p1 * d2) / (p2 ^ 2))
evalDual (Pow e1 e2) x = do
  Dual p1 d1 <- evalDual e1 x
  Dual p2 d2 <- evalDual e2 x
  let primal' = p1 ** p2
  let deriv' = primal' * (d2 * log p1 + d1 * p2 / p1)
  return $ Dual primal' deriv'
evalDual (Sin e) x = do
  Dual p d <- evalDual e x
  return $ Dual (sin p) (cos p * d)
evalDual (Cos e) x = do
  Dual p d <- evalDual e x
  return $ Dual (cos p) (-sin p * d)
evalDual (Exp e) x = do
  Dual p d <- evalDual e x
  return $ Dual (exp p) (exp p * d)
evalDual (Log e) x = do
  Dual p d <- evalDual e x
  if p <= 0
    then Left $ UndefinedVariable "log domain"
    else return $ Dual (log p) (d / p)
evalDual (Sinh e) x = do
  Dual f g <- evalDual e x                  -- f = g(x), g = g'(x)
  return $ Dual (sinh f) (cosh f * g)       -- f(x) = sinh(f), f'(x) = cosh(f) * f'
evalDual (Cosh e) x = do
  Dual f g <- evalDual e x
  return $ Dual (cosh f) (sinh f * g)
evalDual (Tanh e) x = do
  Dual f g <- evalDual e x
  return $ Dual (tanh f) ((1 / (cosh f)^2) * g) -- Derivada: 1 / cosh^2 * g'
evalDual (Arsinh e) x = do
  Dual f g <- evalDual e x
  return $ Dual (asinh f) ((1 / sqrt (f^2 + 1)) * g)
evalDual (Arcosh e) x = do
  Dual f g <- evalDual e x
  if f < 1 then Left InvalidDomain
  else return $ Dual (acosh f) ((1 / sqrt (f^2 - 1)) * g)
evalDual (Artanh e) x = do
  Dual f g <- evalDual e x
  if abs f >= 1 then Left InvalidDomain
  else return $ Dual (atanh f) ((1 / (1 - f^2)) * g)