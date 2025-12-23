module Evaluator (Dual(..), EvalResult, ErrorType(..), eval, evalDual) where

import Expr                              -- Importamos el AST

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
  | v == "pi" = return pi                      -- Interpreta "pi" como la constante π
  | v == "e"  = return (exp 1)                 -- Interpreta "e" como la base de los logaritmos
  | v == "x"  = return x                       -- Única variable soportada
  | otherwise = Left $ UndefinedVariable v     -- Error si la variable no es "x", "pi" o "e"
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
  if base < 0 && not (expn == fromIntegral (round expn :: Integer))
    then Left $ (DomainError "Negative base with fractional exponent")
    else return (base ** expn)
eval (Sin e) x = sin <$> eval e x
eval (Cos e) x = cos <$> eval e x
eval (Tan e) x = do
  v <- eval e x
  if cos v == 0
    then Left $ (DomainError "tan undefined at this input")
    else return (tan v)
eval (Log e) x = do
  v <- eval e x
  if v <= 0
    then Left $ (DomainError "Log domain error")
    else return (log v)
eval (Exp e) x = exp <$> eval e x
eval (Sinh e) x = fmap sinh (eval e x)                          -- f(x) = sinh(g(x))
eval (Cosh e) x = fmap cosh (eval e x)                          -- f(x) = cosh(g(x))
eval (Tanh e) x = fmap tanh (eval e x)                          -- f(x) = tanh(g(x))
eval (Sqrt e) x = do
  v <- eval e x
  if v < 0
    then Left $ DomainError "sqrt requires non-negative argument"
    else return (sqrt v)
eval (Arsinh e) x = fmap asinh (eval e x)                       -- f(x) = arsinh(g(x))
eval (Arcosh e) x = do                                          
  value <- eval e x
  if value < 1 then Left (DomainError "valor fuera del dominio")                        -- Debe cumplirse x >= 1
  else Right (acosh value)                                     -- f(x) = arcosh(g(x))
eval (Artanh e) x = do                                          
  value <- eval e x
  if abs value >= 1 then Left (DomainError "valor fuera del dominio")                    -- Dominio: -1 < x < 1
  else Right (atanh value)                                     -- f(x) = artanh(g(x))


-- Números duales: Almacenan valor primal y derivada
data Dual = Dual { primal :: Double, deriv :: Double }
  deriving (Show, Eq)

-- Instancias para hacer Dual más utilizable con operadores estándar
instance Num Dual where
  Dual p1 d1 + Dual p2 d2 = Dual (p1 + p2) (d1 + d2)
  Dual p1 d1 - Dual p2 d2 = Dual (p1 - p2) (d1 - d2)
  Dual p1 d1 * Dual p2 d2 = Dual (p1 * p2) (p1 * d2 + p2 * d1)
  negate (Dual p d) = Dual (negate p) (negate d)
  abs (Dual p d) = Dual (abs p) (signum p * d)
  signum (Dual p _) = Dual (signum p) 0
  fromInteger n = Dual (fromInteger n) 0

instance Fractional Dual where
  Dual p1 d1 / Dual p2 d2 = Dual (p1 / p2) ((p2 * d1 - p1 * d2) / (p2 * p2))
  fromRational r = Dual (fromRational r) 0

instance Floating Dual where
  pi = Dual pi 0
  exp (Dual p d) = Dual (exp p) (exp p * d)
  log (Dual p d) 
    | p <= 0    = error "log: domain error in dual number"
    | otherwise = Dual (log p) (d / p)
  sin (Dual p d) = Dual (sin p) (cos p * d)
  cos (Dual p d) = Dual (cos p) (-sin p * d)
  sinh (Dual p d) = Dual (sinh p) (cosh p * d)
  cosh (Dual p d) = Dual (cosh p) (sinh p * d)
  asin (Dual p d) 
    | abs p > 1 = error "asin: domain error in dual number"
    | otherwise = Dual (asin p) (d / sqrt (1 - p * p))
  acos (Dual p d)
    | abs p > 1 = error "acos: domain error in dual number" 
    | otherwise = Dual (acos p) (-d / sqrt (1 - p * p))
  atan (Dual p d) = Dual (atan p) (d / (1 + p * p))
  asinh (Dual p d) = Dual (asinh p) (d / sqrt (p * p + 1))
  acosh (Dual p d)
    | p < 1     = error "acosh: domain error in dual number"
    | otherwise = Dual (acosh p) (d / sqrt (p * p - 1))
  atanh (Dual p d)
    | abs p >= 1 = error "atanh: domain error in dual number"
    | otherwise  = Dual (atanh p) (d / (1 - p * p))
  sqrt (Dual p d)
    | p < 0     = error "sqrt: domain error in dual number"
    | p == 0    = Dual 0 0  -- Caso especial: derivada en 0 es infinita, pero devolvemos 0
    | otherwise = Dual (sqrt p) (d / (2 * sqrt p))
  (**) (Dual p1 d1) (Dual p2 d2) = 
    let result = p1 ** p2
    in if isNaN result || isInfinite result
       then error "power: result is NaN or Infinite"
       else Dual result (result * (d2 * log p1 + d1 * p2 / p1))

-- Evaluación dual: calcular valor y derivada simultáneamente
-- Usando las instancias de Num/Fractional/Floating para código más limpio
evalDual :: Expr -> Double -> Either ErrorType Dual
evalDual (Lit n) _ = pure $ Dual n 0  -- BUG CORREGIDO: preservar decimales
evalDual (Var v) x
  | v == "pi" = pure $ Dual pi 0          -- Constante pi, derivada 0
  | v == "e"  = pure $ Dual (exp 1) 0     -- Constante e, derivada 0
  | v == "x"  = pure $ Dual x 1           -- Primal x, derivada 1
  | otherwise = Left $ UndefinedVariable v

-- Operaciones aritméticas usando las instancias
evalDual (Add e1 e2) x = (+) <$> evalDual e1 x <*> evalDual e2 x
evalDual (Sub e1 e2) x = (-) <$> evalDual e1 x <*> evalDual e2 x
evalDual (Mul e1 e2) x = (*) <$> evalDual e1 x <*> evalDual e2 x

evalDual (Div e1 e2) x = do
  d1 <- evalDual e1 x
  d2 <- evalDual e2 x
  if primal d2 == 0
    then Left DivideByZero
    else pure $ d1 / d2

evalDual (Pow e1 e2) x = do
  d1 <- evalDual e1 x
  d2 <- evalDual e2 x
  let p1 = primal d1
      p2 = primal d2
  if p1 < 0 && p2 /= fromIntegral (round p2 :: Integer)
    then Left $ DomainError "Negative base with fractional exponent"
    else if p1 == 0 && p2 <= 0
      then Left $ DomainError "0^0 or 0^negative is undefined"
      else pure $ d1 ** d2

-- Funciones trigonométricas usando las instancias
evalDual (Sin e) x = sin <$> evalDual e x
evalDual (Cos e) x = cos <$> evalDual e x
evalDual (Tan e) x = do
  d <- evalDual e x
  let p = primal d
  if cos p == 0
    then Left $ DomainError "tan undefined at this point"
    else let sec2 = 1 / (cos p ** 2)
         in pure $ Dual (tan p) (deriv d * sec2)

evalDual (Exp e) x = exp <$> evalDual e x

evalDual (Log e) x = do
  d <- evalDual e x
  if primal d <= 0
    then Left $ DomainError "log requires positive argument"
    else pure $ log d

-- Funciones hiperbólicas
evalDual (Sinh e) x = sinh <$> evalDual e x
evalDual (Cosh e) x = cosh <$> evalDual e x
evalDual (Tanh e) x = do
  d <- evalDual e x
  let p = primal d
      sech2 = 1 / (cosh p ** 2)
  pure $ Dual (tanh p) (deriv d * sech2)

-- Funciones hiperbólicas inversas con validación de dominio
evalDual (Arsinh e) x = asinh <$> evalDual e x

evalDual (Sqrt e) x = do
  d <- evalDual e x
  let p = primal d
  if p < 0
    then Left $ DomainError "sqrt requires non-negative argument"
    else if p == 0
      then pure $ Dual 0 0  -- Caso especial en x=0
      else pure $ sqrt d

evalDual (Arcosh e) x = do
  d <- evalDual e x
  if primal d < 1
    then Left $ DomainError "arcosh requires argument >= 1"
    else pure $ acosh d

evalDual (Artanh e) x = do
  d <- evalDual e x
  let p = primal d
  if abs p >= 1
    then Left $ DomainError "artanh requires |x| < 1"
    else pure $ atanh d