module Evaluator (Dual(..), EvalResult, ErrorType(..), eval, evalDual) where

import Expr

data ErrorType
  = DivideByZero
  | UndefinedVariable String
  | DomainError String
  deriving (Show, Eq)

type EvalResult = Either ErrorType Double

-- Evalúa una expresión en un punto dado
eval :: Expr -> Double -> EvalResult
eval (Lit x) _ = return x
eval (Var v) x
  | v == "pi" = return pi
  | v == "e"  = return (exp 1)
  | v == "x"  = return x
  | otherwise = Left $ UndefinedVariable v
eval (Add e1 e2) x = (+) <$> eval e1 x <*> eval e2 x
eval (Sub e1 e2) x = (-) <$> eval e1 x <*> eval e2 x
eval (Mul e1 e2) x = (*) <$> eval e1 x <*> eval e2 x
eval (Div e1 e2) x = do
  v1 <- eval e1 x
  v2 <- eval e2 x
  if abs v2 < 1e-15
    then Left DivideByZero
    else return (v1 / v2)
eval (Pow e1 e2) x = do
  base <- eval e1 x
  expn <- eval e2 x
  if abs base < 1e-15 then
    if expn <= 0
      then Left $ DomainError "0^0 or 0^negative is undefined"
      else return 0
  else if abs (base - 1) < 1e-15 then  -- base ≈ 1
    return 1
  else if abs expn < 1e-15 then  -- expn ≈ 0
    return 1
  else if base < 0 && not (abs (expn - fromIntegral (round expn :: Integer)) < 1e-10)
    then Left $ DomainError "Negative base with fractional exponent"
    else return (base ** expn)
eval (Sin e) x = sin <$> eval e x
eval (Cos e) x = cos <$> eval e x
eval (Tan e) x = do
  v <- eval e x
  if abs (cos v) < 1e-15  -- Usar epsilon para comparar con 0
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
        -- Caso especial: base negativa con exponente entero
        isInteger = abs (p2 - fromIntegral (round p2 :: Integer)) < 1e-10
    in if isNaN result || isInfinite result
       then error "power: result is NaN or Infinite"
       -- Caso especial: base = 0
       else if abs p1 < 1e-15
         then if p2 > 1
           then Dual 0 0  -- 0^n con n>1: tanto valor como derivada son 0
           else if abs (p2 - 1) < 1e-15
             then Dual 0 d1  -- 0^1: derivada es d1
             else Dual 0 0  -- otros casos con base 0
       else if p1 < 0 && isInteger
         -- Para base negativa con exponente entero: d/dx[f^n] = n * f^(n-1) * f'
         then let n = round p2 :: Integer
                  deriv' = fromIntegral n * (p1 ** fromIntegral (n - 1)) * d1
              in Dual result deriv'
         -- Caso especial: exponente constante (d2 = 0)
         else if abs d2 < 1e-15 && p1 > 0
           then Dual result (p2 * (p1 ** (p2 - 1)) * d1)
         -- Fórmula general: d/dx[f^g] = f^g * (g' * ln(f) + f' * g / f)
         else if p1 > 0
           then Dual result (result * (d2 * log p1 + d1 * p2 / p1))
         else error "power: negative base with non-integer exponent"

-- Calcula valor y derivada usando diferenciación automática con números duales
evalDual :: Expr -> Double -> Either ErrorType Dual
evalDual (Lit n) _ = pure $ Dual n 0
evalDual (Var v) x
  | v == "pi" = pure $ Dual pi 0
  | v == "e"  = pure $ Dual (exp 1) 0
  | v == "x"  = pure $ Dual x 1
  | otherwise = Left $ UndefinedVariable v

evalDual (Add e1 e2) x = (+) <$> evalDual e1 x <*> evalDual e2 x
evalDual (Sub e1 e2) x = (-) <$> evalDual e1 x <*> evalDual e2 x
evalDual (Mul e1 e2) x = (*) <$> evalDual e1 x <*> evalDual e2 x

evalDual (Div e1 e2) x = do
  d1 <- evalDual e1 x
  d2 <- evalDual e2 x
  if abs (primal d2) < 1e-15
    then Left DivideByZero
    else pure $ d1 / d2

evalDual (Pow e1 e2) x = do
  d1 <- evalDual e1 x
  d2 <- evalDual e2 x
  let p1 = primal d1
      p2 = primal d2
      isInteger = abs (p2 - fromIntegral (round p2 :: Integer)) < 1e-10
  if p1 < 0 && not isInteger
    then Left $ DomainError "Negative base with fractional exponent"
    else if p1 == 0 && p2 <= 0
      then Left $ DomainError "0^0 or 0^negative is undefined"
      else pure $ d1 ** d2

evalDual (Sin e) x = sin <$> evalDual e x
evalDual (Cos e) x = cos <$> evalDual e x
evalDual (Tan e) x = do
  d <- evalDual e x
  let p = primal d
  if abs (cos p) < 1e-15
    then Left $ DomainError "tan undefined at this point"
    else let sec2 = 1 / (cos p ** 2)
         in pure $ Dual (tan p) (deriv d * sec2)

evalDual (Exp e) x = exp <$> evalDual e x

evalDual (Log e) x = do
  d <- evalDual e x
  if primal d <= 0
    then Left $ DomainError "log requires positive argument"
    else pure $ log d

evalDual (Sinh e) x = sinh <$> evalDual e x
evalDual (Cosh e) x = cosh <$> evalDual e x
evalDual (Tanh e) x = do
  d <- evalDual e x
  let p = primal d
      sech2 = 1 / (cosh p ** 2)
  pure $ Dual (tanh p) (deriv d * sech2)

evalDual (Arsinh e) x = asinh <$> evalDual e x

evalDual (Sqrt e) x = do
  d <- evalDual e x
  let p = primal d
  if p < 0
    then Left $ DomainError "sqrt requires non-negative argument"
    else if p == 0
      then pure $ Dual 0 0
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