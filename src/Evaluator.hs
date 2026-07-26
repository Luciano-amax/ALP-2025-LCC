module Evaluator
  ( Dual(..)
  , EvalResult
  , ErrorType(..)
  , eval
  , evalWithEnv
  , evalDual
  , evalDualWithEnv
  ) where

import Expr
import EvalM
import NumericPolicy

-- Evalua una expresion usando x como unica variable libre.
eval :: Expr -> Double -> EvalResult
eval expr x = evalWithEnv [("x", x)] expr

evalWithEnv :: EvalEnv Double -> Expr -> EvalResult
evalWithEnv env expr = runEvalM env (evalM expr)

-- Interpretacion escalar: calcula solo el valor numerico de la expresion.
evalM :: Expr -> EvalM Double Double
evalM (Lit x) = return x
evalM (Var v)
  | Just value <- constante v = return value
  | otherwise = lookupVar v
evalM (Add e1 e2) = evalBinaria (+) e1 e2
evalM (Sub e1 e2) = evalBinaria (-) e1 e2
evalM (Mul e1 e2) = evalBinaria (*) e1 e2
evalM (Div e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  if esCasiCero v2
    then throwEval DivideByZero
    else return (v1 / v2)
evalM (Pow e1 e2) = do
  base <- evalM e1
  expn <- evalM e2
  -- La potencia real tiene casos parciales que Haskell podria devolver como NaN.
  if base == 0 then
    if expn <= 0
      then throwEval $ DomainError "0^0 o potencia negativa de cero no esta definida"
      else return 0
  else if base == 1 then
    return 1
  else if expn == 0 then
    return 1
  else if base < 0 && not (esEntero expn)
    then throwEval $ DomainError "base negativa con exponente fraccionario"
    else
      let result = base ** expn
      in if isNaN result || isInfinite result
         then throwEval $ DomainError "resultado de potencia no finito"
         else return result
evalM (Sin e) = evalUnaria sin e
evalM (Cos e) = evalUnaria cos e
evalM (Tan e) = do
  v <- evalM e
  -- Cerca de pi/2 + k*pi la tangente no es estable para este DSL.
  if esCasiCero (cos v)
    then throwEval $ DomainError "tan no esta definida en este punto"
    else return (tan v)
evalM (Log e) = do
  v <- evalM e
  if v <= 0
    then throwEval $ DomainError "log requiere argumento positivo"
    else return (log v)
evalM (Exp e) = evalUnaria exp e
evalM (Sinh e) = evalUnaria sinh e
evalM (Cosh e) = evalUnaria cosh e
evalM (Tanh e) = evalUnaria tanh e
evalM (Sqrt e) = do
  v <- evalM e
  if v < 0
    then throwEval $ DomainError "sqrt requiere argumento no negativo"
    else return (sqrt v)
evalM (Arsinh e) = evalUnaria asinh e
evalM (Arcosh e) = do
  value <- evalM e
  if value < 1
    then throwEval $ DomainError "arcosh requiere argumento >= 1"
    else return (acosh value)
evalM (Artanh e) = do
  value <- evalM e
  if abs value >= 1
    then throwEval $ DomainError "artanh requiere |x| < 1"
    else return (atanh value)

evalBinaria :: (Double -> Double -> Double) -> Expr -> Expr -> EvalM Double Double
evalBinaria f e1 e2 =
  evalM e1 >>= \v1 ->
  evalM e2 >>= \v2 ->
  return (f v1 v2)

evalUnaria :: (Double -> Double) -> Expr -> EvalM Double Double
evalUnaria f e =
  evalM e >>= \v ->
  return (f v)

-- Numero dual: valor primal y derivada asociada.
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
  Dual p1 d1 / Dual p2 d2 =
    Dual (p1 / p2) ((p2 * d1 - p1 * d2) / (p2 * p2))
  fromRational r = Dual (fromRational r) 0

instance Floating Dual where
  pi = Dual pi 0
  exp (Dual p d) = Dual (exp p) (exp p * d)
  log (Dual p d)
    | p <= 0 = error "log: domain error in dual number"
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
    | p < 1 = error "acosh: domain error in dual number"
    | otherwise = Dual (acosh p) (d / sqrt (p * p - 1))
  atanh (Dual p d)
    | abs p >= 1 = error "atanh: domain error in dual number"
    | otherwise = Dual (atanh p) (d / (1 - p * p))
  sqrt (Dual p d)
    | p < 0 = error "sqrt: domain error in dual number"
    | p == 0 = Dual 0 0
    | otherwise = Dual (sqrt p) (d / (2 * sqrt p))
  (**) (Dual p1 d1) (Dual p2 d2) =
    let result = p1 ** p2
        isInteger = esEntero p2
    -- Esta instancia es parcial; el evaluador monadico valida antes de llamarla.
    in if isNaN result || isInfinite result
       then error "power: result is NaN or Infinite"
       else if p1 == 0
         then if p2 > 1
           then Dual 0 0
             else if esCasiCero (p2 - 1)
             then Dual 0 d1
             else Dual 0 0
       else if p1 < 0 && isInteger
         then let n = round p2 :: Integer
                  deriv' = fromIntegral n * (p1 ** fromIntegral (n - 1)) * d1
              in Dual result deriv'
       else if esCasiCero d2 && p1 > 0
         then Dual result (p2 * (p1 ** (p2 - 1)) * d1)
       else if p1 > 0
         then Dual result (result * (d2 * log p1 + d1 * p2 / p1))
       else error "power: negative base with non-integer exponent"

-- Operaciones seguras usadas por el evaluador monadico.
safeLogDual :: Dual -> EvalM Dual Dual
safeLogDual d
  | primal d <= 0 = throwEval $ DomainError "log requiere argumento positivo"
  | otherwise = return $ log d

safeSqrtDual :: Dual -> EvalM Dual Dual
safeSqrtDual d
  | primal d < 0 = throwEval $ DomainError "sqrt requiere argumento no negativo"
  -- sqrt(0) existe, pero su derivada no es finita si la entrada varia.
  | esCasiCero (primal d) && not (esCasiCero (deriv d)) =
      throwEval $ DomainError "derivada de sqrt no definida en 0"
  | esCasiCero (primal d) = return $ Dual 0 0
  | otherwise = return $ sqrt d

safeAcoshDual :: Dual -> EvalM Dual Dual
safeAcoshDual d
  | primal d < 1 = throwEval $ DomainError "arcosh requiere argumento >= 1"
  | esCasiCero (primal d - 1) && not (esCasiCero (deriv d)) =
      throwEval $ DomainError "derivada de arcosh no definida en 1"
  | esCasiCero (primal d - 1) = return $ Dual 0 0
  | otherwise = return $ acosh d

safeAtanhDual :: Dual -> EvalM Dual Dual
safeAtanhDual d
  | abs (primal d) >= 1 = throwEval $ DomainError "artanh requiere |x| < 1"
  | otherwise = return $ atanh d

safePowDual :: Dual -> Dual -> EvalM Dual Dual
safePowDual d1 d2
  | p1 == 0 && p2 <= 0 = throwEval $ DomainError "0^0 o potencia negativa de cero no esta definida"
  | p1 == 0 && p2 < 1 && not (esCasiCero (deriv d1)) =
      throwEval $ DomainError "derivada de potencia no definida en base cero"
  | p1 < 0 && not isInteger = throwEval $ DomainError "base negativa con exponente fraccionario"
  | p1 < 0 && not (esCasiCero (deriv d2)) =
      throwEval $ DomainError "base negativa con exponente variable no es real"
  | isNaN result || isInfinite result =
      throwEval $ DomainError "resultado de potencia no finito"
  | otherwise = return $ d1 ** d2
  where
    p1 = primal d1
    p2 = primal d2
    isInteger = esEntero p2
    result = p1 ** p2

-- Por defecto se deriva respecto de x.
evalDual :: Expr -> Double -> Either ErrorType Dual
evalDual expr x = evalDualWithEnv [("x", Dual x 1)] expr

evalDualWithEnv :: EvalEnv Dual -> Expr -> Either ErrorType Dual
evalDualWithEnv env expr = runEvalM env (evalDualM expr)

-- Interpretacion dual: el primal da f(x) y deriv acumula f'(x).
evalDualM :: Expr -> EvalM Dual Dual
evalDualM (Lit n) = return $ Dual n 0
evalDualM (Var v)
  | Just value <- constante v = return value
  | otherwise = lookupVar v
evalDualM (Add e1 e2) = evalDualBinaria (+) e1 e2
evalDualM (Sub e1 e2) = evalDualBinaria (-) e1 e2
evalDualM (Mul e1 e2) = evalDualBinaria (*) e1 e2
evalDualM (Div e1 e2) = do
  d1 <- evalDualM e1
  d2 <- evalDualM e2
  if esCasiCero (primal d2)
    then throwEval DivideByZero
    else return $ d1 / d2
evalDualM (Pow e1 e2) = do
  d1 <- evalDualM e1
  d2 <- evalDualM e2
  safePowDual d1 d2
evalDualM (Sin e) = evalDualUnaria sin e
evalDualM (Cos e) = evalDualUnaria cos e
evalDualM (Tan e) = do
  d <- evalDualM e
  let p = primal d
  if esCasiCero (cos p)
    then throwEval $ DomainError "tan no esta definida en este punto"
    else let sec2 = 1 / (cos p ** 2)
         in return $ Dual (tan p) (deriv d * sec2)
evalDualM (Exp e) = evalDualUnaria exp e
evalDualM (Log e) = do
  d <- evalDualM e
  safeLogDual d
evalDualM (Sinh e) = evalDualUnaria sinh e
evalDualM (Cosh e) = evalDualUnaria cosh e
evalDualM (Tanh e) = do
  d <- evalDualM e
  let p = primal d
      sech2 = 1 / (cosh p ** 2)
  return $ Dual (tanh p) (deriv d * sech2)
evalDualM (Arsinh e) = evalDualUnaria asinh e
evalDualM (Sqrt e) = do
  d <- evalDualM e
  safeSqrtDual d
evalDualM (Arcosh e) = do
  d <- evalDualM e
  safeAcoshDual d
evalDualM (Artanh e) = do
  d <- evalDualM e
  safeAtanhDual d

evalDualBinaria :: (Dual -> Dual -> Dual) -> Expr -> Expr -> EvalM Dual Dual
evalDualBinaria f e1 e2 =
  evalDualM e1 >>= \v1 ->
  evalDualM e2 >>= \v2 ->
  return (f v1 v2)

evalDualUnaria :: (Dual -> Dual) -> Expr -> EvalM Dual Dual
evalDualUnaria f e =
  evalDualM e >>= \v ->
  return (f v)

constante :: Floating a => String -> Maybe a
constante "pi" = Just pi
constante "e" = Just (exp 1)
constante _ = Nothing
