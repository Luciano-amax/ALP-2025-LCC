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

epsilon :: Double
epsilon = 1e-15

isAlmostZero :: Double -> Bool
isAlmostZero value = abs value < epsilon

isIntegerValue :: Double -> Bool
isIntegerValue value =
  let rounded = fromIntegral (round value :: Integer)
  in if rounded == 0
     then value == 0
     else abs (value - rounded) < 1e-10

-- Evalua una expresion usando x como unica variable libre.
eval :: Expr -> Double -> EvalResult
eval expr x = evalWithEnv [("x", x)] expr

evalWithEnv :: EvalEnv Double -> Expr -> EvalResult
evalWithEnv env expr = runEvalM env (evalM expr)

evalM :: Expr -> EvalM Double Double
evalM (Lit x) = pure x
evalM (Var v)
  | v == "pi" = pure pi
  | v == "e" = pure (exp 1)
  | otherwise = lookupVar v
evalM (Add e1 e2) = (+) <$> evalM e1 <*> evalM e2
evalM (Sub e1 e2) = (-) <$> evalM e1 <*> evalM e2
evalM (Mul e1 e2) = (*) <$> evalM e1 <*> evalM e2
evalM (Div e1 e2) = do
  v1 <- evalM e1
  v2 <- evalM e2
  if isAlmostZero v2
    then throwEval DivideByZero
    else pure (v1 / v2)
evalM (Pow e1 e2) = do
  base <- evalM e1
  expn <- evalM e2
  if base == 0 then
    if expn <= 0
      then throwEval $ DomainError "0^0 or 0^negative is undefined"
      else pure 0
  else if base == 1 then
    pure 1
  else if expn == 0 then
    pure 1
  else if base < 0 && not (isIntegerValue expn)
    then throwEval $ DomainError "Negative base with fractional exponent"
    else
      let result = base ** expn
      in if isNaN result || isInfinite result
         then throwEval $ DomainError "power result is not finite"
         else pure result
evalM (Sin e) = sin <$> evalM e
evalM (Cos e) = cos <$> evalM e
evalM (Tan e) = do
  v <- evalM e
  if isAlmostZero (cos v)
    then throwEval $ DomainError "tan undefined at this input"
    else pure (tan v)
evalM (Log e) = do
  v <- evalM e
  if v <= 0
    then throwEval $ DomainError "Log domain error"
    else pure (log v)
evalM (Exp e) = exp <$> evalM e
evalM (Sinh e) = sinh <$> evalM e
evalM (Cosh e) = cosh <$> evalM e
evalM (Tanh e) = tanh <$> evalM e
evalM (Sqrt e) = do
  v <- evalM e
  if v < 0
    then throwEval $ DomainError "sqrt requires non-negative argument"
    else pure (sqrt v)
evalM (Arsinh e) = asinh <$> evalM e
evalM (Arcosh e) = do
  value <- evalM e
  if value < 1
    then throwEval $ DomainError "arcosh requires argument >= 1"
    else pure (acosh value)
evalM (Artanh e) = do
  value <- evalM e
  if abs value >= 1
    then throwEval $ DomainError "artanh requires |x| < 1"
    else pure (atanh value)

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
        isInteger = isIntegerValue p2
    in if isNaN result || isInfinite result
       then error "power: result is NaN or Infinite"
       else if p1 == 0
         then if p2 > 1
           then Dual 0 0
           else if isAlmostZero (p2 - 1)
             then Dual 0 d1
             else Dual 0 0
       else if p1 < 0 && isInteger
         then let n = round p2 :: Integer
                  deriv' = fromIntegral n * (p1 ** fromIntegral (n - 1)) * d1
              in Dual result deriv'
       else if isAlmostZero d2 && p1 > 0
         then Dual result (p2 * (p1 ** (p2 - 1)) * d1)
       else if p1 > 0
         then Dual result (result * (d2 * log p1 + d1 * p2 / p1))
       else error "power: negative base with non-integer exponent"

-- Operaciones seguras usadas por el evaluador monadico.
safeLogDual :: Dual -> EvalM Dual Dual
safeLogDual d
  | primal d <= 0 = throwEval $ DomainError "log requires positive argument"
  | otherwise = pure $ log d

safeSqrtDual :: Dual -> EvalM Dual Dual
safeSqrtDual d
  | primal d < 0 = throwEval $ DomainError "sqrt requires non-negative argument"
  | isAlmostZero (primal d) && not (isAlmostZero (deriv d)) =
      throwEval $ DomainError "sqrt derivative is undefined at 0"
  | isAlmostZero (primal d) = pure $ Dual 0 0
  | otherwise = pure $ sqrt d

safeAcoshDual :: Dual -> EvalM Dual Dual
safeAcoshDual d
  | primal d < 1 = throwEval $ DomainError "arcosh requires argument >= 1"
  | isAlmostZero (primal d - 1) && not (isAlmostZero (deriv d)) =
      throwEval $ DomainError "arcosh derivative is undefined at 1"
  | isAlmostZero (primal d - 1) = pure $ Dual 0 0
  | otherwise = pure $ acosh d

safeAtanhDual :: Dual -> EvalM Dual Dual
safeAtanhDual d
  | abs (primal d) >= 1 = throwEval $ DomainError "artanh requires |x| < 1"
  | otherwise = pure $ atanh d

safePowDual :: Dual -> Dual -> EvalM Dual Dual
safePowDual d1 d2
  | p1 == 0 && p2 <= 0 = throwEval $ DomainError "0^0 or 0^negative is undefined"
  | p1 == 0 && p2 < 1 && not (isAlmostZero (deriv d1)) =
      throwEval $ DomainError "power derivative is undefined at zero base"
  | p1 < 0 && not isInteger = throwEval $ DomainError "Negative base with fractional exponent"
  | p1 < 0 && not (isAlmostZero (deriv d2)) =
      throwEval $ DomainError "Negative base with variable exponent is not real-valued"
  | isNaN result || isInfinite result =
      throwEval $ DomainError "power result is not finite"
  | otherwise = pure $ d1 ** d2
  where
    p1 = primal d1
    p2 = primal d2
    isInteger = isIntegerValue p2
    result = p1 ** p2

-- Por defecto se deriva respecto de x.
evalDual :: Expr -> Double -> Either ErrorType Dual
evalDual expr x = evalDualWithEnv [("x", Dual x 1)] expr

evalDualWithEnv :: EvalEnv Dual -> Expr -> Either ErrorType Dual
evalDualWithEnv env expr = runEvalM env (evalDualM expr)

evalDualM :: Expr -> EvalM Dual Dual
evalDualM (Lit n) = pure $ Dual n 0
evalDualM (Var v)
  | v == "pi" = pure $ Dual pi 0
  | v == "e" = pure $ Dual (exp 1) 0
  | otherwise = lookupVar v
evalDualM (Add e1 e2) = (+) <$> evalDualM e1 <*> evalDualM e2
evalDualM (Sub e1 e2) = (-) <$> evalDualM e1 <*> evalDualM e2
evalDualM (Mul e1 e2) = (*) <$> evalDualM e1 <*> evalDualM e2
evalDualM (Div e1 e2) = do
  d1 <- evalDualM e1
  d2 <- evalDualM e2
  if isAlmostZero (primal d2)
    then throwEval DivideByZero
    else pure $ d1 / d2
evalDualM (Pow e1 e2) = do
  d1 <- evalDualM e1
  d2 <- evalDualM e2
  safePowDual d1 d2
evalDualM (Sin e) = sin <$> evalDualM e
evalDualM (Cos e) = cos <$> evalDualM e
evalDualM (Tan e) = do
  d <- evalDualM e
  let p = primal d
  if isAlmostZero (cos p)
    then throwEval $ DomainError "tan undefined at this point"
    else let sec2 = 1 / (cos p ** 2)
         in pure $ Dual (tan p) (deriv d * sec2)
evalDualM (Exp e) = exp <$> evalDualM e
evalDualM (Log e) = do
  d <- evalDualM e
  safeLogDual d
evalDualM (Sinh e) = sinh <$> evalDualM e
evalDualM (Cosh e) = cosh <$> evalDualM e
evalDualM (Tanh e) = do
  d <- evalDualM e
  let p = primal d
      sech2 = 1 / (cosh p ** 2)
  pure $ Dual (tanh p) (deriv d * sech2)
evalDualM (Arsinh e) = asinh <$> evalDualM e
evalDualM (Sqrt e) = do
  d <- evalDualM e
  safeSqrtDual d
evalDualM (Arcosh e) = do
  d <- evalDualM e
  safeAcoshDual d
evalDualM (Artanh e) = do
  d <- evalDualM e
  safeAtanhDual d
