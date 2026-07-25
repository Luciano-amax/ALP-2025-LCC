module Expr where

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
  | Log Expr
  | Exp Expr
  | Sqrt Expr
  deriving (Eq, Show)

-- Simplifica sin esconder errores que el evaluador deberia reportar.
optimize :: Expr -> Expr
optimize expr = case expr of
  Add (Lit a) (Lit b) -> Lit (a + b)
  Sub (Lit a) (Lit b) -> Lit (a - b)
  Mul (Lit a) (Lit b) -> Lit (a * b)
  Div (Lit a) (Lit b) | not (esCasiCero b) -> Lit (a / b)
  Pow (Lit a) (Lit b)
    | powPlegable a b -> Lit (a ** b)
  
  Add e1 e2 -> 
    let e1' = optimize e1
        e2' = optimize e2
    in case (e1', e2') of
      (Lit 0, _) -> e2'
      (_, Lit 0) -> e1'
      (Lit a, Lit b) -> Lit (a + b)
      _ -> Add e1' e2'
                  
  Sub e1 e2 -> 
    let e1' = optimize e1
        e2' = optimize e2
    in case (e1', e2') of
      (_, Lit 0) -> e1'
      (Lit a, Lit b) -> Lit (a - b)
      _ -> Sub e1' e2'
                  
  Mul e1 e2 -> 
    let e1' = optimize e1
        e2' = optimize e2
    in case (e1', e2') of
      (Lit 1, _) -> e2'
      (_, Lit 1) -> e1'
      (Lit a, Lit b) -> Lit (a * b)
      _ -> Mul e1' e2'
                  
  Div e1 e2 -> 
    let e1' = optimize e1
        e2' = optimize e2
    in case (e1', e2') of
      (_, Lit 1) -> e1'
      (Lit a, Lit b) | not (esCasiCero b) -> Lit (a / b)
      _ -> Div e1' e2'
                  
  Pow e1 e2 -> 
    let e1' = optimize e1
        e2' = optimize e2
    in case (e1', e2') of
      (Lit a, Lit 0) | a /= 0 -> Lit 1
      (_, Lit 1) -> e1'
      (Lit a, Lit b) | powPlegable a b -> Lit (a ** b)
      _ -> Pow e1' e2'
  
  -- Funciones unarias (aridad 1)
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

powDefinida :: Double -> Double -> Bool
powDefinida base exponente
  | base == 0 && exponente <= 0 = False
  | base < 0 && not (esEntero exponente) = False
  | otherwise = True

powPlegable :: Double -> Double -> Bool
powPlegable base exponente =
  powDefinida base exponente && esFinito (base ** exponente)

esEntero :: Double -> Bool
esEntero x =
  let redondeado = fromIntegral (round x :: Integer)
  in if redondeado == 0
     then x == 0
     else abs (x - redondeado) < 1e-10

esCasiCero :: Double -> Bool
esCasiCero x = abs x < 1e-15

esFinito :: Double -> Bool
esFinito x = not (isNaN x || isInfinite x)
