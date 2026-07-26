module PrettyPrinter (
    prettyPrint,
    prettyPrintWithParens,
    prettyPrintOptimized
) where

import Expr

-- Orden usado para decidir parentesis sin cambiar la expresion al reparsear.
data Precedence = PrecAtom | PrecPow | PrecMul | PrecAdd | PrecTop
    deriving (Eq, Ord)

-- Imprime una expresion legible, con los parentesis justos.
prettyPrint :: Expr -> String
prettyPrint = prettyPrintPrec PrecTop

-- Version verbosa, util para depurar el arbol sin depender de precedencias.
prettyPrintWithParens :: Expr -> String
prettyPrintWithParens expr = case expr of
    Lit n -> showNumber n
    Var v -> v
    Add e1 e2 -> "(" ++ prettyPrintWithParens e1 ++ " + " ++ prettyPrintWithParens e2 ++ ")"
    Sub e1 e2 -> "(" ++ prettyPrintWithParens e1 ++ " - " ++ prettyPrintWithParens e2 ++ ")"
    Mul e1 e2 -> "(" ++ prettyPrintWithParens e1 ++ " * " ++ prettyPrintWithParens e2 ++ ")"
    Div e1 e2 -> "(" ++ prettyPrintWithParens e1 ++ " / " ++ prettyPrintWithParens e2 ++ ")"
    Pow e1 e2 -> "(" ++ prettyPrintWithParens e1 ++ " ^ " ++ prettyPrintWithParens e2 ++ ")"
    Sin e -> "sin(" ++ prettyPrintWithParens e ++ ")"
    Cos e -> "cos(" ++ prettyPrintWithParens e ++ ")"
    Tan e -> "tan(" ++ prettyPrintWithParens e ++ ")"
    Sinh e -> "sinh(" ++ prettyPrintWithParens e ++ ")"
    Cosh e -> "cosh(" ++ prettyPrintWithParens e ++ ")"
    Tanh e -> "tanh(" ++ prettyPrintWithParens e ++ ")"
    Arsinh e -> "arsinh(" ++ prettyPrintWithParens e ++ ")"
    Arcosh e -> "arcosh(" ++ prettyPrintWithParens e ++ ")"
    Artanh e -> "artanh(" ++ prettyPrintWithParens e ++ ")"
    Log e -> "log(" ++ prettyPrintWithParens e ++ ")"
    Exp e -> "exp(" ++ prettyPrintWithParens e ++ ")"
    Sqrt e -> "sqrt(" ++ prettyPrintWithParens e ++ ")"

-- Muestra el antes y despues cuando la optimizacion realmente cambia algo.
prettyPrintOptimized :: Expr -> String
prettyPrintOptimized expr =
    let original = prettyPrint expr
        optimized = prettyPrint (optimize expr)
    in if original == optimized
       then "  " ++ original
       else "  Original:   " ++ original ++ "\n  Optimizada: " ++ optimized

-- El contexto de precedencia indica si la subexpresion necesita parentesis.
prettyPrintPrec :: Precedence -> Expr -> String
prettyPrintPrec _ (Lit n) = showNumber n
prettyPrintPrec _ (Var v) = v

prettyPrintPrec prec (Add e1 e2) =
    parenthesize (prec < PrecAdd) $
        prettyPrintPrec PrecAdd e1 ++ " + " ++ prettyPrintPrec PrecAdd e2

prettyPrintPrec prec (Sub e1 e2) =
    parenthesize (prec < PrecAdd) $
        prettyPrintPrec PrecAdd e1 ++ " - " ++ prettyPrintPrec PrecMul e2

prettyPrintPrec prec (Mul e1 e2) =
    parenthesize (prec < PrecMul) $
        prettyPrintPrec PrecMul e1 ++ " * " ++ prettyPrintPrec PrecMul e2

prettyPrintPrec prec (Div e1 e2) =
    parenthesize (prec < PrecMul) $
        prettyPrintPrec PrecMul e1 ++ " / " ++ prettyPrintPrec PrecPow e2

prettyPrintPrec prec (Pow e1 e2) =
    parenthesize (prec < PrecPow) $
        prettyPrintPowBase e1 ++ " ^ " ++ prettyPrintPrec PrecAtom e2

prettyPrintPrec _ (Sin e) = "sin(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Cos e) = "cos(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Tan e) = "tan(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Sinh e) = "sinh(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Cosh e) = "cosh(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Tanh e) = "tanh(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Arsinh e) = "arsinh(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Arcosh e) = "arcosh(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Artanh e) = "artanh(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Log e) = "log(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Exp e) = "exp(" ++ prettyPrintPrec PrecTop e ++ ")"
prettyPrintPrec _ (Sqrt e) = "sqrt(" ++ prettyPrintPrec PrecTop e ++ ")"

-- Los enteros se imprimen sin ".0" para que la salida sea mas limpia.
showNumber :: Double -> String
showNumber n
    | n == fromInteger (round n) = show (round n :: Integer)
    | otherwise = show n

parenthesize :: Bool -> String -> String
parenthesize True s = "(" ++ s ++ ")"
parenthesize False s = s

-- En potencias, las bases negativas o ya exponenciadas necesitan parentesis.
prettyPrintPowBase :: Expr -> String
prettyPrintPowBase expr@(Lit n)
    | n < 0 = "(" ++ showNumber n ++ ")"
    | otherwise = prettyPrintPrec PrecPow expr
prettyPrintPowBase expr@(Pow _ _) = "(" ++ prettyPrintPrec PrecTop expr ++ ")"
prettyPrintPowBase expr = prettyPrintPrec PrecPow expr
