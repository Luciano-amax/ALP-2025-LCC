module Parser (parseExpr) where

import Text.Parsec             -- Biblioteca de parsing Parsec
import Text.Parsec.String      -- Tipo Parser para cadenas
import Expr                    -- Importamos el AST

-- Espaciado (opcional, puede omitir espacios iniciales o entre tokens)
spaces' :: Parser ()
spaces' = skipMany space

-- Parser para literales (números)
parseLit :: Parser Expr
parseLit = do
  n <- many1 (digit <|> char '.') -- Captura números decimales como "3.14"
  return $ Lit (read n)           -- Convierte a Double y lo guarda como `Lit`

-- Parser para variables (como "x")
parseVar :: Parser Expr
parseVar = do
  v <- many1 letter
  spaces'
  return $ Var v  -- Tratamos todas las variables como Vars para diferenciarlas en el evaluador

-- Parser para términos entre paréntesis
parseParens :: Parser Expr
parseParens = do
  _ <- char '('
  spaces'
  expr <- parseExpr                  -- Parseamos una expresión dentro
  _ <- char ')'
  spaces'
  return expr

-- Parser para funciones unarias como sin(x), cos(x) y las nuevas funciones
parseUnary :: Parser Expr
parseUnary = do
  func <- choice
    [ string "sin"   >> return Sin
    , string "cos"   >> return Cos
    , string "tan"   >> return Tan
    , string "exp"   >> return Exp
    , string "log"   >> return Log
    , string "sinh"  >> return Sinh
    , string "cosh"  >> return Cosh
    , string "tanh"  >> return Tanh
    , string "arsinh" >> return Arsinh
    , string "arcosh" >> return Arcosh
    , string "artanh" >> return Artanh
    ]
  spaces'
  arg <- parseTerm
  return $ func arg

-- Parser de términos básicos (literales, variables, funciones, paréntesis)
parseTerm :: Parser Expr
parseTerm = parseLit <|> parseVar <|> parseUnary <|> parseParens

-- Parser para el operador de potencia "^" (con alta precedencia, derecha)
parsePow :: Parser Expr
parsePow = do
  base <- parseTerm
  spaces'
  option base $ do
    _ <- char '^'
    spaces'
    expnt <- parsePow              -- Recursivo para asociatividad derecha
    return $ Pow base expnt

-- Parser para operadores entre términos
parseMulDiv, parseAddSub :: Parser Expr
parseMulDiv = chainl1 parsePow (mulOp <|> divOp)
  where
    mulOp = char '*' >> spaces' >> return Mul
    divOp = char '/' >> spaces' >> return Div

parseAddSub = chainl1 parseMulDiv (addOp <|> subOp)
  where
    addOp = char '+' >> spaces' >> return Add
    subOp = char '-' >> spaces' >> return Sub

-- Parser para expresiones completas
parseExpr :: Parser Expr
parseExpr = spaces' >> parseAddSub