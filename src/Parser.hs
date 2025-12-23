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
    [ try (string "arsinh") >> return Arsinh
    , try (string "arcosh") >> return Arcosh
    , try (string "artanh") >> return Artanh
    , try (string "sinh")  >> return Sinh
    , try (string "cosh")  >> return Cosh
    , try (string "tanh")  >> return Tanh
    , try (string "sin")   >> return Sin
    , try (string "cos")   >> return Cos
    , try (string "tan")   >> return Tan
    , try (string "exp")   >> return Exp
    , try (string "log")   >> return Log
    ]
  spaces'
  _ <- char '('
  spaces'
  arg <- parseExpr
  spaces'
  _ <- char ')'
  spaces'
  return $ func arg

-- Parser para negación unaria
parseNeg :: Parser Expr
parseNeg = do
  _ <- char '-'
  spaces'
  expr <- parseTerm
  return $ Sub (Lit 0) expr  -- Convertimos -x en 0 - x

-- Parser de términos básicos (literales, variables, funciones, paréntesis)
parseTerm :: Parser Expr
parseTerm = try parseUnary <|> try parseNeg <|> parseLit <|> parseVar <|> parseParens

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