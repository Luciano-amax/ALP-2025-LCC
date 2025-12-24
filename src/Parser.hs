module Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String
import Expr

spaces' :: Parser ()
spaces' = skipMany space

parseLit :: Parser Expr
parseLit = do
  sign <- option "" (string "-")
  whole <- many1 digit
  decimal <- option "" $ do
    _ <- char '.'
    digits <- many1 digit
    return ('.' : digits)
  spaces'
  let numStr = sign ++ whole ++ decimal
  return $ Lit (read numStr)

parseVar :: Parser Expr
parseVar = do
  v <- many1 letter
  spaces'
  return $ Var v

parseParens :: Parser Expr
parseParens = do
  _ <- char '('
  spaces'
  expr <- parseExpr
  _ <- char ')'
  spaces'
  return expr

-- Parsea funciones unarias. Orden importante: funciones más largas primero
parseUnary :: Parser Expr
parseUnary = do
  func <- choice
    [ try (string "arsinh") >> return Arsinh
    , try (string "arcosh") >> return Arcosh
    , try (string "artanh") >> return Artanh
    , try (string "sinh")  >> return Sinh
    , try (string "cosh")  >> return Cosh
    , try (string "tanh")  >> return Tanh
    , try (string "sqrt")  >> return Sqrt
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

-- Convierte -expr en (0 - expr), evitando conflicto con literales negativos
parseNeg :: Parser Expr
parseNeg = do
  _ <- char '-'
  spaces'
  notFollowedBy digit
  expr <- parseTerm
  return $ Sub (Lit 0) expr


parseTerm :: Parser Expr
parseTerm = try parseUnary <|> try parseLit <|> try parseNeg <|> parseVar <|> parseParens

parsePow :: Parser Expr
parsePow = do
  base <- parseTerm
  spaces'
  option base $ do
    _ <- char '^'
    spaces'
    expnt <- parsePow
    return $ Pow base expnt

parseMulDiv, parseAddSub :: Parser Expr
parseMulDiv = chainl1 parsePow (mulOp <|> divOp)
  where
    mulOp = char '*' >> spaces' >> return Mul
    divOp = char '/' >> spaces' >> return Div

parseAddSub = chainl1 parseMulDiv (addOp <|> subOp)
  where
    addOp = char '+' >> spaces' >> return Add
    subOp = char '-' >> spaces' >> return Sub


parseExpr :: Parser Expr
parseExpr = spaces' >> parseAddSub