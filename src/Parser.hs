module Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Expr

-- Definición del analizador léxico
algebraDef :: Token.LanguageDef st
algebraDef = emptyDef
  { Token.commentLine     = "--"
  , Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.identStart      = letter
  , Token.identLetter     = letter
  , Token.opStart         = oneOf "+-*/^"
  , Token.opLetter        = oneOf "+-*/^"
  , Token.reservedNames   = [ "sin", "cos", "tan"
                            , "sinh", "cosh", "tanh"
                            , "arsinh", "arcosh", "artanh"
                            , "sqrt", "exp", "log"
                            , "pi", "e"
                            ]
  , Token.reservedOpNames = ["+", "-", "*", "/", "^"]
  , Token.caseSensitive   = True
  }

-- TokenParser con funciones auxiliares
lexer :: Token.TokenParser st
lexer = Token.makeTokenParser algebraDef

-- Funciones auxiliares del TokenParser
lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

natural :: Parser Integer
natural = Token.natural lexer

float :: Parser Double
float = Token.float lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Parser de literales numéricos (enteros o flotantes)
parseLit :: Parser Expr
parseLit = lexeme $ do
  sign <- optionMaybe (char '-')
  num <- try float <|> (fromInteger <$> natural)
  let value = case sign of
                Just _  -> -num
                Nothing -> num
  return $ Lit value

-- Parser de constantes matemáticas
parseConstant :: Parser Expr
parseConstant = 
  (reserved "pi" >> return (Lit pi)) <|>
  (reserved "e" >> return (Lit (exp 1)))

-- Parser de variables
parseVar :: Parser Expr
parseVar = Var <$> identifier

-- Parsea funciones unarias (ahora con reserved)
parseUnary :: Parser Expr
parseUnary = do
  func <- choice
    [ try (reserved "arsinh") >> return Arsinh
    , try (reserved "arcosh") >> return Arcosh
    , try (reserved "artanh") >> return Artanh
    , try (reserved "sinh")  >> return Sinh
    , try (reserved "cosh")  >> return Cosh
    , try (reserved "tanh")  >> return Tanh
    , try (reserved "sqrt")  >> return Sqrt
    , try (reserved "sin")   >> return Sin
    , try (reserved "cos")   >> return Cos
    , try (reserved "tan")   >> return Tan
    , try (reserved "exp")   >> return Exp
    , try (reserved "log")   >> return Log
    ]
  arg <- parens parseExpr
  return $ func arg

-- Convierte -expr en (0 - expr), evitando conflicto con literales negativos
parseNeg :: Parser Expr
parseNeg = lexeme $ do
  _ <- char '-'
  notFollowedBy digit
  expr <- parseTerm
  return $ Sub (Lit 0) expr

-- Términos (elementos atómicos)
parseTerm :: Parser Expr
parseTerm = try parseUnary 
        <|> try parseConstant 
        <|> try parseLit 
        <|> try parseNeg 
        <|> parseVar 
        <|> parens parseExpr

-- Potencias (asociatividad derecha)
parsePow :: Parser Expr
parsePow = do
  base <- parseTerm
  option base $ do
    reservedOp "^"
    expnt <- parsePow
    return $ Pow base expnt

-- Multiplicación y división
parseMulDiv :: Parser Expr
parseMulDiv = chainl1 parsePow (mulOp <|> divOp)
  where
    mulOp = reservedOp "*" >> return Mul
    divOp = reservedOp "/" >> return Div

-- Suma y resta
parseAddSub :: Parser Expr
parseAddSub = chainl1 parseMulDiv (addOp <|> subOp)
  where
    addOp = reservedOp "+" >> return Add
    subOp = reservedOp "-" >> return Sub

-- Parser principal
parseExpr :: Parser Expr
parseExpr = whiteSpace >> parseAddSub