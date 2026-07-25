module Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Expr

algebraDef :: Token.LanguageDef st
algebraDef = emptyDef
  { Token.commentLine = "--"
  , Token.commentStart = "{-"
  , Token.commentEnd = "-}"
  , Token.identStart = letter
  , Token.identLetter = letter
  , Token.opStart = oneOf "+-*/^"
  , Token.opLetter = oneOf "+-*/^"
  , Token.reservedNames =
      [ "sin", "cos", "tan"
      , "sinh", "cosh", "tanh"
      , "arsinh", "arcosh", "artanh"
      , "sqrt", "exp", "log"
      , "pi", "e"
      ]
  , Token.reservedOpNames = ["+", "-", "*", "/", "^"]
  , Token.caseSensitive = True
  }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser algebraDef

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

parseLit :: Parser Expr
parseLit = lexeme $ do
  num <- try float <|> (fromInteger <$> natural)
  pure $ Lit num

parseConstant :: Parser Expr
parseConstant =
  (reserved "pi" >> pure (Lit pi)) <|>
  (reserved "e" >> pure (Lit (exp 1)))

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseUnary :: Parser Expr
parseUnary = do
  func <- choice
    [ try (reserved "arsinh") >> pure Arsinh
    , try (reserved "arcosh") >> pure Arcosh
    , try (reserved "artanh") >> pure Artanh
    , try (reserved "sinh") >> pure Sinh
    , try (reserved "cosh") >> pure Cosh
    , try (reserved "tanh") >> pure Tanh
    , try (reserved "sqrt") >> pure Sqrt
    , try (reserved "sin") >> pure Sin
    , try (reserved "cos") >> pure Cos
    , try (reserved "tan") >> pure Tan
    , try (reserved "exp") >> pure Exp
    , try (reserved "log") >> pure Log
    ]
  func <$> parens parseAddSub

-- El menos unario queda por debajo de la potencia: -x^2 = -(x^2).
parseNeg :: Parser Expr
parseNeg = do
  reservedOp "-"
  expr <- parsePrefix
  pure $ Sub (Lit 0) expr

parseAtom :: Parser Expr
parseAtom = try parseUnary
        <|> try parseConstant
        <|> try parseLit
        <|> parseVar
        <|> parens parseAddSub

parsePrefix :: Parser Expr
parsePrefix = try parseNeg <|> parsePow

parsePow :: Parser Expr
parsePow = do
  base <- parseAtom
  option base $ do
    reservedOp "^"
    expnt <- parsePrefix
    pure $ Pow base expnt

parseMulDiv :: Parser Expr
parseMulDiv = chainl1 parsePrefix (mulOp <|> divOp)
  where
    mulOp = reservedOp "*" >> pure Mul
    divOp = reservedOp "/" >> pure Div

parseAddSub :: Parser Expr
parseAddSub = chainl1 parseMulDiv (addOp <|> subOp)
  where
    addOp = reservedOp "+" >> pure Add
    subOp = reservedOp "-" >> pure Sub

parseExpr :: Parser Expr
parseExpr = whiteSpace >> parseAddSub <* eof
