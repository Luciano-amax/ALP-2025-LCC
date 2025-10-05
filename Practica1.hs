--Ejercicio 1:
module Practica1 where

 newtype Parser a              =  P (String -> [(a,String)])
--import Parsing
-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 1:
-- ¿Qu´e hace sepBy? ¿Qu´e hace symbol?
sepBy  :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do { x <- p
                  ; xs <- many (sep >> p)
                  ; return (x:xs) }

--sepBy p sep construye un parser que lee cero o más ocurrencias de p,
--separadas por sep.
--
--sepBy1 p sep es la versión que exige una o más ocurrencias 
--(al menos una).


-----------------------------------------------------------------------
symbol :: String -> Parser String
symbol xs = token (string xs)

--string xs parsea exactamente la cadena xs.
--token hace que se ignoren espacios antes y después de xs.
--Por lo tanto, symbol xs reconoce un símbolo literal,
--ignorando espacios de alrededor.

-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 2:
--Extender el parser de expresiones visto en clase para permitir
--el uso de la resta y la divisi´on
expr :: Parser Int
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e))
          <|>
          (do symbol "-"
              e <- expr
              return (t - e))
          <|> return t

term :: Parser Int
term = do f <- factor
          (do symbol "*"
              t <- term
              return (f * t))
          <|>
          (do symbol "/"
              t <- term
              return (f `div` t))   -- usamos div porque trabajamos con Int
          <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
        <|> natural

-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 3:
--Escribir un transformador que al recibir un parser, devuelva un nuevo 
--  parser que se comporta como el original pero que tambi´en acepta
--  opcionalmente que las cadenas est´en entre par´entesis.

parensOpt :: Parser a -> Parser a
parensOpt p = (do symbol "("
                  x <- p
                  symbol ")"
                  return x)
              <|> p

-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 4:
--Modificar el parser del ejercicio 2 para que en lugar de evaluar 
--  una expresi´on genere un ´arbol de sintaxis abstracta dado por el tipo

data Op = Add | Mul | Min | Div             deriving Show
data Expr = Num Int | BinOp Op Expr Expr    deriving Show

expr :: Parser Expr
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (BinOp Add t e))
          <|>
          (do symbol "-"
              e <- expr
              return (BinOp Min t e))
          <|> return t

term :: Parser Expr
term = do f <- factor
          (do symbol "*"
              t <- term
              return (BinOP Mul f t))
          <|>
          (do symbol "/"
              t <- term
              return (BinOp Div f t))   -- usamos div porque trabajamos con Int
          <|> return f

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
        <|> (do 
            n <- natural
            return (Num n))

-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 5:
data Basetype = DInt | DChar | DFloat   deriving Show
type Hashtype = [Basetype]

--Int → Char → Float representarlo como [DInt, DChar, DFloat]
baseRead :: Parser Basetype
baseRead = (do symbol "Int"; return DInt)
       <|> (do symbol "Char"; return DChar)
       <|> (do symbol "Float"; return DFloat)

hashRead :: Parser Hasktype
hashRead = sepBy1 baseRead (symbol "->")

-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 6:
--Escribir un parser para listas heterog´eneas de enteros y caracteres 
--por extensi´on usando el formato de Haskell.
--Defina un tipo de datos adecuado para representar estas listas parseadas.
-- Por ejemplo, una cadena a parsear es la
--siguiente: [1,’a’,’b’,2,3,’c’].

data HeteroValue = Heteroint Int | Heterochar Char              deriving Show
data HeteroList = HeteroVoid | Hcons HeteroValue HeteroList     deriving Show

hvalueExpr :: Parse HeteroValue
hvalueExpr = (do
            n <- integer
            return (Heteroint n))
            <|> (do symbol "'"
                c <- item
                symbol "'"
                return (Heterochar c))

getHeteroList :: Parser [HeteroValue]
getHeteroList = do symbol "["
                xs <- sepBy hvalueExpr (symol ",")
                symbol "]"
                return xs

-----------------------------------------------------------------------
-----------------------------------------------------------------------

--EJERCICIO 7:

data Hasktype = DInt | DChar | DFloat | Fun Hasktype Hasktype
  deriving Show

--  type   → atomType "->" type | atomType
--  atomType → "Int" | "Char" | "Float" | "(" type ")"

atomType :: Parser Hasktype
atomType = (do symbol "Int"; return DInt)
       <|> (do symbol "Char"; return DChar)
       <|> (do symbol "Float"; return DFloat)
       <|> (do symbol "("
               t <- hasktype
               symbol ")"
               return t)

hasktype :: Parser Hasktype
hasktype = do t1 <- atomType
              (do symbol "->"
                  t2 <- hasktype
                  return (Fun t1 t2))
              <|> return t1

-----------------------------------------------------------------------
-----------------------------------------------------------------------

-- EJERCICIO 8:

--expr → term expr'
--expr' → (+ term expr') | (- term expr') | ε

--term → factor term'
--term' → (* factor term') | (/ factor term') | ε

--factor → digit | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          restExpr t

restExpr :: Expr -> Parser Expr
restExpr acc = (do symbol "+"
                   t <- term
                   restExpr (BinOp Add acc t))
           <|> (do symbol "-"
                   t <- term
                   restExpr (BinOp Min acc t))
           <|> return acc

term :: Parser Expr
term = do f <- factor
          restTerm f

restTerm :: Expr -> Parser Expr
restTerm acc = (do symbol "*"
                   f <- factor
                   restTerm (BinOp Mul acc f))
           <|> (do symbol "/"
                   f <- factor
                   restTerm (BinOp Div acc f))
           <|> return acc

factor :: Parser Expr
factor = (do n <- natural
             return (Num n))
     <|> (do symbol "("
             e <- expr
             symbol ")"
             return e)
