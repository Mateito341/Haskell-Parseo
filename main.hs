import Parsing
import Control.Monad
import Control.Applicative hiding (many)
import Data.Char

-- Ejercicio 1
transformer :: Parser a -> Parser a 
transformer p = do 
        char '('
        x <- p
        char ')'
        return x
    <|>
        p


-- Ejemplo:
-- *Main> parse (transformer item) "(1)"
-- [('1',"")]

-- FUNCIONA
-- *Main> parse (transformer item) "(1)2"
-- [('12',"")]

-- Ejercicio 2
withQuotes :: Parser a -> Parser a 
withQuotes p = 
    do 
        char '\"'
        x <- p
        char '\"'
        return x
    <|>
    do 
        char '\''
        x <- p
        char '\''
        return x
    <|>
        p

        
-- Ejemplo:
-- *Main> parse (withQuotes digit) "\"4\""
--[('4',"")]

-- Ejercicio 3

seplist :: Parser a -> [Parser sep] -> Parser [a]
seplist p seps = sepBy p (foldr (<|>) empty (map void seps))

-- Ejemplo
-- *Main> parse (seplist int [char ',', char ';']) "1;2;3"
--[([1,2,3],"")]

-- Ejercicio 4
data Tuple = Single Int | Pair Tuple Tuple
  deriving Show

tuplas :: Parser Tuple
tuplas =
      do char '('
         x <- tuplas
         char ','
         y <- tuplas
         char ')'
         return (Pair x y)
  <|> do n <- int
         return (Single n)

-- Ejemplo:
-- *Main> parse tuplas "(1,2)"
--[(Pair (Single 1) (Single 2),"")]

-- Ejercicio 5

data Interval = Interval
  { leftOpen  :: Bool
  , rightOpen :: Bool
  , a         :: Int
  , b         :: Int
  } deriving Show

-- Asumimos que ya tenés:
-- char :: Char -> Parser Char
-- float :: Parser Float
-- (o int :: Parser Int y luego convertís)

intervalos :: Parser Interval
intervalos = do
  open  <- char '(' <|> char '['
  x     <- int
  char ','
  y     <- int
  close <- char ')' <|> char ']'
  return (Interval (open == '(') (close == ')') x y)

--Ejemplo
-- *Main> parse intervalos "[1,4)"
-- [(Interval {leftOpen = False, rightOpen = True, a = 1, b = 4},"")]

-- Ejercicio 6

-- Parser de flotantes (ejemplo simple)
float :: Parser Float
float = do
    s <- some (sat isDigit)
    return (read s)

-- Función que genera la lista linealmente espaciada
linspace' :: Float -> Float -> Int -> [Float]
linspace' inicio fin n =
    let step = (fin - inicio) / fromIntegral (n - 1)
    in [inicio + step * fromIntegral i | i <- [0..n-1]]

-- Parser principal
linspace :: Parser [Float]
linspace = do
    char '('
    inicio <- float
    char ','
    fin    <- float
    char ','
    n      <- int
    char ')'
    return (linspace' inicio fin n)

-- Ejemplo
-- *Main> parse linspace "(1,3,6)"
-- [([1.0,1.4,1.8,2.2,2.6,3.0],"")]

-- Ejercicio 7
data HValue = HInt Int | HChar Char
  deriving (Show, Eq)

caracter :: Parser Char
caracter = do
  char '\''
  c <- item
  char '\''
  return c

valor :: Parser HValue
valor = do
  n <- int
  return (HInt n)
  <|> do
    c <- caracter
    return (HChar c)

heterogeneas :: Parser [HValue]
heterogeneas = do
  char '['
  vals <- sepBy valor (char ',')
  char ']'
  return vals

-- Ejemplo
-- *Main> parse heterogeneas "[3,'z','r',7,9,22,'1']"
-- [([HInt 3,HChar 'z',HChar 'r',HInt 7,HInt 9,HInt 22,HChar '1'],"")]

-- Ejercicio 8

{-
expr -> term expr'
expr' -> ('+' term | '-' term) expr' | e

term -> factor term'
term' -> ('*' factor | '/' factor) term' | e

factor -> digit | '(' expr ')'

digit -> '0' | '1' | ... | '9'
-}


-- Tipos de datos para el árbol de sintaxis abstracta
data Expr = Num Int | BinOp Op Expr Expr
    deriving (Show, Eq)

data Op = Add | Mul | Res | Div
    deriving (Show, Eq)

-- expr -> term expr'
expr :: Parser Expr
expr = do
    t <- term
    expr' t

-- expr' -> ('+' term | '-' term) expr' | e
expr' :: Expr -> Parser Expr
expr' left = do
    (do
        char '+'
        t <- term
        expr' (BinOp Add left t)
     <|> do
        char '-'
        t <- term
        expr' (BinOp Res left t)
     <|> return left)

-- term -> factor term'
term :: Parser Expr
term = do
    f <- factor
    term' f

-- term' -> ('*' factor | '/' factor) term' | e
term' :: Expr -> Parser Expr
term' left = do
    (do
        char '*'
        f <- factor
        term' (BinOp Mul left f)
     <|> do
        char '/'
        f <- factor
        term' (BinOp Div left f)
     <|> return left)

-- factor -> digit | '(' expr ')'
factor :: Parser Expr
factor = do
    (do
        d <- digit
        return (Num (read [d]))
     <|> do
        char '('
        e <- expr
        char ')'
        return e)

-- Ejemplos:
-- expr "2+3*4"     -- BinOp Add (Num 2) (BinOp Mul (Num 3) (Num 4))
-- expr "(2+3)*4"   -- BinOp Mul (BinOp Add (Num 2) (Num 3)) (Num 4)
-- expr "10-5/2"    -- BinOp Res (Num 10) (BinOp Div (Num 5) (Num 2))

-- Ejercicio 10

lista :: Parser [Int]
lista = sepBy int space

matriz :: Parser [[Int]]
matriz = do
    char '['
    filas <- sepBy lista (char ';')
    char ']'
    return filas

-- Ejemplo
-- ghci> parse matriz "[1 2 3;4 5 6;7 8 9]"
-- [([[1,2,3],[4,5,6],[7,8,9]],"")]

-- Ejercicio 11

{-
OrExpr  -> OrExpr 'or' AndExpr | AndExpr
AndExpr -> AndExpr 'and' Factor  | Factor
Factor  -> 'not' Factor | '(' OrExpr ')' | Bool
Bool    -> 'True' | 'False'

-------------------
Elimino recurion izquierda

Eval -> Orexpr

OrExpr -> AndExpr OrExpr'
OrExpr' -> 'or' AndExpr OrExpr' | e

AndExpr -> Factor AndExpr'
AndExpr -> 'and' Factor AndExpr' | e

Factor -> 'not' Factor | '('OrExpr')' | Bool

Bool -> 'True' | 'False'
-}

data BoolExpr = BVal Bool
                | Not BoolExpr
                | And BoolExpr BoolExpr
                | Or BoolExpr BoolExpr
                deriving Show

orExpr :: Parser BoolExpr
orExpr = do t <- andExpr
            f <- orExpr'
            return (f t)

orExpr' :: Parser (BoolExpr -> BoolExpr)
orExpr' = do token (string "or")
             t <- andExpr
             f' <- orExpr'
             return (f'.(\x ->Or x t))
          <|> return id

andExpr :: Parser BoolExpr
andExpr = do t <- factorr
             f <- andExpr'
             return (f t)

andExpr' :: Parser (BoolExpr -> BoolExpr)
andExpr' = do token (string "and")
              t <- factorr
              f' <- andExpr'
              return (f'.(\x ->And x t))
           <|> return id

factorr :: Parser BoolExpr
factorr =
  (do
    token (string "not")
    t <- factorr
    return (Not t))
  <|> (do
    char '('
    t <- orExpr
    char ')'
    return t)
  <|> bool


bool :: Parser BoolExpr
bool =
  (do
    token (string "True")
    return (BVal True))
  <|> (do
    token (string "False")
    return (BVal False))


eval :: String -> BoolExpr
eval xs = fst (head (parse orExpr xs))

-- Ejemplo:
-- *Main> eval "not (True or False and True)"
-- Not (Or (BVal True) (And (BVal False) (BVal True)))
