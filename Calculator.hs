{- Бонус 1: вычислять выражение с переменными.
Для этого нужно добавить возможность хранить переменные в выражении.
Переменная однозначно определяется именем:
type Var = String
Для вычисления нужно передавать значения его переменных:
calcExpr :: Num a => [(Var, a)] -> Expr a -> a
Или же выводить выражение недовычисленным (бонус 1c):
calcExpr :: Num a => [(Var, a)] -> Expr a -> Expr a
-}
{-Бонус 2: выражение также можно выводить.
instance Show a => Show (Expr a) where
    show (Const c) = show c
    show <...>
-}
module Calculator
    ( Expr
    , parseExpr
    , calculate
    )
    where

import Data.Char

data Expr a
    = Const a --константы
    | Binary (Expr a) Operator (Expr a) --операторы
    | Unary Operator (Expr a) --унарные операторы и функции
    | VarNode String
    
instance Num q => Num (Expr q) where
    a + b = Binary a Plus b
    a * b = Binary a Mul b
    a - b = Binary a Sub b
    fromInteger a = Const (fromInteger a)

data Operator = Plus | Mul | Sub | Div | Sin | Cos deriving (Eq)

instance Fractional a => Fractional (Expr a) where
    a / b = Binary a Div b 
    fromRational a = Const (fromRational a)

instance Floating a => Floating (Expr a) where
    sin e = Unary Sin e 
    cos e = Unary Cos e

instance Show Operator where
    show Plus = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Sin = "sin "
    show Cos = "cos "

instance Show a => Show (Expr a) where
    show (Const c) = show c
    show (Binary a op b) = "(" ++ (show a) ++ " " ++ (show op) ++ " " ++ (show b) ++ ")"
    show (Unary op a) = (show op) ++ (show a)
    show (VarNode s) = ""++s

doBinOp :: Floating a => Expr a -> Operator -> Expr a -> Expr a
doBinOp (Const a) op (Const b)
    | op == Plus = Const (a + b)
    | op == Mul =  Const (a * b)
    | op == Sub =  Const (a - b)
    | op == Div =  Const (a / b)
doBinOp a op b = Binary a op b

doUnOp :: Floating a => Operator -> Expr a -> Expr a
doUnOp op (Const a)
    | op == Sin = Const (sin a)
    | op == Cos =  Const (cos a)
    | op == Sub =  Const (-a)
doUnOp op a = Unary op a

calcExpr :: Floating a => Expr a -> Expr a
calcExpr (Const a) = Const a --константа вычисляется в её значение
calcExpr (VarNode a) = VarNode a
calcExpr (Binary a op b) 
    | op == Plus = doBinOp (calcExpr a) Plus (calcExpr b)
    | op == Mul = doBinOp (calcExpr a) Mul (calcExpr b)
    | op == Sub = doBinOp (calcExpr a) Sub (calcExpr b)
    | op == Div = doBinOp (calcExpr a) Div (calcExpr b)
calcExpr (Unary op a)
    | op == Sin = doUnOp Sin (calcExpr a)
    | op == Cos = doUnOp Cos (calcExpr a)
    | op == Sub = doUnOp Sub (calcExpr a)
    | op == Plus = calcExpr a

replace :: Floating a => (String, a) -> Expr a -> Expr a
replace _ (Const a) = Const a
replace (var, val) (VarNode v)
  | var == v = Const val
  | otherwise = VarNode v
replace x (Binary a op b) = Binary (replace x a) op (replace x b)
replace x (Unary op a) = Unary op (replace x a)

calculate :: Floating a => [(String, a)] -> Expr a -> Expr a
calculate [] a = calcExpr a
calculate (x:xs) a = calculate xs $ replace x a

data Token a 
    = TokOp Operator
    | TokLParen
    | TokRParen
    | TokIdent String
    | TokNum a
    | TokEnd deriving (Show, Eq)

instance Num a => Num (Token a) where
    fromInteger a = TokNum (fromInteger a)

instance Fractional a => Fractional (Token a) where
    fromRational a = TokNum (fromRational a)

operator :: String -> Operator
operator c | c == "+" = Plus
           | c == "-" = Sub
           | c == "*" = Mul
           | c == "/" = Div
           | c == "sin" = Sin
           | c == "cos" = Cos
 
tokenize :: (Floating a, Read a) => String -> [Token a]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokOp (operator $ c:"") : tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]


identifier :: (Floating a, Read a) => Char -> String -> [Token a]
identifier c cs = let (name, cs') = span isAlphaNum cs in
                  if elem (c:name) ["sin","cos"] then TokOp (operator (c:name)) : tokenize cs'
                  else TokIdent (c:name) : tokenize cs'

number :: (Floating a, Read a) => Char -> String -> [Token a]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'

lookAhead :: (Floating a, Read a) => [Token a] -> Token a
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: (Floating a, Read a) => [Token a] -> [Token a]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

expression :: (Floating a, Read a, Eq a, Show a) => [Token a] -> (Expr a, [Token a])
expression toks = 
   let (termExpr, toks') = term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Plus, Sub] -> 
            let (exExpr, toks'') = expression (accept toks') 
            in (Binary termExpr op exExpr, toks'')
         _ -> (termExpr, toks')

term :: (Floating a, Read a, Eq a, Show a) => [Token a] -> (Expr a, [Token a])
term toks = 
   let (facExpr, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Mul, Div] ->
            let (termExpr, toks'') = term (accept toks') 
            in (Binary facExpr op termExpr, toks'')
         _ -> (facExpr, toks')

factor :: (Floating a, Read a, Eq a, Show a) => [Token a] -> (Expr a, [Token a])
factor toks = 
   case lookAhead toks of
      (TokNum x)     -> (Const x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Sub, Sin, Cos] -> 
            let (facExpr, toks') = factor (accept toks) 
            in (Unary op facExpr, toks')
      TokLParen      -> 
         let (expExpr, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expExpr, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks

parse :: (Floating a, Read a, Eq a, Show a) => [Token a] -> Expr a
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

parseExpr :: (Show a, Read a, Eq a, Floating a) => String -> Expr a
parseExpr a = parse $ tokenize a
