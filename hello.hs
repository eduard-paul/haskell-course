import Data.Char
 
{- Выражение не очень удобно вычислять сразу при вводе, поэтому разумно 
сохранить его в новый тип данных, которое представить в виде синтаксического дерева.
Тип выражения параметризован типом чисел, которые в нём могут храниться.-}
data Expr a
    = Const a --константы
    | Binary (Expr a) Operator (Expr a) --операторы
    | Unary Operator (Expr a) --унарные операторы и функции
    | VarNode String
    
instance Num q => Num (Expr q) where
    a + b = Binary a Plus b
    a * b = Binary a Mul b
    a - b = Binary a Sub b
    --a / b = Binary a Div b
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
    show Sin = "sin"
    show Cos = "cos"

instance Show a => Show (Expr a) where
    show (Const c) = show c
    show (Binary a op b) = "(" ++ (show a) ++ " " ++ (show op) ++ " " ++ (show b) ++ ")"
    show (Unary op a) = (show op) ++ "(" ++ (show a) ++ ")"
    show (VarNode s) = show s

{- Выражение можно парсить. Проще всего сделать это методом рекурсивного спуска.
(https://ru.wikipedia.org/wiki/Метод_рекурсивного_спуска)
-}
parseExpr :: String -> Expr a
parseExpr = undefined
{- Разбор выражения можно сделать и любым другим методом, например, перегонкой через
обратную польскую запись. -}

{-Бонус 2: выражение также можно выводить.
instance Show a => Show (Expr a) where
    show (Const c) = show c
    show <...>
    
Но для этого придётся хранить функции в символьном виде:
data Operator = Plus | <...>
-}
    
{- Выражение нужно уметь вычислять. -}
calcExpr :: Num a => Expr a -> a
calcExpr (Const a) = a --константа вычисляется в её значение
calcExpr (Binary a op b)
    | op == Plus = (calcExpr a) + (calcExpr b)
    | op == Mul = (calcExpr a) * (calcExpr b)
    | op == Sub = (calcExpr a) - (calcExpr b)

calcExprFract :: Fractional a => Expr a -> a
calcExprFract (Const a) = a --константа вычисляется в её значение
calcExprFract (Binary a op b)
    | op == Plus = (calcExprFract a) + (calcExprFract b)
    | op == Mul = (calcExprFract a) * (calcExprFract b)
    | op == Sub = (calcExprFract a) - (calcExprFract b)
    | op == Div = (calcExprFract a) / (calcExprFract b)

calcExprFloat :: Floating a => Expr a -> a
calcExprFloat (Const a) = a --константа вычисляется в её значение
calcExprFloat (Binary a op b)
    | op == Plus = (calcExprFloat a) + (calcExprFloat b)
    | op == Mul = (calcExprFloat a) * (calcExprFloat b)
    | op == Sub = (calcExprFloat a) - (calcExprFloat b)
    | op == Div = (calcExprFloat a) / (calcExprFloat b)
calcExprFloat (Unary op a)
    | op == Sin = sin (calcExprFloat a)
    | op == Cos = cos (calcExprFloat a)
    | op == Sub = -(calcExprFloat a)
    | op == Plus = calcExprFloat a

{- Бонус 1: вычислять выражение с переменными.
Для этого нужно добавить возможность хранить переменные в выражении.
Переменная однозначно определяется именем:
type Var = String
Для вычисления нужно передавать значения его переменных:
calcExpr :: Num a => [(Var, a)] -> Expr a -> a
Или же выводить выражение недовычисленным (бонус 1c):
calcExpr :: Num a => [(Var, a)] -> Expr a -> Expr a
-}

{- 
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
                ("+", Plus),
                ("-", Sub),
                ("/", Div),
                ("*", Mul)
            ]
            
            
--calculate :: String -> Double
--calculate = eval operatorRegister . words
            
--eval :: Register -> [String] -> Double
--eval _ [number] = read number
--eval ((operator, function):rest) unparsed =
--    case span (/=operator) unparsed of
--        (_, []) -> eval rest unparsed
--        (beforeOperator, afterOperator) -> 
--            function
--                (eval operatorRegister beforeOperator)
--                (eval operatorRegister $ drop 1 afterOperator)

toExpr :: (Floating a, Read a) => String -> Expr a
toExpr =  www operatorRegister . words

qqq :: (Floating a, Read a) => Register -> [String] -> Expr a
qqq _ [number] = Const (read number)
qqq ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> qqq rest unparsed
        (beforeOperator, afterOperator) -> 
                Binary 
                    (qqq operatorRegister beforeOperator)
                    function
                    (qqq operatorRegister $ drop 1 afterOperator)

join:: [String] -> String
join [] = []
join list = (head list) ++ (join (tail list))

--www :: (Floating a, Read a) => Register -> [String] -> Expr a
--www _ [number] = Const (read number)
--www reg unparsed =
--    case span (/=")") unparsed of
--        (_, []) -> qqq reg unparsed
--        (beforeOperator, afterOperator) -> 
--            www reg $ ((reverse $ drop 1 left)++(words $ show $ qqq reg $ reverse middle))++(drop 1 afterOperator) where
--                (middle,left) = span (/="(") (reverse beforeOperator)
 
www :: (Floating a, Read a) => Register -> [String] -> Expr a
www _ [number] = Const (read number)
www reg unparsed =
    case span (/=")") unparsed of
        (_, []) -> qqq reg unparsed
        (beforeOperator, afterOperator) -> 
            www reg $ ((reverse $ drop 1 left)++([show $ calcExprFloat $ qqq reg $ reverse middle]))++(drop 1 afterOperator) where
                (middle,left) = span (/="(") (reverse beforeOperator)

-}

{-////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////-}   


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


operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Sub
           | c == '*' = Mul
           | c == '/' = Div
 
tokenize :: (Floating a, Read a) => String -> [Token a]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

identifier :: (Floating a, Read a) => Char -> String -> [Token a]
identifier c cs = let (name, cs') = span isAlphaNum cs in
                  TokIdent (c:name) : tokenize cs'

number :: (Floating a, Read a) => Char -> String -> [Token a]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'


--data Expr = Binary Operator Expr Expr
--          | Unary Operator Expr
--          | Const Double
--          | VarNode String
--    deriving Show

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
      (TokOp op) | elem op [Plus, Sub] -> 
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
