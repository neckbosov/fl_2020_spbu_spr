module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), Result (..), elem', fail',
                                      satisfy, strEq, success, symbol)
import           Control.Applicative
import           Data.Char           (digitToInt, isAlpha, isDigit)

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = error "evalExpr undefined"

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast

uberExpr ops elem binaryCreator unaryCreator = foldr f elem ops
    where
        f (op, Unary) elem1 = (unaryCreator <$> op <*> elem1) <|> elem1
        f (op, Binary NoAssoc) elem1 = do
            lhs <- elem1
            ((`binaryCreator` lhs) <$> op <*> elem1) <|> return lhs

        f (op, Binary assoc) elem1 = do
            x <- elem1
            foldAst assoc x <$> many ((,) <$> op <*> elem1)

        foldAst LeftAssoc x = foldl (\lhs (op, rhs) -> binaryCreator op lhs rhs) x
        foldAst RightAssoc x = \ls -> snd $ foldr1 (\(o, lhs) (op, rhs) -> (o, binaryCreator op lhs rhs)) ((undefined, x) : ls)

parseCurOp :: String -> Parser String String Operator
parseCurOp op = strEq op >>= toOperator

plus' = parseCurOp "+"
minus' = parseCurOp "-"
mult' = parseCurOp "*"
div' = parseCurOp "/"
pow' = parseCurOp "^"
eq' = parseCurOp "=="
neq' = parseCurOp "/="
gt' = parseCurOp ">"
ge' = parseCurOp ">="
lt' = parseCurOp "<"
le' = parseCurOp "<="
and' = parseCurOp "&&"
or' = parseCurOp "||"
not' = parseCurOp "!"


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
    (or', Binary RightAssoc),
    (and', Binary RightAssoc),
    (not', Unary),
    (eq' <|> neq' <|> ge' <|> gt' <|> le' <|> lt' , Binary NoAssoc),
    (plus' <|> minus', Binary LeftAssoc),
    (mult' <|> div', Binary LeftAssoc),
    (minus', Unary),
    (pow', Binary RightAssoc)
    ] ((Num <$> parsePositiveNum) <|> (Ident <$> parseIdent) <|> parseBracketsExpr) BinOp UnaryOp

parseBracketsExpr :: Parser String String AST
parseBracketsExpr = symbol '(' *> parseExpr <* symbol ')'

-- Парсер для натуральных чисел с 0
parsePositiveNum :: Parser String String Int
parsePositiveNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> some (satisfy isDigit)

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = ((\ls x -> x * (-1) ^ length ls) <$> many (symbol '-')) <*> parsePositiveNum

identSym :: Parser String String Char
identSym = satisfy isAlpha <|> symbol '_' <|> satisfy isDigit

parseIdent :: Parser String String String
parseIdent = (:) <$> (satisfy isAlpha <|> symbol '_') <*> many identSym

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = eq' <|> neq' <|> ge' <|> gt'  <|> le' <|> lt' <|> or' <|> and' <|> plus' <|> minus' <|> mult' <|> div' <|> pow'

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "/"  = success Div
toOperator "^"  = success Pow
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator ">"  = success Gt
toOperator ">=" = success Ge
toOperator "<"  = success Lt
toOperator "<=" = success Le
toOperator "&&" = success And
toOperator "||" = success Or
toOperator "!"  = success Not
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y

