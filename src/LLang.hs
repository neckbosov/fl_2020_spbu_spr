module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..), symbol, satisfy, strEq, success)
import Expr (parseExpr, parseBracketsExpr, parseIdent)
import Data.Char (isSpace)
import Control.Applicative

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseL :: Parser String String LAst
parseL = parseSeq

parseSeq :: Parser String String LAst
parseSeq = Seq <$> many (parseInstruction <* parseWss)

parseSpace :: Parser String String Char
parseSpace = symbol ' '

parseWs :: Parser String String Char
parseWs = satisfy isSpace

parseSpaces :: Parser String String String
parseSpaces = many parseSpace

parseWss :: Parser String String String
parseWss = many parseWs

parseBlock :: Parser String String LAst
parseBlock = symbol '{' *> parseWss *> parseSeq <* parseWss <* symbol '}'

parseIf :: Parser String String LAst
parseIf = strEq "if" *> parseSpaces *> fmap If parseBracketsExpr <* parseWss <*>
    parseBlock <*> ((parseWss *> strEq "else" *> parseWss *> parseBlock) <|> success Seq {statements = []})

parseWhile :: Parser String String LAst
parseWhile = strEq "while" *> parseSpaces *> fmap While parseBracketsExpr <* parseWss <*> parseBlock

parseAssign :: Parser String String LAst
parseAssign = Assign <$> parseIdent <* parseSpaces <* symbol '=' <* parseSpaces <*> parseExpr <* symbol ';'

parseRead :: Parser String String LAst
parseRead = strEq "read(" *> fmap Read parseIdent <* symbol ')' <* symbol ';'

parseWrite :: Parser String String LAst
parseWrite = strEq "write" *> fmap Write parseBracketsExpr <* symbol ';'

parseInstruction :: Parser String String LAst
parseInstruction = parseAssign <|> parseIf <|> parseWhile <|> parseRead <|> parseWrite