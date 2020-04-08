module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..), symbol, satisfy, strEq, success)
import Expr (parseExpr, parseBracketsExpr, parseIdent)
import Data.Char (isSpace)
import Control.Applicative
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

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

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"