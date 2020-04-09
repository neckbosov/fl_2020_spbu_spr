module LLang where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), satisfy, strEq, success,
                                      symbol)
import           Control.Applicative
import Control.Monad
import           Data.Char           (isSpace)
import qualified Data.Map            as Map
import           Expr                (parseBracketsExpr, parseExpr, parseIdent)

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

fromBool :: Bool -> Int
fromBool True  = 1
fromBool False = 0

toBool :: Int -> Bool
toBool n | n /= 0 = True
         | otherwise = False

doBinOp :: Operator -> Int -> Int -> Int
doBinOp Plus   = (+)
doBinOp Minus  = (-)
doBinOp Mult   = (*)
doBinOp Div    = div
doBinOp Pow    = (^)
doBinOp Equal  = \x y -> fromBool $ x == y
doBinOp Nequal = \x y -> fromBool $ x /= y
doBinOp Lt     = \x y -> fromBool $ x < y
doBinOp Le     = \x y -> fromBool $ x <= y
doBinOp Gt     = \x y -> fromBool $ x > y
doBinOp Ge     = \x y -> fromBool $ x >= y
doBinOp And    = \x y -> fromBool $ toBool x && toBool y
doBinOp Or     = \x y -> fromBool $ toBool x || toBool y
doBinOp Not    = undefined

computeLExpr :: AST -> Subst -> Maybe Int
computeLExpr (Num x)   _         = Just x
computeLExpr (Ident x) s = Map.lookup x s
computeLExpr (BinOp Div x y) s = do
    lhs <- computeLExpr x s
    rhs <- computeLExpr y s
    guard (rhs /= 0)
    return $ lhs `div` rhs
computeLExpr (BinOp Pow x y) s = do
    lhs <- computeLExpr x s
    rhs <- computeLExpr y s
    guard (rhs >= 0)
    return $ lhs ^ rhs
computeLExpr (BinOp op x y) s = do
    lhs <- computeLExpr x s
    rhs <- computeLExpr y s
    return $ doBinOp op lhs rhs
computeLExpr (UnaryOp Minus x) s = (\y -> (-y)) <$> computeLExpr x s
computeLExpr (UnaryOp Not x) s = (\y -> (fromBool . not . toBool) y) <$> computeLExpr x s

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (Seq ls) conf = foldl (\c ast -> (c >>= eval ast)) (Just conf) ls
eval (If cond thn els) conf = do
    c <- computeLExpr cond (subst conf)
    if toBool c then eval thn conf else eval els conf
eval ast@(While cond body) conf = do
    c <- computeLExpr cond (subst conf)
    let nconf = if toBool c then eval body conf else Nothing
    (nconf >>= eval ast) <|> return conf
eval (Assign v e) conf = do
    val <- computeLExpr e (subst conf)
    let nsubst = Map.alter ((const . Just) val) v (subst conf)
    return $ Conf nsubst (input conf) (output conf)
eval (Write e) conf = do
    val <- computeLExpr e (subst conf)
    return $ Conf (subst conf) (input conf) (val : output conf)
eval (Read v) conf = do
    guard (not $ null (input conf))
    let nsubst = Map.alter ((const . Just . head . input) conf) v (subst conf)
    return $ Conf nsubst ((tail . input) conf) (output conf)
