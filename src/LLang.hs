module LLang where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), satisfy, success, symbol,
                                      word, eof)
import           Control.Applicative
import           Control.Monad
import           Data.List           (intercalate)
import qualified Data.Map            as Map
import           Expr                (parseBracketsExpr, parseExpr, parseIdent)
import           Text.Printf         (printf)
import           Util                (bracketize, parseSpace, parseSpaces,
                                      parseWs, parseWss, spaceSurrounded,
                                      wsSurrounded)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }
    deriving Eq

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }
              deriving (Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

parseProg :: Parser String String Program
parseProg = Program <$> many (parseDef <* parseWss) <*> parseL <* parseWss <* eof

parseDef :: Parser String String Function
parseDef = uncurry <$> parseDeclaration <*> parseDefBlock
    where
        parseDeclaration = Function <$ word "fun" <* parseSpace <*> parseIdent <*> (bracketize parseArgs) <* parseWss
        parseArgs = ((:) <$> parseIdent <*> many (symbol ',' *> parseWss *> parseIdent)) <|> success []

parseL :: Parser String String LAst
parseL = parseSeq

parseSeq :: Parser String String LAst
parseSeq = Seq <$> many (parseInstruction <* parseWss)

parseDefBlock :: Parser String String (LAst, Expr)
parseDefBlock = (,) <$ symbol '{' <* parseWss <*> parseSeq  <*> (parseReturn <|> success (Num 0)) <* parseWss <* symbol '}'

parseBlock :: Parser String String LAst
parseBlock = symbol '{' *> wsSurrounded parseSeq <* symbol '}'

parseIf :: Parser String String LAst
parseIf = If <$ word "if" <* parseSpaces <*> parseBracketsExpr <* parseWss <*> parseBlock <*> parseElse
    where
        parseElse = (wsSurrounded (word "else") *> parseBlock) <|> success (Seq [])

parseWhile :: Parser String String LAst
parseWhile = While <$ word "while" <* parseSpaces <*> parseBracketsExpr <* parseWss <*> parseBlock

parseAssign :: Parser String String LAst
parseAssign = Assign <$> parseIdent <* spaceSurrounded (symbol '=') <*> parseExpr <* symbol ';'

parseRead :: Parser String String LAst
parseRead = Read <$ word "read" <*> (bracketize parseIdent) <* symbol ';'

parseWrite :: Parser String String LAst
parseWrite = Write <$ word "write" <*> parseBracketsExpr <* symbol ';'

parseReturn :: Parser String String Expr
parseReturn = word "return" *> parseWss *> parseExpr <* symbol ';'

parseInstruction :: Parser String String LAst
parseInstruction = parseAssign <|> parseIf <|> parseWhile <|> parseRead <|> parseWrite

instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
