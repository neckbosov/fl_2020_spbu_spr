module LEval where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (InputStream (..), Result (..), runParser)
import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as Map
import           LLang               (Configuration (..), Expr, Function (..),
                                      LAst (..), Program (..), parseProg)
evalProg :: Program -> [Int] -> Maybe Configuration
evalProg prog input = eval (main prog) conf
    where
        conf = Conf Map.empty input [] (Map.fromList funcs)
        funcs = map (\f -> (name f, f)) (functions prog)

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg s input = case runParser parseProg s of
    Success (InputStream "" _) res -> evalProg res input
    _                              -> Nothing

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
doBinOp Not    = error "Not isn't binary operator"

evalExpr :: Configuration -> AST -> Maybe (Configuration, Int)
evalExpr c (Num x)   = return (c, x)
evalExpr c (Ident x) = (,) c <$> Map.lookup x (subst c)
evalExpr c (BinOp Div x y) = do
    (c1, lhs) <- evalExpr c x
    (c2, rhs) <- evalExpr c1 y
    guard $ rhs /= 0
    return (c2, lhs `div` rhs)
evalExpr c (BinOp Pow x y) = do
    (c1, lhs) <- evalExpr c x
    (c2, rhs) <- evalExpr c1 y
    guard $ rhs >= 0
    return (c2, lhs ^ rhs)
evalExpr c (BinOp op x y) = do
    (c1, lhs) <- evalExpr c x
    (c2, rhs) <- evalExpr c1 y
    return (c2, doBinOp op lhs rhs)
evalExpr c (UnaryOp Minus x) = fmap negate <$> evalExpr c x
evalExpr c (UnaryOp Not x) = fmap (fromBool . not . toBool) <$> evalExpr c x
evalExpr c (FunctionCall s exprs) = do
    f <- Map.lookup s (defs c)
    guard $ length (args f) == length exprs
    (nconf, vals) <- foldM (\(c, ls) e -> fmap (: ls) <$> evalExpr c e) (c, []) exprs
    let fconf = nconf {subst = Map.fromList $ zip (args f) (reverse vals)}
    (nnconf, res) <- evalFun fconf (funBody f) (returnExpr f)
    return (nconf {input = input nnconf, output = output nnconf}, res)

evalFun :: Configuration -> LAst -> Expr -> Maybe (Configuration, Int)
evalFun c b ret = do
    c1 <- eval b c
    evalExpr c1 ret

eval :: LAst -> Configuration -> Maybe Configuration
eval (Seq ls) conf = foldl (\c ast -> (c >>= eval ast)) (Just conf) ls
eval (If cond thn els) conf = do
    (nconf, val) <- evalExpr conf cond
    if toBool val then eval thn nconf else eval els nconf
eval ast@(While cond body) conf = do
    (nconf, val) <- evalExpr conf cond
    let nnconf = if toBool val then eval body nconf else Nothing
    (nnconf >>= eval ast) <|> return nconf
eval (Assign v e) conf = do
    (nconf, val) <- evalExpr conf e
    return $ nconf {subst = Map.alter ((const . Just) val) v (subst nconf)}
eval (Write e) conf = do
    (nconf, val) <- evalExpr conf e
    return $ nconf {output = val : output nconf}
eval (Read v) conf = do
    guard $ (not . null . input) conf
    let nsubst = Map.alter ((const . Just . head . input) conf) v (subst conf)
    return $ conf {subst = nsubst, input = (tail . input) conf}
