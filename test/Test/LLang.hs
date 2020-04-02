module Test.LLang where

import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import LLang (LAst(..), parseL)
import AST (AST(..), Operator(..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol, strEq)

unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "write(1+2);" @?= Success "" Seq{statements = [Write (BinOp Plus (Num 1) (Num 2))]}
    runParser parseL "" @?= Success "" Seq{statements = []}
    runParser parseL "read(x);" @?= Success "" Seq{statements = [Read "x"]}
    runParser parseL "x =   567^123;" @?= Success "" Seq{statements = [Assign "x" (BinOp Pow (Num 567) (Num 123))]}
    runParser parseL "while(x<22)       \n\
    \ \n\
    \   {\n\
    \ x = x+1;\n\
    \  \n\
    \}" @?= Success "" Seq{statements = [While (BinOp Lt (Ident "x") (Num 22)) Seq{statements = [Assign "x" (BinOp Plus (Ident "x") (Num 1))]}]}
    runParser parseL "if(x<22)       \n\
    \ \n\
    \ \n\
    \   {\n\
    \ x = x+1;\n\
    \  \n\
    \}" @?= Success "" Seq{statements = [If (BinOp Lt (Ident "x") (Num 22)) 
    Seq{statements = [Assign "x" (BinOp Plus (Ident "x") (Num 1))]} Seq{statements = []}]}
    runParser parseL "if(x<22) {\n\
    \ x = x+1;\n\
    \  \n\
    \}   else    {\n\
    \  \n\
    \ x=x-1;\n\
    \  \n\
    \  \n\
    \ }" @?= Success "" Seq{statements = [If (BinOp Lt (Ident "x") (Num 22)) 
    Seq{statements = [Assign "x" (BinOp Plus (Ident "x") (Num 1))]} Seq{statements = [Assign "x" (BinOp Minus (Ident "x") (Num 1))]}]}
    runParser parseL "read(X);\n\
    \ if(X>13) {\n\
        \write(X);\n\
       \} else {\n\
       \while(X<42) {\n\
            \X = X*7;\n\
            \write(X);\n\
            \}\n\
        \}" @?= Success "" (Seq [ Read "X"
                        , If (BinOp Gt (Ident "X") (Num 13))
                             Seq {statements = [(Write (Ident "X"))]}
                             Seq {statements = [(While (BinOp Lt (Ident "X") (Num 42))
                                    (Seq [ Assign "X"
                                            (BinOp Mult (Ident "X") (Num 7))
                                         , Write (Ident "X")
                                         ]
                                    )
                             )]}
                        ])