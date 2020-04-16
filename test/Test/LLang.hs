module Test.LLang where

import           AST              (AST (..), Operator (..))
import           Combinators      (Parser (..), Result (..), runParser, strEq,
                                   symbol, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang            (Configuration (..), LAst (..), eval,
                                   initialConf, parseL, Function (..), Program (..))
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

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

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Seq [(Write (Num 1))])
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing
