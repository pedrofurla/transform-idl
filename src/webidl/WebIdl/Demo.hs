module WebIdl.Demo where

-- import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Helper
import WebIdl.Parser
import WebIdl.PrettyPrint

import Text.Parsec.Prim
-- import Text.Parsec.String

main :: IO ()
main =  do
    input <- readFile "idl/webgl.idl"
    putStrLn $ 
      case (runWith pprint webIdl input) of
          Left x -> "Error: " ++ show x
          Right x -> x
    

main2 :: IO ()
main2 =  do
    input <- readFile "idl/audio/audiocontext.webidl"
    run webIdl input    

test = do
    run operation "RET OP(T1 ARG1, T2 ARG2, T3 ARG3);"
    run operation "RET OP(T1 ARG1, /* */ T2 ARG2, T3 ARG3);"
    run operation "RET OP(T1 /**/ ARG1, T2 ARG2, T3 ARG3);"
    run (whites >>> operation) "/**/RET/**/ OP(/**/T1/**/ ARG1,\n/* */T2 ARG2, T3 ARG3);"

