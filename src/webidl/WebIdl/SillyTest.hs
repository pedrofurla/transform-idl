module WebIdl.SillyTest where

-- import WebIdl.Ast
import WebIdl.Parser.IMember
import WebIdl.PrettyPrint
import WebIdl.Ast(emptyEAs)

import Control.Applicative((<$>))


-- A silly test for the 
test :: IO ()
test = 
  let 
    op = ($ emptyEAs) <$> operation
  in
    do
      runPrint op "RET OP(T1 ARG1, T2 ARG2, T3 ARG3);"
      runPrint op "RET OP(T1 ARG1, /* */ T2 ARG2, T3 ARG3);"
      runPrint op "RET OP(T1 /**/ ARG1, T2 ARG2, T3 ARG3);"
      runPrint (whites >>> op) "/**/RET/**/ OP(/**/T1/**/ ARG1,\n/* */T2 ARG2, T3 ARG3);"