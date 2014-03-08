module WebIdl.Demo where

-- import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Helper
import WebIdl.Parser
import WebIdl.PrettyPrint

import Text.Parsec.Prim()

pprintFile :: String -> IO String
pprintFile filename =  do
    input <- readFile filename
    return $ 
      case runWith pprint webIdl input of
          Left x -> "Error: " ++ show x
          Right x -> x
    
pprintWebGl :: IO String
pprintWebGl = pprintFile "idl/webgl.idl"

pprintGeoL :: IO String
pprintGeoL = pprintFile "idl/geolocation.idl"

printAudio :: IO ()
printAudio =  do
    input <- readFile "idl/audio/audiocontext.webidl"
    run webIdl input    

test :: IO ()
test = do
    run operation "RET OP(T1 ARG1, T2 ARG2, T3 ARG3);"
    run operation "RET OP(T1 ARG1, /* */ T2 ARG2, T3 ARG3);"
    run operation "RET OP(T1 /**/ ARG1, T2 ARG2, T3 ARG3);"
    run (whites >>> operation) "/**/RET/**/ OP(/**/T1/**/ ARG1,\n/* */T2 ARG2, T3 ARG3);"

