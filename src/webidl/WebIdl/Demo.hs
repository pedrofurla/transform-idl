module WebIdl.Demo where

import WebIdl.Helper
import WebIdl.Parser.WebIdl
import WebIdl.PrettyPrint
import WebIdl.Mozilla
import WebIdl.Ast

import Text.Parsec.Prim()

pprintFile :: (WebIdl -> String) -> String -> IO ()
pprintFile pp filename = 
    cppFile filename <$>> runWith pp webIdl >>= either (putStrLn . show) putStrLn 
    
pprintWebGl :: IO ()
pprintWebGl = pprintFile pprint "idl/webgl.idl"

pprintGeoL :: IO ()
pprintGeoL = pprintFile pprint "idl/geolocation.idl"

pprintMozWindow :: IO ()
pprintMozWindow = pprintFile pprint  $ mozHome ++ "Window.webidl"

printAudio :: IO ()
printAudio = 
    pprintFile pprint "idl/audio/audiocontext.webidl"
    



-- readFile "idl/geolocation.idl" <$>> parse webIdl "" <$>> (pprint <$>) >>= \(Right j) -> return j