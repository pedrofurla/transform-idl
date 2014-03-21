{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where 

import Text.HandsomeSoup
import Text.XML.HXT.Core
import System.Environment(getArgs,getProgName)
import Data.Maybe(fromMaybe)

main :: IO ()
main = do 
    args <- getArgs
    progName <- getProgName
    let maybeEffect = (\(u,p) -> write p $ w3cIdl u) `fmap` processArgs args
    fromMaybe (putStrLn $ progName ++ " <url> [<output-path>] # default output path is 'idl/'") maybeEffect
        

processArgs :: [String] -> Maybe (String,String)
processArgs []   =  Nothing --(defaultUrl, defaultOutputPath)
processArgs [u]  = Just (u, defaultOutputPath)
processArgs (u : p : _) = Just (u,if last p == '/' then p else p ++ "/") 
 
defaultOutputPath = "idl/"
defaultUrl = "http://www.w3.org/TR/webaudio/"

w3cIdl :: String -> IO [(String, String)]
w3cIdl url = 
    let doc = fromUrl url in
    runX $ doc >>> css "code.idl-code" >>> hasAttr "id" >>> ( getAttrValue "id" &&& (multi getText >. concat)  ) 
    

webAudio:: IO [(String,String)]
webAudio = w3cIdl defaultUrl

showIt :: IO [(String,String)] -> IO ()
showIt xml = do xs <- xml; mapM_ (\(t,c) -> putStrLn t >> putStrLn "\n-------\n" >> putStrLn c) (take 10 xs)

write :: String -> IO [(String,String)] -> IO () 
write path xml = do xs <- xml; mapM_ (\(t,c) -> writeFile (concat [path, t,".idl"]) c) xs