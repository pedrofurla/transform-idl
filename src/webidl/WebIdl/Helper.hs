{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WebIdl.Helper where

import Text.Parsec.Prim
import Text.Parsec.Error
import Text.Parsec.String

import Control.Applicative((<$>))
import Language.Preprocessor.Cpphs

-- $setup
-- >>> import Text.Parsec.Char
-- >>> import Text.Parsec.Combinator

type Id a = a

{- |
>>> run ((char '\\') >>> (char 't')) "\\t"
't'
>>> run ((many1 digit) >>> (many1 letter)) "123abc"
"abc"
-}
(>>>) :: Parser a -> Parser b -> Parser b
a >>> b = do
    a
    b 

infixl 4 <$>>

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) = (<|>) <$> try <$> try

infixr 1 <||>

-- | An infix synonym for 'fmap' that flips it's arguments. eg. [1,2] <$>> (+1) == [2,3]
--   it's a bit analogous to `=<<` vs `>>=`
--   It's been said that some libs use `<&>` for that
(<$>>) :: Functor f => f a -> (a -> b) -> f b
(<$>>) = flip fmap

runPrint :: Show a => Parser a -> String -> IO ()
runPrint p input
        = case parse p "" input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

runWith :: (Show a, Show b) => (a -> b) -> Parser a -> String -> Either ParseError b
runWith f p input = f `fmap` parse p "" input
            
runFile :: Show a => Parser a -> String -> IO ()
runFile p fname = do
    input <- cppFile fname
    runPrint p input

cppOpts :: CpphsOptions
cppOpts = defaultCpphsOptions { boolopts=defaultBoolOptions { locations = False, hashline = False, lang = False }}

cpp :: String -> IO String
cpp = runCpphs cppOpts ""

deriving instance Show BoolOptions
deriving instance Show CpphsOptions 

cppFile :: String -> IO String
cppFile file = cpp =<< readFile file

cppFiles :: [String] -> IO String
cppFiles files = 
    concat <$> sequence (cppFile <$> files)

maybeRead ::
  Read a =>
  String
  -> Maybe a
maybeRead s =
  case reads s of
    [] -> Nothing
    ((a, _):_) -> Just a   

-- grep :: String -> String
-- grep s = "grep --exclude output.tmp "++ concat (("--exclude "++) <$> mozIdlNames)++ ""++ s ++ "* >> output.tmp; \n"


otherIdls :: [String]
otherIdls = [
    -- https://www.khronos.org/registry/typedarray/specs/latest/
    "idl/typedarray.idl"
    ]

-- Concatenates all cpp-ed files into one big file
writeFiles :: String -> [String] -> IO ()
writeFiles out idls = cppFiles idls >>= writeFile out


