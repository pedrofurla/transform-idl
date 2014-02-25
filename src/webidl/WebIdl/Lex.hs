
module WebIdl.Lex where

import WebIdl.Helper

import GHC.Exts(sortWith)
-- import Data.Traversable
import Control.Applicative((<$>))
import Control.Monad(liftM)
-- import Data.Maybe(fromMaybe)

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim


cTypes :: [String]
cTypes = 
    ["byte", "octet", "short", "unsigned short", "long", "unsigned long", 
     "long long", "unsigned long long",
     "boolean", "integer", "float", "unrestricted float", "double", "unrestricted double",
     "void"
    ]

noType :: String
noType = "void"

argKeywords :: [String]
argKeywords =
    ["attribute", "callback", "const", "creator", "deleter", "dictionary", "enum", 
     "exception", "getter", "implements", "inherit", "interface", "legacycaller", "partial", 
     "setter", "static", "stringifier", "typedef", "unrestricted"]

keywords :: [String]
keywords =  concat (words <$> cTypes) ++ argKeywords 

isKeyword :: String -> Bool
isKeyword = (`elem` keywords)

identTok :: Parser String
identTok = skipWhites $ do
    x <- letter <|> char '_'
    xs <-  many $ letter <|> char '_' <|> digit
    -- TODO: relexing reserved keyword checking, it's conflicting with types (which are also identfiers)
    -- let i = x : xs
    -- if isKeyword i then parserFail $ "`"++i++"` is a reserved word"
    -- else 
    return $ x : xs

parseCTypes :: Parser String
parseCTypes = skipWhites $ choice $ (try . string) <$> sortWith ((100-) . length) cTypes

data Literal = 
      Number String
    | Hex String
    | Str String 
    | Boolean String deriving (Show, Eq)

value :: Parser Literal
value =  try hexLit <|> numberLit <|> stringLit <|> boolLit

stringLit :: Parser Literal
stringLit = 
    let escaped = (>>>) $ char '\\' 
        escControl = escaped $ oneOf "tnr\\"
        ch = satisfy (not . flip elem "\"\\") in
    Str <$> between (char '"') (charTok '"') (many (escControl <|> ch)) 

numberLit :: Parser Literal
numberLit = do
    ls <- many1 digit
    rs <- option "" (char '.' >>> (('.':) <$> many1 digit))
    return $ Number $ ls ++ rs
    
hexLit :: Parser Literal
hexLit = liftM Hex (string "0x" >>> many1 hexDigit)

boolLit :: Parser Literal
boolLit = Boolean <$> (string "true" <|> string "false")

inParens :: Parser a -> Parser a
inParens = betweenTok (char '(') (char ')')

inBraces :: Parser a -> Parser a
inBraces = betweenTok (char '{') (char '}')

inBrackets :: Parser a -> Parser a
inBrackets = betweenTok (char '[') (char ']')

betweenTok :: Parser open -> Parser close -> Parser a -> Parser a
betweenTok o c = between (skipWhites o) (skipWhites c)

charTok :: Char -> Parser Char
charTok = skipWhites . char

stringTok :: String -> Parser String
stringTok = skipWhites . string

-- | Reads a sequence of any non-visible " \t\n\r" characteres and comments
whites :: Parser ()
whites = do
    invisibles
    option () (lineComment >>> whites)
    option () (blockComment >>> whites)
    return () <?> "Ignored lexemes"

eol :: Parser Char
eol = char '\n' <|> (char '\r' >>> char '\n')

invisibles :: Parser ()
invisibles = skipMany $ oneOf " \t" <|> eol

lineComment :: Parser ()
lineComment = 
    try (string "//") >>> (const () <$> manyTill anyChar (try eol))

blockComment :: Parser ()
blockComment = -- TODO try $ string "/*" will have the same meaning regarding >>>?
    const () <$> try (string "/*") >>> manyTill anyChar (try $ string "*/")

comments :: Parser ()
comments = skipMany $ (lineComment <|> blockComment) >>> whites

-- | Skips whites after parsing p
skipWhites :: Parser a -> Parser a
skipWhites p = do
    xs <- p
    whites
    return xs

endl :: Parser ()
endl = do
    skipWhites $ char ';'
    --skipMany (lineComment >>> whites)
    whites
    return () <?> "End of line marker ';'"


