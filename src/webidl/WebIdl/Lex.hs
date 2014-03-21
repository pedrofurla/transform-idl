
module WebIdl.Lex where

import WebIdl.Literal
import WebIdl.Helper

import GHC.Exts(sortWith)
import Control.Applicative((<$>))
import Control.Monad(liftM)
import Data.Maybe(fromMaybe)

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim


{-| Primitive types. ref http://heycam.github.io/webidl/#prod-PrimitiveType -}
cTypes :: [String]
cTypes = 
    ["boolean", "byte", "octet", 
     "short", "long", 
     "integer", 
     "float", "double", 
     "long long", 
     "unsigned short", "unsigned long", 
     "unsigned long long",
     "unrestricted float", "unrestricted double"
    ]

{-cTypesParser =
    let
        prims = ["boolean", "byte", "octet"]
        unsigs = ["short", "long", "long long"]
        unres = ["float","double"]
    in do
        error ""-}

{-| ref http://heycam.github.io/webidl/#proddef-NonAnyType  -}
preDefinedTypes :: [String]
preDefinedTypes = 
    "DOMTimeStamp" : "DOMString" : "ByteString" : "Date" 
    : "RegExp" : "object" : "void" : "any" : cTypes 

noType :: String
noType = "void"

argKeywords :: [String]
argKeywords =
    ["attribute", "callback", "const", "creator", "deleter", "dictionary", "enum", "null", 
     "exception", "getter", "implements", "inherit", "interface", "legacycaller", "partial", 
     "setter", "static", "stringifier", "typedef", "unrestricted"]

longestTok :: String -> Parser String
longestTok s = try (do{ string s; notFollowedBy (alphaNum <|> char '_'); return s}) 
--cToks = cTypes <$>> words

-- TODO rename to parsePredefinedTypes
-- TODO solution to "try . stringTok" and sorting would be to consume everything until a non valid char is found, but then there are spaces in C types.
-- or I can break spaces in them and try to consume bit by bit. This last one seems to be the sensible option (it smells Applicative).
parseCTypes :: Parser String 
parseCTypes = 
    skipWhites $ 
    choice     $ 
    (try . longestTok) <$> sortWith ((100-) . length) preDefinedTypes


keywords :: [String]
keywords =  concat (words <$> cTypes) ++ argKeywords 

isKeyword :: String -> Bool
isKeyword = (`elem` keywords)

identHead :: Parser Char
identHead = letter <|> char '_'

identTail :: Parser Char
identTail = identHead <|> digit

identTok :: Parser String
identTok = skipWhites $ do
    x <- identHead
    xs <-  many identTail
    let i = x : xs
    --if isKeyword i then parserFail $ "`"++i++"` is a reserved word"
    --else 
    return i        
    
value :: Parser Literal
value =  try hexLit <|> numberLit <|> stringLit <|> boolLit <|> (stringTok "null" <$>> const Null)

stringLit :: Parser Literal
stringLit = 
    let escaped = (>>>) $ char '\\' 
        escControl = escaped $ oneOf "tnr\\"
        ch = satisfy (not . flip elem "\"\\") in
    Str <$> between (char '"') (charTok '"') (many (escControl <|> ch)) 

numberLit :: Parser Literal
numberLit = do
    neg <- optionMaybe $ stringTok "-"
    ls <- many1 digit
    rs <- option "" (char '.' >>> (('.':) <$> many1 digit))
    return $ Number $ fromMaybe "" neg ++ ls ++ rs
    
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
    {-skipWhites $ do
    r <- string s
    notFollowedBy identTail
    return r-}


{-| Attempt to parse `p`, giving s if succeeds or f if fails -}
parsePostfixKeyword :: a -> a -> Parser b -> Parser a
parsePostfixKeyword s f p = option f (p >>> return s)

{-| Same as `parsePostfixKeyword` but using `True` and `False` for success and failure respectively -}
parsePostfixBool :: Parser b -> Parser Bool
parsePostfixBool p = option False (p >>> return True)

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


