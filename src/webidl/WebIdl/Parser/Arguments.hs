module WebIdl.Parser.Arguments where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Helper
import WebIdl.Parser.Identifier
import WebIdl.Parser.Type
import WebIdl.Parser.ExtendedAttributes(extendedAttributes)

import Control.Applicative((<$>))
import Data.Traversable(traverse)
import Data.Maybe(isJust)

import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim


formalArgs :: Parser [FormalArg]
formalArgs = inParens (sepBy (regularArg <||> variadicArg) $ charTok ',') <?> "argument list"

variadicArg :: Parser FormalArg
variadicArg = do
    eatt <- extendedAttributes
    typ <- parseType 
    stringTok "..."
    i <- identifier
    return (VariadicArg i typ eatt) <?> "variadic argument"
    
{- |
>>> run regularArg "AudioBuffer decodedData"
FormalArg "AudioBuffer" "decodedData" Nothing
>>> run regularArg "optional Bleh bufferSize = 0"
FormalArg "Bleh" "bufferSize" (Just (Number "0"))
>>> run regularArg "optional unsigned long bufferSize = \"aaa\""
FormalArg "unsigned long" "bufferSize" (Just (Str "aaa"))
-}
regularArg :: Parser FormalArg
regularArg = do
    eatt <- extendedAttributes
    opt <- optionMaybe $ try $ stringTok "optional" --  opt :: Maybe String
    typ <- parseType
    i <- identifier
    df <- (id =<<) <$> const (optionMaybe $ charTok '=' >>> value) `traverse` opt
    return (RegularArg i typ (Optional $ isJust opt) df eatt) <?> "argument"      