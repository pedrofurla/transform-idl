{-| Parser for types -}
module WebIdl.Parser.Type where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Parser.Identifier

-- import GHC.Exts(sortWith)
import Control.Applicative((<$>))

import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

{- Reference: http://www.w3.org/TR/WebIDL/#prod-Type
  
  TODO void is only accepted as return type

  TODO DOCTESTS
-}
parseRegularType :: Parser Type 
parseRegularType = do
    i <- parseTypeId
    arr <- array
    nul <- nullable
    return (Type i nul arr) <?> "type declaration"

parseSequenceType :: Parser Type
parseSequenceType = do
    stringTok "sequence<"
    t <- parseType -- parseRegularType
    charTok '>'
    nul <- nullable
    return (Sequence t nul) <?> "sequence type declaration"

parseUnionType :: Parser Type
parseUnionType = do
    ts <- inParens $ sepBy parseType $ stringTok "or "        
    arr <- array
    nul <- nullable
    return (Union ts nul arr) <?> "union type declaration"

parseType :: Parser Type
parseType = do
       t <- try parseSequenceType
           <|> parseUnionType
           <|> parseRegularType
       whites
       return t

parseTypeId :: Parser Ident
parseTypeId = (Ident <$> try parseCTypes) <|> identifier       

nullable :: Parser Nullable
nullable = Nullable <$> parsePostfixBool (charTok '?')

array    :: Parser Array
array    = Array <$> parsePostfixBool (stringTok "[]") 
