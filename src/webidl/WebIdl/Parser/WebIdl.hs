{-| Parser for Toplevel definitions -}
module WebIdl.Parser.WebIdl where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Literal
import WebIdl.Helper
import WebIdl.Parser.Type
import WebIdl.Parser.IMember
import WebIdl.Parser.Identifier
import WebIdl.Parser.ExtendedAttributes
import WebIdl.Parser.Arguments

-- import GHC.Exts(sortWith)
import Control.Applicative((<$>))

import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

{-
TODO there is still many opportunities to better abstract over many things these parsers do, for example:
- Things that goes after "=" (const, dictionary attributes, interface attributes - not callback, though)
- All top level definitions (interface, callback, dictionary etc) all share many common data
- Perhaps even implement an IDL parser based on these abstractions
-}

type Def = EAs -> Definition

webIdl :: Parser WebIdl
webIdl = 
    let 
        process :: Parser [Definition]
        process = do 
            whites
            defs <- many (
                do
                    eatt <- extendedAttributes 
                    def <- anyDefinition
                    return $ def eatt
                ) <?> "Top level defitinion"
            eof
            return defs
    in WebIdl <$> process

anyDefinition :: Parser Def
anyDefinition = 
    interface 
    <||> partialInterface
    <||> callback 
    <||> typeDef 
    <||> dictionary
    <||> enum
    -- implements is the only one that doesn't start with a keyword, so it's the last.
    <||> implements 

implements :: Parser Def
implements = do
    i0 <- identifier
    stringTok "implements"
    i1 <- identifier
    endl
    return (Implements i0 i1) <?> "Implements definition" -- eatt

enum :: Parser Def
enum = do
    stringTok "enum"
    i <- identifier
    members <- inBraces $ 
        do 
            ss <- ((\(Str s) -> s) <$>) <$> sepBy (try stringLit) (try $ charTok ',') 
            optional $ charTok ','
            return ss
    endl 
    --members  <- literals <$> (\(Str s) -> s)
    return (Enum i members) <?> "Enum definition"

{- | 
>>> run interface "[Constructor]\n interface SELF : SUPER {}"
Interface "SELF" "Constructor" (Just "SUPER") []

TODO Think of a better way to test:

>>> runFile interface "interface-test1.webidl"
Interface "IFACE" "Constructor" (Just "SUPER") [Attribute "ATT_TYP1" "ATT1" True False,Attribute "ATT_TYP2" "ATT2" True True,Attribute "ATT_TYP3" "ATT3" False False,Operation "OP1" "RET_TYP1" [FormalArg "unsigned long" "ARG1" Nothing,FormalArg "unsigned long" "ARG2" Nothing],Operation "OP2" "void" [FormalArg "ARG_TYP3" "ARG3" Nothing,FormalArg "unsigned long" "ARG4" (Just (Number "2"))]]
-}
interface :: Parser Def
interface = do
    optionMaybe $ stringTok "callback" -- TODO^^ PUT THIS VALUE SOMEWHERE
    stringTok "interface"
    i <- identifier
    inherits <- inheriting 
    members <- interfaceMembers
    endl
    return (Interface i inherits members) <?> "Interface definition"

{- 
    TODO: any thing after ":" can be a type parsed by "parseType", this is wrong, 
    eg. `unsigned long long` can't be inherited, or union types!
-}
inheriting :: Parser (Maybe Type)
inheriting = optional (charTok ':') >>> optionMaybe parseType

partialInterface :: Parser Def
partialInterface = do
    stringTok "partial"
    stringTok "interface"
    i <- identifier
    members <- interfaceMembers
    endl
    return (PartialInterface i members) <?> "Partial Interface definition"

{- |
>>> run typeDef "typedef unsigned long  GLenum;"
TypeDef "GLenum" (Type "unsigned long" False False)
-}
typeDef :: Parser Def
typeDef = do 
    stringTok "typedef"
    typ <- parseType
    i <- identifier
    endl
    return (TypeDef i typ) <?> "typedef definition"

{- | 
>>> run callback "callback DecodeSuccessCallback = void (AudioBuffer decodedData);"
Callback "DecodeSuccessCallback" "" (CallbackDecl "void" [FormalArg "AudioBuffer" "decodedData" Nothing])
-}
callback :: Parser Def
callback = do 
    stringTok "callback"
    ident <- identifier
    charTok '='
    typ <- parseType
    args <- formalArgs
    endl
    return (Callback ident (CallbackDef typ args)) <?> "callback definition"

dictionary :: Parser Def
dictionary = do
    stringTok "dictionary"
    ident <- identifier
    inherits <- inheriting
    members <- inBraces $ many dictAttribute
    endl
    return (Dictionary ident inherits members) <?> "dictionary def"


dictAttribute :: Parser DictAttribute
dictAttribute = do
    eatt <- extendedAttributes
    typ <- parseType
    i <- identifier
    lit <- optionMaybe (charTok '=' >>> value)
    endl
    return (DictAttribute i typ lit eatt) <?> "dictionary attribute"