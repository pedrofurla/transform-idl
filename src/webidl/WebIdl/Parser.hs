module WebIdl.Parser where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Literal
import WebIdl.Helper

-- import GHC.Exts(sortWith)
import Data.Traversable
import Control.Applicative((<$>), liftA2)
import Data.Maybe(fromMaybe)

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

{- |
$setup

-}

{-
TODO there is still many opportunities to better abstract over many things these parses do, for example:
- Things that goes after "=" (const, dictionary attributes, interface attributes - not callback, though)
- All top level definitions (interface, callback, dictionary etc) all share many common data
- Perhaps even implement an IDL parser based on these abstractions
-}

webIdl :: Parser WebIdl
webIdl = 
    let 
        process :: Parser [Definition]
        process = do 
            whites
            many (try interface 
                <|>try callback 
                <|>try typeDef 
                <|>try dictionary
                <|>try implements
                <|>try enum) <?> "Top level defitinion"
    in WebIdl <$> process

implements :: Parser Definition
implements = do
    eatt <- extendedAtt
    i0 <- identifier
    stringTok "implements"
    i1 <- identifier
    endl
    return $ Implements i0 i1 eatt

enum :: Parser Definition
enum = do
    eatt <- extendedAtt
    stringTok "enum"
    i <- identifier
    members <- inBraces $ ((\(Str s) -> s) <$>) <$> (sepBy stringLit $ charTok ',') 
    endl 
    --members  <- literals <$> (\(Str s) -> s)
    return (Enum i members eatt) <?> "Enum definition"

{- | 
>>> run interface "[Constructor]\n interface SELF : SUPER {}"
Interface "SELF" "Constructor" (Just "SUPER") []

TODO Think of a better way to test:

>>> runFile interface "interface-test1.webidl"
Interface "IFACE" "Constructor" (Just "SUPER") [Attribute "ATT_TYP1" "ATT1" True False,Attribute "ATT_TYP2" "ATT2" True True,Attribute "ATT_TYP3" "ATT3" False False,Operation "OP1" "RET_TYP1" [FormalArg "unsigned long" "ARG1" Nothing,FormalArg "unsigned long" "ARG2" Nothing],Operation "OP2" "void" [FormalArg "ARG_TYP3" "ARG3" Nothing,FormalArg "unsigned long" "ARG4" (Just (Number "2"))]]
-}
interface :: Parser Definition 
interface = do
    eatt <- extendedAtt
    partial <- optionMaybe $ stringTok "partial"
    stringTok "interface"
    i <- identifier
    inherits <- inheriting 
    members <-option [] $ inBraces $ 
        many (try attribute 
            <|> try operation 
            <|> try constVal
            <|> try getter
            <|> try setter
            <|> try creator
            <|> try deleter)
    endl
    return (Interface i inherits (Partial . justTrue $ partial) members eatt) <?> "Interface definition"

{- |
    TODO: any thing after ":" can be a type parsed by "parseType", this is wrong, 
    eg. `unsigned long long` can't be inherited
-}
inheriting :: Parser (Maybe Type)
inheriting = do 
        optional $ charTok ':' -- TODO this is wrong. if ":" is present the "type" is obligatory
        optionMaybe parseType

{- |
>>> run typeDef "typedef unsigned long  GLenum;"
TypeDef "GLenum" (Type "unsigned long" False False)
-}
typeDef :: Parser Definition
typeDef = do 
    stringTok "typedef"
    eatt <- extendedAtt
    typ <- parseType
    i <- identifier
    endl
    return (TypeDef i typ eatt) <?> "typedef definition"

{- | 
>>> run callback "callback DecodeSuccessCallback = void (AudioBuffer decodedData);"
Callback "DecodeSuccessCallback" "" (CallbackDecl "void" [FormalArg "AudioBuffer" "decodedData" Nothing])
-}
callback :: Parser Definition
callback = do 
    eatt <- extendedAtt
    stringTok "callback"
    ident <- identifier
    charTok '='
    typ <- parseType
    args <- formalArgs
    endl
    return (Callback ident (CallbackDef typ args) eatt) <?> "callback definition"

dictionary :: Parser Definition
dictionary = do
    eatt <- extendedAtt
    stringTok "dictionary"
    ident <- identifier
    inherits <- inheriting
    members <- inBraces $ many dictAttribute
    endl
    return (Dictionary ident inherits members eatt) <?> "dictionary def"

dictAttribute :: Parser DictAttribute
dictAttribute = do
    eatt <- extendedAtt
    typ <- parseType
    i <- identifier
    lit <- optionMaybe (charTok '=' >>> value)
    endl
    return (DictAttribute i typ lit eatt) <?> "dictionary attribute"


{- |
>>> run operation "AudioBuffer createBuffer(unsigned long numberOfChannels, unsigned long length, float sampleRate);"
Operation "createBuffer" "AudioBuffer" [FormalArg "unsigned long" "numberOfChannels" Nothing,FormalArg "unsigned long" "length" Nothing,FormalArg "float" "sampleRate" Nothing]
-}
operation :: Parser IMember
operation = do
    eatt <- extendedAtt
    typ <- parseType
    ident <- identifier
    args <- formalArgs
    endl
    return (Operation ident typ args eatt) <?> "operation"

formalArgs :: Parser [FormalArg]
formalArgs = (inParens $ sepBy (try regularArg <|> try variadicArg) $ charTok ',') <?> "argument list"

variadicArg :: Parser FormalArg
variadicArg = do
    eatt <- extendedAtt
    typ <- parseType <?> "type of formal argument"
    stringTok "..."
    i <- identifier <?> "identifier of formal argument"
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
    eatt <- extendedAtt
    opt <- optionMaybe $ stringTok "optional" --  opt :: Maybe String
    typ <- parseType <?> "type of formal argument"
    i <- identifier <?> "identifier of formal argument"
    df <- (id =<< ) <$> const (optionMaybe $ charTok '=' >>> value) `traverse` opt
    return (RegularArg i typ (Optional $ justTrue opt) df eatt) <?> "argument"

{- |
>>> run attribute "readonly attribute TYPE NAME;"
Attribute "TYPE" "NAME" True False
>>> run attribute "inherit readonly attribute TYPE NAME;"
Attribute "TYPE" "NAME" True True
-}
attribute :: Parser IMember
attribute = do
    eatt <- extendedAtt
    inherit <- optionMaybe $ stringTok "inherit"
    readonly <- optionMaybe $ stringTok "readonly"
    stringTok "attribute"
    typ <- parseType
    i <- identifier
    endl
    return (Attribute i typ ((ReadOnly . justTrue) readonly) ((Inherit . justTrue) inherit) eatt) <?> "attribute"

getter :: Parser IMember
getter = special1 "getter" Getter

deleter :: Parser IMember
deleter = special1 "deleter" Deleter

setter :: Parser IMember
setter = special2 "setter" Setter

creator :: Parser IMember
creator = special2 "creator" Creator

{-| Special is how the above keywords are named in the WebIDL spec. -}
special1 :: String -> (Maybe Ident -> Type -> FormalArg -> b) -> Parser b
special1 tok cons = do
    _eatt <- extendedAtt
    stringTok tok
    typ <- parseType
    i   <- optionMaybe identifier
    arg <- inParens regularArg
    endl
    return $ cons i typ arg

special2 :: String -> (Maybe Ident -> Type -> FormalArg -> FormalArg -> b) -> Parser b
special2 tok cons = do
    _eatt <- extendedAtt
    stringTok tok
    typ <- parseType
    i   <- optionMaybe identifier
    (arg0, arg1) <- inParens $ liftA2 (,) regularArg regularArg
    endl
    return $ cons i typ arg0 arg1

extendedAtt :: Parser ExtendedAtt
extendedAtt = -- TODO: skipWhites is really needed here? should it be before or after `option`?
    ExtendedAtt <$> 
      option [] (skipWhites $ inBrackets $ sepBy extendedAttributes $ charTok ',') 

extendedAttributes :: Parser ExtendedAttributes
extendedAttributes = 
    either OtherEA id <$> readableOr
     

readableOr :: Parser (Either String ExtendedAttributes) 
readableOr = do
    chs <- many (noneOf ",]")
    -- TODO this chs repetition seems to be ask for an Applicative...
    return $ fromMaybe (Left chs) (Right <$> maybeRead chs)

{- |
>>> run constVal "const TYPE NAME = 0x0000;"
Const "NAME" (Type "TYPE" False False) (Hex "0000")
-}
constVal :: Parser IMember 
constVal = do 
    eatt <- extendedAtt
    stringTok "const"
    typ <- parseType
    i <- identifier
    charTok '='
    lit <- value
    endl
    return (Const i typ lit eatt) <?> "const definition"


{- TODO 
  Still misses:
  - sequence<>
  - any
  - object
  - union types
    
  http://www.w3.org/TR/WebIDL/#prod-Type

  TODO DOCTESTS
-}
parseType :: Parser Type
parseType = do
    -- TODO sequence<T> where T can be another type, write now I am ignoring the recursivity
    -- also, T itself can be/have "[]" "?", this is also currently ignored
    sequ <- optionMaybe $ try $ stringTok "sequence<" -- :: Parser Maybe String
    i <- (Ident <$> try parseCTypes) <|> identifier 
    const (stringTok ">") `traverse` sequ
    array <- optionMaybe $ string "[]"
    nullable <- optionMaybe $ char '?'
    whites
    return 
        (Type i 
          ((Nullable . justTrue) nullable) ((Array . justTrue) array) ((Sequence . justTrue) sequ)) <?> "type declaration"

identifier :: Parser Ident
identifier = Ident <$> (identTok <?> "identifier")

