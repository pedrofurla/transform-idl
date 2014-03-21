{-| Parser for interface members -}
module WebIdl.Parser.IMember where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex
import WebIdl.Helper
import WebIdl.Parser.Type
import WebIdl.Parser.ExtendedAttributes
import WebIdl.Parser.Identifier
import WebIdl.Parser.Arguments

-- import GHC.Exts(sortWith)
import Control.Applicative((<$>), liftA2)
import Data.Maybe(isJust)

import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

type IM = EAs -> IMember
type PIM = Parser (IM)

interfaceMembers :: Parser [IMember]
interfaceMembers = 
    option [] $ inBraces $ 
        many (
            do 
                eatt <- extendedAttributes
                im <- anyInterfaceMember
                return $ im eatt
            <|> mozillaIMemberWorkaround
        )

-- a hack to make it work with a crazy keyword Mozilla decided is part of the spec (but is not.)
mozillaIMemberWorkaround :: Parser IMember
mozillaIMemberWorkaround =
    try $ do 
            tok <- stringTok "jsonifier" <|> stringTok "stringifier"
            endl
            return (Attribute (Ident $ "__"++tok) 
                  (Type (Ident "void") (Nullable False) (Array False))
                  (ReadOnly False) (Inherit False) (ExtendedAttributes []))
        

anyInterfaceMember :: PIM
anyInterfaceMember = 
    getter
    <||> setter
    <||> creator
    <||> deleter 
    <||> attribute 
    <||> unamedOperation
    <||> operation 
    <||> constVal

getter :: PIM
getter = special1 "getter" Getter

deleter :: PIM
deleter = special1 "deleter" Deleter

{-| Special is how the above keywords are named in the WebIDL spec. -}
special1 :: String -> (Maybe Ident -> Type -> FormalArg -> b) -> Parser b
special1 tok cons = do
    stringTok tok
    typ <- parseType
    i   <- optionMaybe identifier
    arg <- inParens regularArg
    endl
    return $ cons i typ arg

{-- Setter can be also have the keyword creator, so it's ignored. -}
setter :: PIM
setter = special2 "setter" "creator" Setter

{-| According to the spec, creator has the same general semantics as JS' setters, so, it creates a setter. -}
creator :: PIM
creator = special2 "creator" "setter" Setter

{- -}
special2 :: String -> String -> (Maybe Ident -> Type -> FormalArg -> FormalArg -> b) -> Parser b
special2 tok ignore cons = do
    stringTok tok
    optional $ try $ stringTok ignore
    typ <- parseType
    i   <- optionMaybe identifier
    (arg0, arg1) <- inParens $ liftA2 (,) regularArg (charTok ',' >>> regularArg)
    endl
    return $ cons i typ arg0 arg1

{- |
>>> run operation "AudioBuffer createBuffer(unsigned long numberOfChannels, unsigned long length, float sampleRate);"
Operation "createBuffer" "AudioBuffer" [FormalArg "unsigned long" "numberOfChannels" Nothing,FormalArg "unsigned long" "length" Nothing,FormalArg "float" "sampleRate" Nothing]
-}
operation :: PIM
operation = do
    option False $ const True <$> try (stringTok "legacycaller") -- TODO^^
    option False $ const True <$> try (stringTok "static") -- TODO^^
    typ <- parseType
    ident <- identifier
    args <- formalArgs
    endl
    return (Operation ident typ args) <?> "operation"

{-| http://heycam.github.io/webidl/#idl-legacy-callers -}
unamedOperation :: PIM
unamedOperation = do
    stringTok "legacycaller" <|> stringTok "stringifier" -- TODO^^
    typ <- parseType
    args <- formalArgs
    endl
    return (Operation (Ident "") typ args) <?> "unamed operation"    

{- |
>>> run attribute "readonly attribute TYPE NAME;"
Attribute "TYPE" "NAME" True False
>>> run attribute "inherit readonly attribute TYPE NAME;"
Attribute "TYPE" "NAME" True True
-}
attribute :: PIM
attribute = do
    -- Array <$> parsePostfixBool (string "[]")
    optionMaybe $ stringTok "stringifier" -- TODO do we really care about that?
    inherit <- optionMaybe $ stringTok "inherit"
    readonly <- optionMaybe $ stringTok "readonly"
    stringTok "attribute"
    typ <- parseType
    i <- identifier
    endl
    return (Attribute i typ ((ReadOnly . isJust) readonly) ((Inherit . isJust) inherit)) <?> "attribute interface member"

{- |
>>> run constVal "const TYPE NAME = 0x0000;"
Const "NAME" (Type "TYPE" False False) (Hex "0000")
-}
constVal :: PIM
constVal = do 
    stringTok "const"
    typ <- parseType
    i <- identifier
    charTok '='
    lit <- value
    endl
    return (Const i typ lit) <?> "const interface member"
