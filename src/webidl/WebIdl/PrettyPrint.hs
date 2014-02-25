{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- TODO temporary while not everything is handled -}

module WebIdl.PrettyPrint where

import WebIdl.Ast
import WebIdl.Lex()

import Control.Applicative((<$>))
import Data.List(intercalate)
import Data.Maybe(fromMaybe)

class PrettyPrint a where
    pprint :: a -> String

instance PrettyPrint WebIdl where
    pprint (WebIdl definitions) = 
        printMembers definitions

instance PrettyPrint Definition where
    pprint (Interface id' inherits members ext) = 
        "interface " 
        ++ pprint id' ++ pprint inherits
        ++ printMembers members
    pprint (Callback id' _ _) = 
        "callback " ++ pprint id'
    pprint (Dictionary id' inherits members ext) = 
        "dict " 
        ++ pprint id' ++ pprint inherits
        ++ printMembers members
    pprint (TypeDef id' typ _) = 
        "typdef " ++ pprint id' ++ " = " ++ pprint typ

instance PrettyPrint Ident where
    pprint (Ident id') = id'

instance PrettyPrint Type where
    pprint (Type id' (Nullable null') (Array array) (Sequence sequ)) = 
        emptyIf (not sequ) "sequence<"
        ++ pprint id'
        ++ emptyIf (not array) "[]"
        ++ emptyIf (not null') "?"
        ++ emptyIf (not sequ) ">"

instance PrettyPrint InheritsFrom where
    pprint maybeType = fromMaybe "" $ ((" : "++) . pprint) <$> maybeType

instance PrettyPrint IMember where
    pprint (Const id' typ lit ext) = 
        "const "
        ++ pprint typ ++ " "
        ++ pprint id' ++ " = "
        ++ show lit
    pprint (Attribute id' typ ro inherits ext) =
        "attribute "
        ++ pprint typ ++ " "
        ++ pprint id' 
    pprint (Operation id' typ args ext) = 
        pprint typ ++ " "
        ++ pprint id' ++ "("
        ++ intercalate ", " (pprint <$> args) ++ ")"
    pprint (Getter mid typ arg)        = error "TODO"
    pprint (Setter mid typ arg0 arg1)  = error "TODO"
    pprint (Deleter mid typ arg)       = error "TODO"
    pprint (Creator mid typ arg0 arg1) = error "TODO"

instance PrettyPrint FormalArg where
    pprint (FormalArg id' typ (Optional opt) default' ext) = 
        if opt then "optional " else ""
        ++ pprint typ ++ " "
        ++ pprint id'
        ++ pprint default'

instance PrettyPrint DictAttribute where
    pprint (DictAttribute id' typ default' ext) = 
        pprint typ ++ " "
        ++ pprint id'
        ++ pprint default'

instance PrettyPrint Default where
    pprint default' = fromMaybe "" $ (" = "++) . show <$> default'

sep :: String
sep = "\t"

sepped :: String -> String
sepped = ("\t"++) . ((\x -> if x=='\n' then "\n\t" else [x]) =<<) 

emptyIf :: Bool -> String -> String
emptyIf b s = if b then "" else s

printMembers :: PrettyPrint m => [m] -> String
printMembers members = 
    (if null members then "" else "\n") 
    ++ intercalate "\n" (sepped . pprint <$> members)