{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- TODO temporary while not everything is handled -}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module WebIdl.PrettyPrint where

import WebIdl.Ast
import WebIdl.Lex()
import WebIdl.Literal

import Control.Applicative((<$>))
import Data.List(intercalate)
import Data.Maybe(fromMaybe)

class PrettyPrint a where
    pprint :: a -> String
    pprintShallow :: a -> String
    pprintShallow = pprint 

instance PrettyPrint WebIdl where
    pprint (WebIdl definitions) = 
        printMembers definitions
    pprintShallow _ = ""

instance PrettyPrint Definition where
    pprint (Interface id' inherits members ext) = 
        "interface " 
        ++ pprint id' 
        ++ fromMaybe "" (((" : "++) . pprint) <$> inherits)
        ++ printMembers members
    pprint (PartialInterface id' members ext) = 
        "partial interface " 
        ++ pprint id'
        ++ printMembers members
    pprint (Callback id' _ _) = 
        "callback " ++ pprint id'
    pprint (Dictionary id' inherits members ext) = 
        "dict " 
        ++ pprint id' ++ pprint inherits
        ++ printMembers members
    pprint (TypeDef id' typ _) = 
        "typdef " ++ pprint id' ++ " = " ++ pprint typ
    pprint (Implements id0 id1 ext) = 
        pprint id0 ++ " implements " ++ pprint id1
    pprint (Enum id' members ext) =
        pprint id' ++ " : " ++ intercalate "," (show <$> members)

    pprintShallow (Interface id' inherits _ ext) =
        pprint (Interface id' inherits [] ext)
    pprintShallow (PartialInterface id' _ ext) =
        pprint (PartialInterface id' [] ext)
    pprintShallow (Dictionary id' inherits members ext) = 
        pprint (Dictionary id' inherits [] ext) 
    pprintShallow x = pprint x

instance PrettyPrint Ident where
    pprint (Ident id') = id'

instance PrettyPrint Type where
    pprint (Type id' (Nullable null') (Array array)) = 
        --emptyIf (not sequ) "sequence<"
        pprint id'
        ++ emptyIf (not array) "[]"
        ++ emptyIf (not null') "?"
        ++ " "
        -- ++ emptyIf (not sequ) ">"
    pprint (Union ts (Nullable nul) (Array array)) =
        "("
        ++ intercalate " or " (pprint <$> ts)
        ++ ")"
        ++ emptyIf (not array) "[]"
        ++ emptyIf (not nul) "?"
        ++ " "
    pprint (Sequence t (Nullable nul)) =
        "sequence <" 
        ++ pprint t 
        ++ ">"
        ++ emptyIf (not nul) "?"
        ++ " "

-- instance PrettyPrint InheritsFrom where
--    pprint maybeType = fromMaybe "" $ ((" : "++) . pprint) <$> maybeType

instance PrettyPrint IMember where
    pprint (Const id' typ lit ext) = 
        "const "
        ++ pprint typ
        ++ pprint id' ++ "= "
        ++ show lit
    pprint (Attribute id' typ ro inherits ext) =
        "attribute "
        ++ pprint typ
        ++ pprint id' 
    pprint (Operation id' typ args ext) = 
        pprint typ
        ++ pprint id' ++ "("
        ++ intercalate ", " (pprint <$> args) ++ ")"
    pprint (Getter mid typ arg _)        = 
        "getter "
        ++ pprint mid
        ++ pprint arg
    pprint (Setter mid typ arg0 arg1 _)  = 
        "setter "
        ++ pprint mid
        ++ pprint arg0 ++ pprint arg1
    pprint (Deleter mid typ arg _)       = 
        "deleter "
        ++ pprint mid
        ++ pprint arg
    {-pprint (Creator mid typ arg0 arg1) = 
        "creator "
        ++ (maybe "" pprint mid) ++ " "
        ++ pprint arg0 ++ pprint arg1-}
    pprint (IComment str) = "--" ++ str


instance PrettyPrint FormalArg where
    pprint (RegularArg id' typ (Optional opt) default' ext) = 
        (if opt then "optional " else "")
        ++ pprint typ 
        ++ pprint id'
        ++ pprintDefault default'
    pprint (VariadicArg id' typ ext) = 
        pprint typ ++ "... " 
        ++ pprint id'

instance PrettyPrint DictAttribute where
    pprint (DictAttribute id' typ default' ext) = 
        pprint typ 
        ++ pprint id'
        ++ pprintDefault default'

instance PrettyPrint Default where
    pprint default' = fromMaybe "" $ ("= "++) . show <$> default'

instance PrettyPrint b => PrettyPrint [b] where
    -- pprint :: a -> String
    pprint = printMembers -- concat (pprint <$> a)
    pprintShallow  = intercalate "\n" . map pprintShallow 

instance PrettyPrint b => PrettyPrint (Maybe b) where
    pprint = maybe "" pprint

pprintDefault :: Maybe Literal -> String
pprintDefault  = fromMaybe "" . (("= "++) . show <$>) -- default'

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

