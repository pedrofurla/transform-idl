{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}

module WebIdl.Ast where

import WebIdl.Literal

import Data.Typeable
import Data.Data

data Ident = Ident String  deriving (Show, Eq, Ord, Typeable, Data)

data ExtendedAttribute = 
    NoInterfaceObject
    | ChromeOnly -- Mozilla specific to be ignored in normal JS API
    | Constructor [FormalArg]
    | OtherEA String
      deriving (Show, Eq, Ord, Typeable, Data)

-- | Extended Attribute
data ExtendedAttributes = 
    ExtendedAttributes [ExtendedAttribute]
      deriving (Show, Eq, Ord, Typeable, Data)

type EA  = ExtendedAttribute
type EAs = ExtendedAttributes

eAtts :: [ExtendedAttribute] -> ExtendedAttributes
eAtts = ExtendedAttributes
emptyEAs :: ExtendedAttributes
emptyEAs = eAtts []

data Nullable = Nullable Bool  deriving (Show, Eq, Ord, Typeable, Data)
data Array    = Array Bool     deriving (Show, Eq, Ord, Typeable, Data)
-- data Sequence = Sequence Bool  deriving (Show, Eq, Ord, Typeable, Data)

{-| A type is the "usage of type" to introduce a identified instance. 
    eg, `DictAttribute someId someType default ext`, defines a dictionary attribute named `someId` having type `someType`. -}
data Type     = 
    Union [Type] Nullable Array
    | Sequence Type Nullable
    | Type Ident Nullable Array deriving (Show, Eq, Ord, Typeable, Data)

type Default   = Maybe Literal
data Optional  = Optional Bool deriving (Show, Eq, Ord, Typeable, Data)
data FormalArg = 
    -- TODO Notice that variadic arguments are only allowed to be in the end of the argument list
    -- so better create an ArgumentList [RegularArg] [VariadicArg]
      VariadicArg Ident Type EAs
    | RegularArg Ident Type Optional Default EAs deriving (Show, Eq, Ord, Typeable, Data)

data CallbackDef = CallbackDef Type [FormalArg] deriving (Show, Eq, Ord, Typeable, Data)
     
type InheritsFrom = Maybe Type

data ReadOnly = ReadOnly Bool deriving (Show, Eq, Ord, Typeable, Data)
data Inherit  = Inherit Bool  deriving (Show, Eq, Ord, Typeable, Data)
data IMember  =
    Const Ident Type Literal EAs 
    | Attribute Ident Type ReadOnly Inherit EAs 
    | Operation Ident Type [FormalArg] EAs -- TODO static - http://www.w3.org/TR/WebIDL/#idl-static-operations
    -- these are know as "specials" in the spec
    | Getter (Maybe Ident) Type FormalArg EAs           -- getters and setters and other
    | Setter (Maybe Ident) Type FormalArg FormalArg EAs -- specials http://www.w3.org/TR/WebIDL/#idl-indexed-properties. 
    | Deleter (Maybe Ident) Type FormalArg EAs         -- this is crazy...
    -- | Creator (Maybe Ident) Type FormalArg FormalArg  -- Creator is just like Setter in all use cases I can imagine
    | IComment String -- an synthetic construct to help keep track of some information
        deriving (Show, Eq, Ord, Typeable, Data)

data DictAttribute = DictAttribute Ident Type Default EAs deriving (Show, Eq, Ord, Typeable, Data)

data Partial = Partial Bool deriving (Show, Eq, Ord, Typeable, Data)

{-| Top level defintions. All of them are considered to be types, except for `Implements` -}
data Definition =  
    Interface Ident InheritsFrom [IMember] EAs
    | PartialInterface Ident [IMember] EAs -- http://heycam.github.io/webidl/#dfn-partial-interface
    | Callback Ident CallbackDef EAs
    | Dictionary Ident InheritsFrom [DictAttribute] EAs  
    | TypeDef Ident Type EAs 
    | Implements Ident Ident EAs 
    | Enum Ident [String] EAs 
    -- as per http://heycam.github.io/webidl/#dfn-partial-interface editor's draft there is also `callback interfaces`
      deriving (Show, Eq, Ord, Typeable, Data)

data WebIdl = WebIdl [Definition] deriving (Show, Eq, Typeable, Data)

