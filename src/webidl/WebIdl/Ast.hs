module WebIdl.Ast where

import WebIdl.Lex

data Ident = Ident String  deriving (Show, Eq)

data ExtendedAttributes = 
    NoInterfaceObject
    | OtherEA String
      deriving (Show, Eq, Read)

-- | Extended Attribute
data ExtendedAtt = 
    ExtendedAtt [ExtendedAttributes]
      deriving (Show, Eq)

data Nullable = Nullable Bool  deriving (Show, Eq)
data Array    = Array Bool     deriving (Show, Eq)
data Sequence = Sequence Bool  deriving (Show, Eq)
data Type     = Type Ident Nullable Array Sequence deriving (Show, Eq)

type Default   = Maybe Literal
data Optional  = Optional Bool deriving (Show, Eq)
data FormalArg = 
      VariadicArg Ident Type ExtendedAtt
    | RegularArg Ident Type Optional Default ExtendedAtt deriving (Show, Eq)

--data Callback     = Callback Ident CallbackDef ExtendedAtt deriving (Show, Eq)
data CallbackDef = CallbackDef Type [FormalArg] deriving (Show, Eq)

--data TypeDef = TypeDef Ident Type ExtendedAtt deriving (Show, Eq)
     
type InheritsFrom = Maybe Type

--data Interface = Interface Ident InheritsFrom [IMember] ExtendedAtt deriving (Show, Eq)

data ReadOnly = ReadOnly Bool deriving (Show, Eq)
data Inherit  = Inherit Bool  deriving (Show, Eq)
data IMember  =
    Const Ident Type Literal ExtendedAtt 
    | Attribute Ident Type ReadOnly Inherit ExtendedAtt 
    | Operation Ident Type [FormalArg] ExtendedAtt -- TODO static - http://www.w3.org/TR/WebIDL/#idl-static-operations
    -- these are know as "specials"
    | Getter (Maybe Ident) Type FormalArg           -- getters and setters and other
    | Setter (Maybe Ident) Type FormalArg FormalArg -- specials http://www.w3.org/TR/WebIDL/#idl-indexed-properties. 
    | Deleter (Maybe Ident) Type FormalArg          -- this is crazy...
    | Creator (Maybe Ident) Type FormalArg FormalArg  
        deriving (Show, Eq)

--data Dictionary = Dictionary Ident InheritsFrom [DictAttribute] ExtendedAtt deriving (Show, Eq)
data DictAttribute = DictAttribute Ident Type Default ExtendedAtt deriving (Show, Eq)

-- data TopLevel = Callback | Interface | Const | TypeDef | Dictionary

--data WebIdl = WebIdl [Either Interface Callback] deriving (Show,Eq)

data Definition =  
    Interface Ident InheritsFrom [IMember] ExtendedAtt
    | Callback Ident CallbackDef ExtendedAtt
    | Dictionary Ident InheritsFrom [DictAttribute] ExtendedAtt  
    | TypeDef Ident Type ExtendedAtt 
    | Implements Ident Ident ExtendedAtt 
    | Enum Ident [String] ExtendedAtt 
      deriving (Show,Eq)

data Definitions = Definitions [Definition]

data WebIdl = WebIdl [Definition] deriving (Show,Eq)

