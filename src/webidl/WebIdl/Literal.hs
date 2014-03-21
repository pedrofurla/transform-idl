{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}

module WebIdl.Literal where

import Data.Typeable
import Data.Data

data Literal = 
      Number String
    | Hex String
    | Str String 
    | Null
    | Boolean String deriving (Show, Eq, Ord, Typeable, Data)