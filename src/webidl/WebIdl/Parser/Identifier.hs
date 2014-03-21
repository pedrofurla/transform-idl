module WebIdl.Parser.Identifier where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex

import Control.Applicative((<$>))

import Text.Parsec.Prim
import Text.Parsec.String

identifier :: Parser Ident
identifier = Ident <$> (identTok <?> "identifier")
