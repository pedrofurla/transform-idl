module WebIdl.Parser.Arguments where

import WebIdl.Ast
import Text.Parsec.String

formalArgs :: Parser [FormalArg]