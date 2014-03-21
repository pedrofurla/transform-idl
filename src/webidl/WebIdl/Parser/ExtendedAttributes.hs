module WebIdl.Parser.ExtendedAttributes where

-- import WebIdl
import WebIdl.Ast
import WebIdl.Lex
import {-# SOURCE #-} WebIdl.Parser.Arguments(formalArgs)

import Control.Applicative((<$>))

import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Char


extendedAttributes :: Parser EAs
extendedAttributes = -- TODO: is skipWhites really needed here? should it be before or after `option`?
    ExtendedAttributes <$> 
      option [] (skipWhites $ inBrackets $ sepBy extendedAttribute $ charTok ',') 

extendedAttribute :: Parser EA
extendedAttribute = 
    let 
        cons = [("NoInterfaceObject", NoInterfaceObject), ("ChromeOnly", ChromeOnly)]
        tuple :: (String, EA) -> Parser ExtendedAttribute
        tuple t = const (snd t) <$> longestTok (fst t)
        someAtt = choice (tuple <$> cons)
    in
        do
            try (stringTok "Constructor")
            args <- option [] formalArgs
            return $ Constructor args
        <|> someAtt <|> 
        OtherEA <$> many (noneOf ",]")