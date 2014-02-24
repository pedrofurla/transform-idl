
module WebIdl.Helper where

import Data.Maybe(fromMaybe)
import Control.Applicative((<$>))

import Text.Parsec.Prim
import Text.Parsec.Error
import Text.Parsec.String


-- $setup
-- >>> import Text.Parsec.Char
-- >>> import Text.Parsec.Combinator

{- |
>>> run ((char '\\') >>> (char 't')) "\\t"
't'
>>> run ((many1 digit) >>> (many1 letter)) "123abc"
"abc"
-}
(>>>) :: Parser a -> Parser b -> Parser b
a >>> b = do
    a
    b 

justTrue :: Maybe a -> Bool
justTrue m = False `fromMaybe` (const True <$> m)

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

runWith :: (Show a, Show b) => (a -> b) -> Parser a -> String -> Either ParseError b
runWith f p input
        = case (parse p "" input) of
            Left err -> Left err
            Right a  -> Right $ f a

runFile :: Show a => Parser a -> String -> IO ()
runFile p fname = do
    input <- readFile fname
    run p input