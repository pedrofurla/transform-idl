{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module WebIdl.Query where 

import WebIdl.Helper
import WebIdl.PrettyPrint
import WebIdl.Ast
import WebIdl.Parser.WebIdl

import Control.Applicative -- hiding (Const)
import Data.Maybe(maybeToList)
import Data.Generics
import Text.Parsec.Prim

-- type GenericT = forall a. Data a => a -> a
-- type GenericQ r = forall a. Data a => a -> r    
-- type GenericM m = forall a. Data a => a -> m a

-- mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
-- mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
-- mkM :: (Monad m, Typeable a, Typeable b) => (b -> m b) -> a -> m a

-- extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q

-- gmapT :: (forall b. Data b => b -> b) -> a -> a
-- gmapQ :: (forall d. Data d => d -> u) -> a -> [u]
-- gmapM :: forall m. Monad m => (forall d. Data d => d -> m d) -> a -> m a

-- everywhere :: GenericQ -> GenericQ 
-- everywhere :: (forall a. Data a => a -> a) -> forall a. Data a => a -> a
-- everywhereM :: Monad m => GenericM m -> GenericM m
-- everything :: (r -> r -> r) -> GenericQ r -> GenericQ r
   -- everything :: (r -> r -> r) -> (a -> r) -> (b -> r)
-- everythingBut :: (r -> r -> r) -> GenericQ (r, Bool) -> GenericQ r

-- listify :: Typeable r => (r -> Bool) -> GenericQ [r]

-- queryDef (intercalate "\n" . (show <$>) . M.toList . mergePartials M.empty)
-- gquery (\ds -> mergePartials M.empty $  listInterfaces filterNonPartial ds ++ listInterfaces filterPartial ds )  

gquery :: String -> (forall a. Data a => a -> [Definition])  -> IO ()
gquery filename t = cppFile filename  <$>> parse webIdl "" <$>> (pprintShallow . t <$>) >>= either print putStrLn 

--queryDef :: ([Definition] -> [Definition]) -> IO ()
--queryDef t = cppFile mozWindow  <$>> parse webIdl "" <$>> ((\(WebIdl ds) -> (pprintShallow . t) ds) <$>) >>= either (putStrLn . show) putStrLn 

queryDef :: [String] -> ([Definition] -> String) -> IO ()
queryDef idls t = cppFiles idls <$>> parse webIdl "" <$>> ((\(WebIdl ds) -> t ds) <$>) >>= either print putStrLn

getName2 :: (Data a,Typeable a) => a -> [String]
getName2 = concat . (maybeToList <$>) . gmapQ (Nothing `mkQ` (\(Ident i) -> Just i))

getName :: (Typeable a, Data a) => a -> String -- this head there is smelling bad... TODO I want a shallow listify
getName a = listify (isA :: IsA Char) $ head $ listify (isA :: IsA Ident) a

typeInsts :: (Typeable a, Data a) => a -> [String]
typeInsts = 
    (getName <$>) . listify (isA :: IsA Type)
    -- (concat . getName2 <$>) . listify (isA :: IsA Type)

typeNameG :: Typeable a => a -> Maybe (String, Type)
typeNameG = Nothing `mkQ` typeName

typeName :: Type -> Maybe (String, Type)
-- todo could use a generic way to extract name
typeName t@(Type (Ident name) _ _ ) = Just (name, t)
typeName _ = Nothing

isInterface :: Definition -> Bool
isInterface (Interface{}) = True
isInterface _ = False

isPartial :: Definition -> Bool
isPartial (PartialInterface{}) = True
isPartial _ = False

isImplements :: Definition -> Bool
isImplements (Implements{}) = True
isImplements _ = False

isInterfaceG :: (Typeable a, Data a) => a -> Bool
isInterfaceG = False `mkQ` isInterface

isPartialG :: (Typeable a, Data a) => a -> Bool
isPartialG = False `mkQ` isPartial

isImplementsG :: (Typeable a, Data a) => a -> Bool
isImplementsG = False `mkQ` isImplements

isEmptyIface :: Definition -> Bool
isEmptyIface (Interface _ _ [] _ _) = True
isEmptyIface _ = False

type IsA a = Typeable a => a -> Bool

isTyp :: Type -> Bool
isTyp _ = True

isA :: (Typeable a, Data a) => a -> Bool
isA _ = True

isNotA :: (Typeable a, Data a) => a -> Bool
isNotA _ = False

{-
tmp :: [Definition]
tmp = []

tmp2 :: [Definition] -> [Ident]
--tmp2 ds = listify isIden ds
tmp2 ds = listify (isA :: Ident -> Bool) ds

sample :: [IMember]
sample = [
    Operation (Ident "getCurrentPosition") (Type (Ident "void") (Nullable False) (Array False)) [RegularArg (Ident "successCallback") (Type (Ident "PositionCallback") (Nullable False) (Array False)) (Optional False) Nothing (ExtendedAttributes []),RegularArg (Ident "errorCallback") (Type (Ident "PositionErrorCallback") (Nullable False) (Array False)) (Optional True) Nothing (ExtendedAttributes []),RegularArg (Ident "options") (Type (Ident "PositionOptions") (Nullable False) (Array False)) (Optional True) Nothing (ExtendedAttributes [])] (ExtendedAttributes []),
    Operation (Ident "XXX") (Type (Ident "TTT") (Nullable False) (Array False)) [RegularArg (Ident "AAA") (Type (Ident "AAA") (Nullable False) (Array False)) (Optional False) Nothing (ExtendedAttributes []),RegularArg (Ident "BBB") (Type (Ident "BBB") (Nullable False) (Array False)) (Optional True) Nothing (ExtendedAttributes []),RegularArg (Ident "options") (Type (Ident "PositionOptions") (Nullable False) (Array False)) (Optional True) Nothing (ExtendedAttributes [])] (ExtendedAttributes [])]
-}

-- (runWith id webIdl <$> (readFile "idl/geolocation.idl")) <$>> (\(Right (WebIdl ds)) -> ds) <$>> ((++"\n") . show . typeInsts <$>) >>= putStr . concat
-- readFile "idl/geolocation.idl" <$>> parse webIdl "" <$>> (pprint <$>) >>= \(Right j) -> return j
-- putStrLn =<< (\(Right (WebIdl ds)) -> pprint $ filter (filterId "PositionCallback") ds) <$> (runWith id webIdl <$> (readFile "idl/geolocation.idl"))
-- putStrLn =<< (\(Right (WebIdl ds)) -> pprint $ tmp2 ds) <$> (runWith id webIdl <$> (readFile "idl/geolocation.idl"))
-- (\(Right (WebIdl ds)) -> filter (withId "PositionCallback") ds) <$> (runWith id webIdl <$> (readFile "idl/geolocation.idl"))
