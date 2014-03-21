{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}

module WebIdl.Process where 

import WebIdl.Ast
import WebIdl.Query
import qualified WebIdl.Lex as L

import Data.Maybe(fromMaybe)
import Data.Char(toLower)
import Data.List(isPrefixOf)
import qualified Data.Set           as S
import Control.Applicative((<$>), liftA2)

import qualified Data.Map.Lazy as M

type Global = (M.Map String Definition)

-- queryDef ( concat . toList . fromList . ((\s -> grep s ) <$>) <$> looseTypes)

looseTypes :: Global -> [String]
looseTypes ds =
    let
        notDefed = flip M.notMember ds `filter` typeInsts ds
    in 
        S.toList $ S.fromList $ flip notElem L.preDefinedTypes `filter` notDefed

{-| The original should always be the interface on the right. TODO this "rule" doesn't look good -}
mergeInterfaces :: Definition -> Definition -> Definition
mergeInterfaces (Interface (Ident pname) _ pms _) (Interface i inh ms atts) = 
    Interface i inh (
        concat [
            [IComment $ "merged partial interface "++pname],
            ms,
            [IComment $ "from partial interface "++pname],  
            pms, 
            [IComment $ "end partial interface "++pname]]
    ) atts
mergeInterfaces i@(Interface{}) (PartialInterface p@_ pms@_ atts@_) = mergeInterfaces (Interface p Nothing pms atts) i
mergeInterfaces (PartialInterface p@_ pms@_ atts@_) i@(Interface{}) = mergeInterfaces (Interface p Nothing pms atts) i
mergeInterfaces a b = error $ "Can't merge a non interfaces: `" ++ getName a ++ "` and `" ++ getName b ++ "` "

{- 
-- 
    then M.alter (Just . maybe iface (mergeInterfaces iface)) i map' -- TODO one of the two interfaces gotta be a partial
    else M.insert i iface map') `mergePartials` ds
    (\if2 -> M.adjust (mergeInterfaces if2) i0 map')
-}

processPartial :: Definition -> Global -> Global
processPartial p@(PartialInterface (Ident i) _ _) m =  
    M.alter (
        Just . maybe 
            (error $ "Partial interface without an interface "++i)
            (mergeInterfaces p)
        ) i m
processPartial d _ = error $ "It got be a interface or a partial interface " ++ getName d-- Nothing        

processImplements :: (String -> Bool) -> Definition -> Global -> Global
processImplements ignores (Implements (Ident l) (Ident r) _) m = 
    let
        err = error . (++) "Missing interface in implements declaration: " 
        ri = fromMaybe (err r) $ M.lookup r m
     in
         if (not $ ignores r) then
             M.alter (Just . mergeInterfaces ri . fromMaybe (err l)) l m
         else
             m
processImplements _ _ _ = error "Can't process an implements if it's not a implements"         

processInterface :: Definition -> Global -> Global
processInterface d =
    M.alter ( 
        Just . maybe 
            d -- in case it's not already in the map
            (\i ->
                if isEmptyIface d || isEmptyIface i -- if it already exists and any is empty,
                    then i                            --  it's ok
                    else error $ "Redefined interface " ++ getName d -- the case it's already in the map TODO - yeah, I know. to be a Validation
            ) 
        ) (getName d)

processOther :: Definition -> Global -> Global
processOther d =
    M.alter ( 
        Just . maybe 
            d -- in case it's not already in the map
            (error . (++) "Redefined type" . getName)  -- the case it's already in the map TODO - yeah, I know. to be a Validation
        ) (getName d)

process :: [Definition] -> Global
process defs = 
    let 
        ignores = isPrefixOf "moz" . (toLower <$>)
        ignoresD :: (Definition -> Bool) -> Definition -> Bool
        ignoresD = liftA2 (&&) (not . ignores . getName)

        interfaces = filter (ignoresD isInterface)  defs
        --TODO  invariant: M.size map1 == length interfaces
        map1 = foldr processInterface M.empty interfaces
        
        partials = filter (ignoresD isPartial) defs
        map2 = foldr processPartial map1 partials

        implements = filter (ignoresD isImplements) defs
        map3 = foldr (processImplements ignores) map2 implements

        isOther :: Definition -> Bool
        isOther = not . or . sequence [isImplements, isInterface, isPartial, ignores . getName]
        others = filter isOther defs
        map4 = foldr processOther map3 others
    in  
        map4
        --map1
        --error "TODO"