{-# LANGUAGE KindSignatures, GADTs #-}

module SchemaChange 
  (SchemaChange(..),
   apply,
   insertColumn,
   deleteColumn,
   SymLens(..)
  ) where

import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromJust)
import Database
import SymLens (SymLens(..), inv, prod)
import qualified SymLens as S
import qualified SymLens.List as SL 
import qualified SymLens.Map as SM

type Predicate = Fields -> Bool

data SchemaChange  :: * -> * -> *  where
  InsertColumn :: Header -> Field -> SchemaChange Table Table
  DeleteColumn :: Header -> Field -> SchemaChange Table Table
  Join :: SchemaChange (Table, Table) Table 
  Decompose :: SchemaChange Table (Table, Table)
  Append :: SchemaChange (Table, Table) Table
  Split :: SchemaChange Table (Table, Table)
  Compose :: SchemaChange a b -> SchemaChange b c -> SchemaChange a c

insertColumn :: Header -> Field -> Table -> Table
insertColumn h f t = 
  let Table hs rs = deleteColumn h t in
  Table (h : hs) (Map.map (f:) rs)
  
deleteColumn :: Header -> Table -> Table
deleteColumn h t@(Table hs rs) = 
  case elemIndex h hs of
    Just n   -> Table (delete n hs) $ Map.map (delete n) rs 
    Nothing  -> t
  where delete n fs = take n fs ++ drop (n+1) fs

projectColumn :: Header -> Table -> [Field]
projectColumn h t@(Table hs rs) = 
  case elemIndex h hs of
    Just n   -> map (!!n) (Map.elems rs) 
    Nothing  -> []
    
tableLens :: SymLens Table (Headers, Records)
tableLens = SymLens ()
                    (\(Table hs rs) () -> ((hs, rs), ()))
                    (\(hs, rs) () -> (Table hs rs, ()))    
  
apply :: SchemaChange from to -> SymLens from to
apply (InsertColumn h f) =
  S.inv tableLens . (SL.cons h `S.prod` SM.fmmap (SL.cons f)) . tableLens
apply (DeleteColumn h f) = 
  S.inv (apply (InsertColumn h f))
apply Join = 
  SymLens undefined
    (\(Table hs1 rs1, Table hs2 rs2) c -> 
         let hs = intersect hs1 hs2 in 
         let hs1c = hs1 \\ hs in
         let hs2c = hs2 \\ hs in 
         let is1 = map (\h -> fromJust $ elemIndex h hs1) hs in
         let is1c = [0..length hs1 - 1] \\ is1 in
         let is2 = map (\h -> fromJust $ elemIndex h hs2) hs in
         let is2c = [0..length hs2 - 1] \\ is2 in
         let p hs is = map (\i -> hs !! i) is in
         let m1 = Map.foldr (\hs m -> Map.insert (p hs is1) (p hs is1c) m) Map.empty rs1 in
         let (_,m2) = Map.foldr (\hs (i,m) ->
                                  let k = p hs is2 in
                                  if Map.member k m1 then
                                    let v1 = fromJust $ Map.lookup k m1 in
                                    (i+1, Map.insert i (v1 ++ (p hs is2c)) m)
                                  else (i,m))
                      (0,Map.empty) rs2 in
         (Table (hs ++ hs1c ++ hs2c) m2, undefined))
   (\(Table hs rs) c -> undefined)                                
         
apply Decompose = 
  undefined
  
apply Append = 
  let hl = S.inv $ S.dup "Appending tables of different schemas" in
  S.inv tableLens . (hl `S.prod` SM.appendInto) . S.transpose . (tableLens `S.prod` tableLens)
                                 
apply Split = 
  S.inv (apply Append)             
                                  
apply (Compose s1 s2) = S.compose (apply s1) (apply s2)                                  

append :: Table -> Table -> Either String Table
append = undefined  

--split
split :: Predicate -> Table -> (Table, Table)
split = undefined
