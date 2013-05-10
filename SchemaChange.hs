{-# LANGUAGE KindSignatures, GADTs #-}

module SchemaChange 
  (SchemaChange(..),
   apply,
   insertColumn,
   deleteColumn,
   SymLens(..)
  ) where

import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromJust)
import Database
import SymLens

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
  
apply :: SchemaChange from to -> SymLens from to
apply (InsertColumn h f) =
  SymLens (repeat f)
          (\(Table hs rs) fs -> (Table (h:hs) $ Map.fromList $ 
                                  map (\(a,(k,v)) -> (k,a:v)) $ zip (fs ++ repeat f) (Map.toList rs), fs ++ repeat f))
          (\t _ -> (deleteColumn h t, projectColumn h t))
apply (DeleteColumn h f) =
  SymLens (repeat f)
          (\t _ -> (deleteColumn h t, projectColumn h t))
          (\(Table hs rs) fs -> (Table (h:hs) $ Map.fromList $ 
                                  map (\(a,(k,v)) -> (k,a:v)) $ zip (fs ++ repeat f) (Map.toList rs), fs ++ repeat f))
apply Append = 
  SymLens Map.empty 
          (\(Table hs1 rs1, Table hs2 rs2) _ ->
              if hs1 /= hs2 then error "Appending tables of different schemas"
              else foldl appendRekey (Table hs1 rs1, Map.empty) (Map.toList rs2))
          (\(Table hs rs) cm -> ((Table hs $ Map.difference rs cm, 
                                  Table hs $ Map.mapKeys (fromJust . flip Map.lookup cm) 
                                           $ Map.intersection rs cm),
                                 cm))      
  where appendRekey (t, cm) (k, fs) = let (i, t') = createRecord fs t in (t', Map.insert i k cm)                                 
apply Split = 
  inv (apply Append)             
                                  
apply (Compose s1 s2) = compose (apply s1) (apply s2)                                  

append :: Table -> Table -> Either String Table
append = undefined  

--split
split :: Predicate -> Table -> (Table, Table)
split = undefined