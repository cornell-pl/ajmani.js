{-# LANGUAGE KindSignatures, GADTs #-}

module SchemaChange
  (SchemaChange(..),
   apply,
   SymLens(..)
  ) where

import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromJust)
import Database.Memory
import SymLens (SymLens(..), inv, prod)
import qualified SymLens as S
import qualified SymLens.List as SL
import qualified SymLens.Map as SM
import SymLens.Database (DatabaseLens, TableLens)
import qualified SymLens.Database as DB
import qualified SymLens.Table as T

type Predicate = Fields -> Bool

-- TODO SwapColumn, RenameColumn, Join-Decompose

data SchemaChange  :: * -> * -> *  where
  SwapColumn :: Header -> Header -> SchemaChange Table Table
  InsertColumn :: Header -> Field -> SchemaChange Table Table
  DeleteColumn :: Header -> Field -> SchemaChange Table Table
  RenameColumn :: Header -> Header -> SchemaChange Table Table
  InsertTable :: Name -> Table -> SchemaChange Database Database
  DeleteTable :: Name -> SchemaChange Database Database
  RenameTable :: Name -> Name -> SchemaChange Database Database
  Join :: Name -> Name -> Name -> SchemaChange Database Database
  Decompose :: Name -> Name -> Name -> SchemaChange Database Database
  Append :: Name -> Name -> Name -> SchemaChange Database Database
  Split :: (Id -> Fields -> Bool) -> Name -> Name -> Name -> SchemaChange Database Database
  Compose :: SchemaChange a b -> SchemaChange b c -> SchemaChange a c

tableLens :: SymLens Table (Headers, Records)
tableLens = SymLens ()
                    (\(Table hs rs) () -> ((hs, rs), ()))
                    (\(hs, rs) () -> (Table hs rs, ()))

apply :: SchemaChange from to -> SymLens from to
apply (SwapColumn i1 i2) = T.swapColumn i1 i2
  
apply (InsertColumn h f) = T.insertColumn h f
  
apply (DeleteColumn h f) = T.deleteColumn h f

apply (RenameColumn h1 h2) = T.renameColumn h1 h2

apply (InsertTable n t) = DB.insert n t

apply (DeleteTable n) = DB.drop n

apply (RenameTable n1 n2) = DB.rename n1 n2

apply (Join n1 n2 n) = undefined
--   SymLens undefined
--     (\(Table hs1 rs1, Table hs2 rs2) c ->
--          let hs = intersect hs1 hs2 in
--          let hs1c = hs1 \\ hs in
--          let hs2c = hs2 \\ hs in
--          let is1 = map (\h -> fromJust $ elemIndex h hs1) hs in
--          let is1c = [0..length hs1 - 1] \\ is1 in
--          let is2 = map (\h -> fromJust $ elemIndex h hs2) hs in
--          let is2c = [0..length hs2 - 1] \\ is2 in
--          let p hs is = map (\i -> hs !! i) is in
--          let m1 = Map.foldr (\hs m -> Map.insert (p hs is1) (p hs is1c) m) Map.empty rs1 in
--          let (_,m2) = Map.foldr (\hs (i,m) ->
--                                   let k = p hs is2 in
--                                   if Map.member k m1 then
--                                     let v1 = fromJust $ Map.lookup k m1 in
--                                     (i+1, Map.insert i (v1 ++ (p hs is2c)) m)
--                                   else (i,m))
--                       (0,Map.empty) rs2 in
--          (Table (hs ++ hs1c ++ hs2c) m2, undefined))
--    (\(Table hs rs) c -> undefined)

apply (Decompose n n1 n2) =
  undefined

apply (Append n1 n2 n) = DB.append (\_ _ -> True) n1 n2 n

--  let hl = S.inv $ S.dup "Appending tables of different schemas" in
--  S.inv tableLens . (hl `S.prod` SM.appendInto) . S.transpose . (tableLens `S.prod` tableLens)

apply (Split on n n1 n2) = DB.split on n n1 n2
--  S.inv (apply Append)

apply (Compose s1 s2) = S.compose (apply s1) (apply s2)

append :: Table -> Table -> Either String Table
append = undefined

--split
split :: Predicate -> Table -> (Table, Table)
split = undefined
