{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module SymLens.Database where

import SymLens 
import Database 
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Tuple as T

type TableLens = SymLens Table Table
type DatabaseLens = SymLens Database Database

liftTable :: Map.Map Name TableLens -> DatabaseLens
liftTable m = 
  SymLens m
          (\md mc -> Map.foldlWithKey (\(md', mc') n t -> 
                       case Map.lookup n mc of
                         Just (SymLens c pr pl) -> let (t', c') = pr t c in
                                                       (Map.insert n t' md', 
                                                        Map.insert n (SymLens c' pr pl) mc')
                         Nothing -> (Map.insert n t md', mc')) 
                     (Map.empty, Map.empty) md)
          (\md mc -> Map.foldlWithKey (\(md', mc') n t -> 
                       case Map.lookup n mc of
                         Just (SymLens c pr pl) -> let (t', c') = pl t c in
                                                       (Map.insert n t' md', 
                                                        Map.insert n (SymLens c' pr pl) mc')
                         Nothing -> (Map.insert n t md', mc')) (Map.empty, Map.empty) md)
          

liftTableLens :: Name -> TableLens -> DatabaseLens
liftTableLens n (SymLens s pr pl) = SymLens s (doit pr) (doit pl)
  where doit put d c = T.swap $ Map.mapAccumWithKey onT c d
          where onT c n' t | n == n'   = T.swap $ put t c 
                           | otherwise = (c,t) 
         
rename :: Name -> Name -> DatabaseLens
rename n1 n2 = SymLens () put put
  where sigma n | n == n1   = n2
                | n == n2   = n1
                | otherwise = n
        put d () = (Map.mapKeys sigma d, ())

drop :: Name -> DatabaseLens
drop n = SymLens Nothing pr pl
  where pr d c = case Map.lookup n d of
          Just t  -> (Map.delete n d, Just t)
          Nothing -> (d,c)
        pl d Nothing    = (d, Nothing)
        pl d c@(Just v) = (Map.insert n v d,c) 

--  Can this be implemented using drop ??
insert :: Name -> Table -> DatabaseLens
insert n t = SymLens () pr pl
  where pr d c = (Map.insert n t d, c)
        pl d c = (Map.delete n d  , c) 

