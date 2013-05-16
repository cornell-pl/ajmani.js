{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module SymLens.Database where

import SymLens 
import Database 
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple

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
liftTableLens n (SymLens s pr pl) = 
  SymLens s (doit pr) (doit pl)
  where doit put d c = 
            Data.Tuple.swap $ 
                Map.mapAccumWithKey 
                       (\c n' t -> 
                            if n == n' then Data.Tuple.swap $ put t c 
                            else (c,t)) 
                       c d
         
rename :: Name -> Name -> DatabaseLens
rename n1 n2 = 
  SymLens () put put
  where sigma n = if n == n1 then n2 else if n == n2 then n1 else n
        put d () = (Map.mapKeys sigma d, ())

