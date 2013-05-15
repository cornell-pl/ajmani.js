{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module SymLens.Database where

import SymLens 
import Database 
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

liftTables :: Map.Map Name (SymLens [Table] [Table]) -> SymLens Database Database
liftTables = undefined

liftTable :: Map.Map Name (SymLens Table Table) -> SymLens Database Database   
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
          
  