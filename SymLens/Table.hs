
module SymLens.Table where

import SymLens
import Database.Memory
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.State

insertColumn :: Header -> Field -> SymLens Table Table
insertColumn h f = SymLens Map.empty putr putl 
  where
    putr (Table hs rs) = do
      m <- get
      let insertFn m' k fs = case Map.lookup k m of
            Just f' -> (m', f':fs)
            Nothing -> (Map.insert k f m', f:fs)
      let (m', rs') = Map.mapAccumWithKey insertFn m rs
      put m'
      return $ Table (h:hs) rs'
    putl (Table hs rs) = do
      case List.elemIndex h hs of
        Just n  -> do
          let (_, hs') = extract n hs
          let (m, rs') = Map.mapAccumWithKey (extractIntoMap n) Map.empty rs
          put m
          return $ Table hs' rs'
        Nothing -> put Map.empty >> return (Table hs rs)                                 
      where extract n l = let (l1, x:l2) = List.splitAt n l in 
              (x, l1 ++ l2)
            extractIntoMap n m i l = let (x, l') = extract n l in
              (Map.insert i x m, l')                 

deleteColumn :: Header -> Field -> SymLens Table Table
deleteColumn h f = inv $ insertColumn h f

renameColumn :: Header -> Header -> SymLens Table Table
renameColumn h h' = 
  SymLens () fn fn 
    where fn (Table hs rs) = put () >> return (Table (List.map rename hs) rs)
          rename hd | hd == h    = h'
                    | hd == h'   = h
                    | otherwise  = hd

swapColumn :: Header -> Header -> SymLens Table Table
swapColumn h1 h2 = 
  SymLens () fn fn
    where fn (Table hs rs) = do
            put ()
            case (List.elemIndex h1 hs, List.elemIndex h2 hs) of
              (Just n1, Just n2) -> return $ Table (swapElem n1 n2 hs) (Map.map (swapElem n1 n2) rs)
              _                  -> return $ Table hs rs
          swapElem n1 n2 l | n1 == n2  = l
                           | n1 >  n2  = swapElem n2 n1 l
                           | n1 <  n2  = let (l1, a:l2) = splitAt n1 l in
                                         let (l3, b:l4) = splitAt (n2 - n1 - 1) l2 in
                                         l1 ++ b:l3 ++ a:l4

--deleteColumn :: Header -> Table -> Table
--deleteColumn h t@(Table hs rs) =
--  case elemIndex h hs of
--    Just n   -> Table (delete n hs) $ Map.map (delete n) rs
--    Nothing  -> t
--  where delete n fs = take n fs ++ drop (n+1) fs
--
--projectColumn :: Header -> Table -> [Field]
--projectColumn h t@(Table hs rs) =
--  case elemIndex h hs of
--    Just n   -> map (!!n) (Map.elems rs)
--    Nothing  -> []
--

