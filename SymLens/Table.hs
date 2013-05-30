
module SymLens.Table where

import SymLens
--import Database.Memory
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.State

import Database.HDBC
import Database.HDBC.Sqlite3
import SymLens.Database

insertColumn :: Conn -> Name -> Name -> String -> String -> DatabaseLens
insertColumn cc tname colname colsql f = SymLens (Nothing :: Maybe String) putr putl 
  where
    putr c = do
      cn <- maybe (lift $ getUniqueName cc) return =<< get
      lift $ do
        runRaw cc $ "CREATE TABLE IF NOT EXISTS " ++ cn ++ " (" ++ colname ++ " " ++ colsql ++ ")"   
        runRaw c $ "ALTER TABLE " ++ tname ++ " ADD COLUMN " ++ colname 
                     ++ " " ++ colsql ++ " DEFAULT " ++ f
        s1 <- prepare cc $ "SELECT " ++ colname ++ " FROM " ++ cn ++ " WHERE rowid = ? " 
        s2 <- prepare c $ "SELECT rowid FROM " ++ tname
        s3 <- prepare c $ "UPDATE " ++ tname ++ " SET " ++ colname ++ " = ? WHERE rowid = ?"
        execute s2 []
        rs <- fetchAllRows s2
        mapM_ (updateFn s1 s3) rs
      return c
      where updateFn s1 s3 [r] = do
              execute s1 [r]
              c <- fetchRow s1
              maybe (return 0) (\[cname] -> execute s3 [cname, r]) c
    putl = undefined
--    putl c = do
--      cn <- get
--      lift $ do
--        quickQuery' cn ("SELECT rowid, " ++ colname ++ " FROM " ++ tname ++   
          
      
--    putl (Table hs rs) = do
--      case List.elemIndex h hs of
--        Just n  -> do
--          let (_, hs') = extract n hs
--          let (m, rs') = Map.mapAccumWithKey (extractIntoMap n) Map.empty rs
--          put m
--          return $ Table hs' rs'
--        Nothing -> put Map.empty >> return (Table hs rs)                                 
--      where extract n l = let (l1, x:l2) = List.splitAt n l in 
--              (x, l1 ++ l2)
--            extractIntoMap n m i l = let (x, l') = extract n l in
--              (Map.insert i x m, l')                 

--deleteColumn :: Header -> Field -> SymLens Table Table
--deleteColumn h f = inv $ insertColumn h f
--
--renameColumn :: Header -> Header -> SymLens Table Table
--renameColumn h h' = 
--  SymLens () fn fn 
--    where fn (Table hs rs) = put () >> return (Table (List.map rename hs) rs)
--          rename hd | hd == h    = h'
--                    | hd == h'   = h
--                    | otherwise  = hd
--
--projectColumn :: Header -> Table -> [Field]
--projectColumn h t@(Table hs rs) =
--  case elemIndex h hs of
--    Just n   -> map (!!n) (Map.elems rs)
--    Nothing  -> []
--
--swapColumn :: Header -> Header -> SymLens Table Table
--swapColumn h1 h2 = 
--  SymLens () fn fn
--    where fn (Table hs rs) = do
--            put ()
--            case (List.elemIndex h1 hs, List.elemIndex h2 hs) of
--              (Just n1, Just n2) -> return $ Table (swapElem n1 n2 hs) (Map.map (swapElem n1 n2) rs)
--              _                  -> return $ Table hs rs
--          swapElem n1 n2 l | n1 == n2  = l
--                           | n1 >  n2  = swapElem n2 n1 l
--                           | n1 <  n2  = let (l1, a:l2) = splitAt n1 l in
--                                         let (l3, b:l4) = splitAt (n2 - n1 - 1) l2 in
--                                         l1 ++ b:l3 ++ a:l4
--
