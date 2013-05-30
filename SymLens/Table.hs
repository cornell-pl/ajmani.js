
module SymLens.Table where

import SymLens
--import Database.Memory
import Data.Maybe (fromMaybe)
import Data.List
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
        commit cc
      return' c
      where updateFn s1 s3 [r] = do
              execute s1 [r]
              c <- fetchRow s1
              maybe (return 0) (\[cname] -> execute s3 [cname, r]) c
    putl c = do
      cn <- maybe (lift $ getUniqueName cc) return =<< get
      lift $ do
        temp <- getUniqueName c
        (Table _ (sql, _) _) <- readTableStructure c tname
        let (col, rest) = removeColumnName (getColumns sql) colname
        let (col', rest') = (head $ words col, map (head . words) rest)
        runRaw c $ makeCreateStatement temp rest
        runRaw c $ "INSERT INTO " ++ temp ++ "(" ++ intercalate "," ("rowid":rest') ++ ")" 
                     ++ " SELECT " ++ intercalate "," ("rowid":rest') ++ " FROM " ++ tname
        runRaw cc $ makeCreateExistsStatement cn [col]
        runRaw cc $ "DELETE FROM " ++ cn
        query <- prepare c $ "SELECT rowid, " ++ col' ++ " FROM " ++ tname
        ins <- prepare cc $ "INSERT INTO " ++ cn ++ "(rowid, " ++ col' ++ ") VALUES (?, ?)"
        execute query []
        rs <- fetchAllRows query
        mapM_ (execute ins) rs
        commit cc
        dropTable c tname
        renameTable c temp tname
      put (Just cn)
      return' c
        
                
deleteColumn :: Conn -> Name -> Name -> String -> String -> DatabaseLens
deleteColumn cc tname colname colsql f = inv $ insertColumn cc tname colname colsql f

-- Table should not already have a column with the target name

renameColumn :: Conn -> Name -> Name -> Name -> DatabaseLens
renameColumn _ tname from to = 
  SymLens () (fn from to) (fn to from) 
    where fn from to c = do
            lift $ do
              (Table _ (sql, _) _) <- readTableStructure c tname
              let (col1, rest1) = removeColumnName (getColumns sql) from
                  rest          = map (head . words) rest1
                  newcol        = to ++ " " ++ (concat $ tail $ words col1)
              temp <- getUniqueName c
              runRaw c $ makeCreateStatement temp (newcol:rest1)
              runRaw c $ "INSERT INTO " ++ temp ++ "(" ++ intercalate "," ("rowid":to:rest) ++ ")" ++
                           " SELECT " ++ intercalate "," ("rowid":from:rest) ++ " FROM " ++ tname
              dropTable c tname
              renameTable c temp tname
            return' c                
