import SymLens
import SymLens.Database
import SymLens.Table
import Prelude hiding (drop)

import Control.Monad
import Control.Monad.State
import Database.HDBC
import Database.HDBC.Sqlite3

import Test.HUnit
import Test.Framework.Providers.HUnit

testDB :: Conn -> IO ()
testDB c = do
  runRaw c "CREATE TABLE emails (header varchar(20), body varchar(100))"
  runRaw c "CREATE TABLE moreEmails (header varchar(20), body varchar(100))"
  i1 <- prepare c "INSERT INTO emails VALUES (?,?)"
  i2 <- prepare c "INSERT INTO moreEmails VALUES (?,?)"
  let r1 = map (map toSql) [["From:Nate","Turn The Crank"],["From:Raghu","Hi"],["From:Satvik","Hey"]]
  let r2 = map (map toSql) [["From:Arjun","What ??"],["From:Nate","Its so sketchy"]]
  mapM_ (execute i1) r1
  mapM_ (execute i2) r2
  commit c

getTable :: Conn -> String -> IO [[SqlValue]]
getTable c n = quickQuery' c ("select rowid," ++ n ++ ".* from " ++ n) []
testRename :: Conn -> IO ()
testRename c = do
  e <- getTable c "emails"
  me <- getTable c "moreEmails"
  case rename "emails" "moreEmails" of
    SymLens comp pr pl -> do
      c' <- execStateT (pr c) comp
      e1 <- getTable c "emails"
      me1 <- getTable c "moreEmails"
      print $ e == me1
      print $ e1 == me
      execStateT (pl c) c'
      e2 <- getTable c "emails"
      me2 <- getTable c "moreEmails"
      print $ e == e2
      print $ me2 == me
    

testDrop :: Conn -> IO ()
testDrop c = do
  e <- getTable c "emails"
  case drop "emails" of
    SymLens comp pr pl -> do
      c' <- execStateT (pr c) comp
      b <- hasTable c "emails"
      print $ not b
      execStateT (pl c) c'
      e1 <- getTable c "emails"
      print $ e == e1

testInsert :: Conn -> IO ()
testInsert c = do
  e <- getTable c "emails"
  t1 <- readTable c "emails"
  case insert "otherEmails" t1 of
    SymLens comp pr pl -> do
      c' <- execStateT (pr c) comp
      b <- hasTable c "otherEmails"
      print b
      e1 <- getTable c "otherEmails"
      print $ e == e1
      execStateT (pl c) c'
      b <- hasTable c "otherEmails"
      print $ not b 

testAppend :: Conn -> IO ()
testAppend c = do
  e <- getTable c "emails"
  me <- getTable c "moreEmails"
  case append (const True) "emails" "moreEmails" "allEmails" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< quickQuery' c "select * from allEmails" []
      print =<< quickQuery' c ("select * from sqlite_master")  []
      comp'' <- execStateT (pl c) comp'
      print =<< quickQuery' c ("select * from sqlite_master")  []
      print =<< quickQuery' c ("select * from emails")  []
      print =<< quickQuery' c ("select * from moreEmails")  []
      
testInsertColumn :: Conn -> IO ()
testInsertColumn c = do
  e <- getTable c "emails"
  case insertColumn c  

main :: IO ()
main = do
  c <- connectSqlite3 ":memory:"
  testDB c
  --testRename c
  --testDrop c
  --testInsert c
  testAppend c
  

