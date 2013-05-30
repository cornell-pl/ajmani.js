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

testRename :: Conn -> Conn -> IO ()
testRename c compConn = do
  e <- getTable c "emails"
  me <- getTable c "moreEmails"
  case rename compConn "emails" "moreEmails" of
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
    

testDrop :: Conn -> Conn -> IO ()
testDrop c compConn = do
  e <- getTable c "emails"
  case drop compConn "emails" of
    SymLens comp pr pl -> do
      c' <- execStateT (pr c) comp
      b <- hasTable c "emails"
      print $ not b
      execStateT (pl c) c'
      e1 <- getTable c "emails"
      print $ e == e1

testInsert :: Conn -> Conn -> IO ()
testInsert c compConn = do
  e <- getTable c "emails"
  t1 <- readTable c "emails"
  case insert compConn "otherEmails" t1 of
    SymLens comp pr pl -> do
      c' <- execStateT (pr c) comp
      b <- hasTable c "otherEmails"
      print b
      e1 <- getTable c "otherEmails"
      print $ e == e1
      execStateT (pl c) c'
      b <- hasTable c "otherEmails"
      print $ not b 

testAppend :: Conn -> Conn -> IO ()
testAppend c compConn = do
  e <- getTable c "emails"
  me <- getTable c "moreEmails"
  case append compConn (const True) "emails" "moreEmails" "allEmails" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< quickQuery' c "select * from allEmails" []
      b <- hasTable c "emails"
      print $ not b
      b <- hasTable c "moreEmails"
      print $ not b
      comp'' <- execStateT (pl c) comp'
      b <- hasTable c "allEmails"
      print $ not b      
      print =<< quickQuery' c ("select * from emails")  []
      print =<< quickQuery' c ("select * from moreEmails")  []
      
testInsertColumn :: Conn -> Conn -> IO ()
testInsertColumn c compConn = do
  e <- getTable c "emails"
  case insertColumn compConn "emails" "timestamp" "VARCHAR(20)" "'20 May 2013'" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< quickQuery' c "select rowid, * from emails" []
      comp'' <- execStateT (pl c) comp'
      print =<< quickQuery' c "select rowid, * from emails" []

testDeleteColumn :: Conn -> Conn -> IO ()
testDeleteColumn c compConn = do
  e <- getTable c "emails"
  case deleteColumn compConn "emails" "header" "VARCHAR(20)" "'From:Haskell'" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< quickQuery' c "select rowid, * from emails" []
      --comp'' <- execStateT (pl c) comp'
      print =<< quickQuery' c "select rowid, * from emails" []

testRenameColumn :: Conn -> Conn -> IO ()
testRenameColumn c compConn = do
  e <- getTable c "emails"
  case renameColumn compConn "emails" "header" "from" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< readTableStructure c "emails"
      print =<< quickQuery' c "select rowid, * from emails" []
      comp'' <- execStateT (pl c) comp'
      print =<< readTableStructure c "emails"
      print =<< quickQuery' c "select rowid, * from emails" []

main :: IO ()
main = do
  c <- connectSqlite3 ":memory:"
  compConn <- connectSqlite3 ":memory:"
  testDB c
  testRename c compConn
  testDrop c  compConn 
  testInsert c compConn
  testAppend c compConn
  testInsertColumn c compConn
  testDeleteColumn c compConn
  disconnect c
  disconnect compConn