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
      runRaw c $ "insert into moreEmails values ('From:god','bang')"
      execStateT (pl c) c'
      e2 <- getTable c "emails"
      me2 <- getTable c "moreEmails"
      print =<< quickQuery' c ("select * from emails")  []
      print =<< quickQuery' c ("select * from moreEmails")  []
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
      runRaw c $ "update otherEmails set body='other body' where rowid=2"  
      b <- hasTable c "otherEmails"
      print b
      e1 <- getTable c "otherEmails"
      print $ e == e1
      c'' <- execStateT (pl c) c'
      b <- hasTable c "otherEmails"
      print $ not b 
      execStateT (pr c) c''
      print =<< quickQuery' c ("select * from otherEmails")  []
      
testAppend :: Conn -> Conn -> IO ()
testAppend c compConn = do
  e <- getTable c "emails"
  me <- getTable c "moreEmails"
  case append compConn (const True) "emails" "moreEmails" "allEmails" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      runRaw c $ "insert into allEmails values ('From:god', 'bigger bang')"
      runRaw c $ "update allEmails set header='From:Rome' where rowid=4"
      print =<< quickQuery' c "select * from allEmails" []
      b <- hasTable c "emails"
      print $ not b
      b <- hasTable c "moreEmails"
      print $ not b
      comp'' <- execStateT (pl c) comp'
      runRaw c $ "insert into moreEmails values ('From:Arjun', 'What bang?')"
      runRaw c $ "update emails set header='From:Arjun' where rowid=4"
      b <- hasTable c "allEmails"
      print $ not b      
      print =<< quickQuery' c ("select * from emails")  []
      print =<< quickQuery' c ("select * from moreEmails")  []
      _ <- execStateT (pr c) comp''
      print =<< quickQuery' c ("select * from allEmails")  []

testSplit :: Conn -> Conn -> IO ()
testSplit c compConn = do
  e <- getTable c "emails"
  me <- getTable c "moreEmails"
  case split compConn (\(a:_) -> fromSql a == "From:Raghu") "emails" "raghus" "others" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< quickQuery' c "select * from raghus" []
      print =<< quickQuery' c "select * from others" []
      runRaw c $ "insert into raghus values ('From:god', 'bigger bang')"
      runRaw c $ "update raghus set header='From:Rome' where rowid=1"
      print =<< quickQuery' c "select * from raghus" []
      print =<< quickQuery' c "select * from others" []
      comp'' <- execStateT (pl c) comp'
      runRaw c $ "insert into emails values ('From:Arjun', 'What bang?')"
      runRaw c $ "update emails set header='From:Arjun' where rowid=4"
      print =<< quickQuery' c ("select * from emails")  []
      _ <- execStateT (pr c) comp''
      print =<< quickQuery' c ("select * from raghus")  []
      print =<< quickQuery' c ("select * from others")  []
      
testInsertColumn :: Conn -> Conn -> IO ()
testInsertColumn c compConn = do
  e <- getTable c "emails"
  case insertColumn compConn "emails" "timestamp" "VARCHAR(20)" "'20 May 2013'" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      runRaw c $ "insert into emails values ('From:Arjun', 'What bang?', 'April 1 2013')"
      runRaw c $ "update emails set timestamp='August 20 2000' where rowid=2"
      print =<< quickQuery' c "select rowid, * from emails" []
      comp'' <- execStateT (pl c) comp'
      print =<< quickQuery' c "select rowid, * from emails" []
      execStateT (pr c) comp''
      print =<< quickQuery' c "select rowid, * from emails" []

testDeleteColumn :: Conn -> Conn -> IO ()
testDeleteColumn c compConn = do
  e <- getTable c "emails"
  case deleteColumn compConn "emails" "header" "VARCHAR(20)" "'From:Haskell'" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      print =<< quickQuery' c "select rowid, * from emails" []
      comp'' <- execStateT (pl c) comp'
      print =<< quickQuery' c "select rowid, * from emails" []

testRenameColumn :: Conn -> Conn -> IO ()
testRenameColumn c compConn = do
  e <- getTable c "emails"
  case renameColumn compConn "emails" "header" "fromfield" of
    SymLens comp pr pl -> do
      comp' <- execStateT (pr c) comp
      runRaw c $ "insert into emails values ('From:Arjun', 'What bang?')"
      runRaw c $ "update emails set fromfield='From:God' where rowid=2"
      print =<< readTableStructure c "emails"
      print =<< quickQuery' c "select rowid, * from emails" []
      putStrLn ""
      comp'' <- execStateT (pl c) comp'
      runRaw c $ "update emails set header='From:Satvik Chauhan' where rowid=1"
      print =<< readTableStructure c "emails"
      print =<< quickQuery' c "select rowid, * from emails" []
      putStrLn ""
      execStateT (pr c) comp''
      print =<< readTableStructure c "emails"
      print =<< quickQuery' c "select rowid, * from emails" []
      putStrLn ""

main :: IO ()
main = do
  c <- connectSqlite3 ":memory:"
  compConn <- connectSqlite3 ":memory:"
  testDB c
--  testRename c compConn
--  testDrop c  compConn 
--  testInsert c compConn
--  testAppend c compConn
--  testSplit  c compConn
--  testInsertColumn c compConn
--  testDeleteColumn c compConn
  testRenameColumn c compConn
  disconnect c
  disconnect compConn