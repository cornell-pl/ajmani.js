{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad.Trans
import Control.Concurrent.MVar
import Data.Aeson hiding (json)
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad (mzero)

import Network.HTTP.Types (status302)
import Network.Wai
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.Text.Lazy.Encoding (decodeUtf8)

import Database.Memory (Database, Table, Name)
import qualified Database.Memory as DB
import qualified SchemaChange as SC
import SymLens (SymLens(..))
import SymLens.Database
import SymLens.Table
import qualified SymLens as S
import qualified Data.List as List
import Control.Applicative ((<$>),(<*>))
-- Just for Testing

emails :: DB.Database
emails = DB.putTable "moreEmail" emailTable2 
       $ DB.putTable "email" emailTable DB.empty
  where emailTable = DB.Table ["headers", "body"] records
        records    = Map.fromList [ (0, ["From:Satvik", "Hello"])
                                  , (1, ["From:Raghu", "Greetings"])
                                  , (2, ["From:Raghu", "Tomorrow afternoon"])
                                  , (4, ["From:Nate", "Lets turn the crank"])
                                  , (5, ["From:Raghu", ":)"])]
        emailTable2 = DB.Table ["headers", "body"] records2
        records2    = Map.fromList [(1, ["From:Raghu", "Hello"]),
                                    (2, ["From:Satvik", "Greetings"])]

emailsV1 :: DB.Database
emailsV1 = emails      

emailsV2 :: DB.Database
emailsV2 = 
  DB.putTable "moreEmail" emailTable2 
    $ DB.putTable "email" emailTable DB.empty
  where emailTable = DB.Table ["timestamp", "headers", "body"] records
        records    = Map.fromList [(0, ["10 May 2012", "From:Satvik", "Hello"]),
                                 (1, ["1 Jan 2013", "From:Raghu", "Greetings"])]  
        emailTable2 = DB.Table ["starred", "headers", "body"] records2
        records2    = Map.fromList [(1, ["true", "From:Raghu", "Hello"]),
                                  (2, ["false", "From:Satvik", "Greetings"])]

emailsV2' :: DB.Database
emailsV2' = 
  DB.putTable "moreEmail" emailTable2 
    $ DB.putTable "email" emailTable DB.empty
  where emailTable = DB.Table ["timestamp", "headers", "body"] records
        records    = Map.fromList [(0, ["10 May 2012", "From:Satvik", "This is modified"]),
                                   (1, ["1 Jan 2013", "From:Raghu", "Greetings"]),
                                   (2, ["13 May 2013", "From:Nate", "At 10:30"])]  
        emailTable2 = DB.Table ["starred", "headers", "body"] records2
        records2    = Map.fromList [(1, ["true", "From:Raghu", "Hello"]),
                                    (2, ["false", "From:Satvik", "This is modified"])]

                                  
emailsV3 :: DB.Database
emailsV3 = 
  DB.putTable "moreEmail" emailTable2 
    $ DB.putTable "email" emailTable DB.empty
  where emailTable = DB.Table ["priority", "timestamp", "headers"] records
        records    = Map.fromList [(0, ["high", "10 May 2012", "From:Satvik"]),
                                 (1, ["low", "1 Jan 2013", "From:Raghu"])]  
        emailTable2 = DB.Table ["priority", "starred", "headers"] records2
        records2    = Map.fromList [(1, ["medium", "true", "From:Raghu"]),
                                  (2, ["deadline-grade", "false", "From:Satvik"])]
                                  
instance Parsable BL.ByteString where
  parseParam t = fmap (\a -> BL.fromChunks [a]) $ parseParam t 

type Version = Int

-- | Versioned value.
data Versioned a = Versioned Version a
                 deriving (Show)

-- | Versioned 1 "hello" <=> {"version": 1, body: "hello"}
instance (ToJSON a) => ToJSON (Versioned a) where
  toJSON (Versioned v a) = object [ "version" .= v
                                  , "body"    .= a
                                  ]
instance (FromJSON a) => FromJSON (Versioned a) where
  parseJSON (Object v) = Versioned <$>
                        v .: "version" <*>
                        v .: "body"
  parseJSON _          = mzero
  
type DBLens = Map.Map Name (SymLens Table Table)
type Optometrist = [(Version, DBLens)]

applyLensR :: DBLens 
           -> Database 
           -> Database
applyLensR l db =
  Map.mapWithKey (\k t -> case Map.lookup k l of
                            Just (SymLens c pr _) -> fst $ pr t c
                            Nothing               -> t
                 ) db          

applyLensL :: DBLens 
           -> Database 
           -> Database
applyLensL l db =
  Map.mapWithKey (\k t -> case Map.lookup k l of
                            Just (SymLens c _ pl) -> fst $ pl t c
                            Nothing               -> t
                 ) db
                 
updateLensL :: DBLens 
            -> Database 
            -> (DBLens, Database)
updateLensL l db =
  Map.mapAccumWithKey (\l' k t -> case Map.lookup k l of
                                    Just (SymLens c pr pl) -> let (t',c') = pl t c in
                                                              (Map.insert k (SymLens c' pr pl) l', t') 
                                    Nothing                -> (l', t)
                      ) Map.empty db
                      
updateLensR :: DBLens 
            -> Database 
            -> (DBLens, Database)
updateLensR l db =
  Map.mapAccumRWithKey (\l' k t -> case Map.lookup k l of
                                     Just (SymLens c pr pl) -> let (t',c') = pr t c in
                                                               (Map.insert k (SymLens c' pr pl) l', t') 
                                     Nothing                -> (l', t)
                       ) Map.empty db
                      
                  
runOptometrist :: (Version, Version) 
               -> Optometrist 
               -> Database 
               -> Database
runOptometrist (from,to) opt db | from == to = db
                                | from  < to = 
  List.foldl (\currDB (_, m) -> applyLensR m currDB) db 
    $ List.filter (\(v, _) -> from <= v && v < to) opt
                                | from  > to =
  List.foldr (\(_, m) currDB -> applyLensL m currDB) db 
    $ List.filter (\(v, _) -> to <= v && v < from) opt
      
runUpdate :: Version 
          -> Database 
          -> Optometrist 
          -> Optometrist
runUpdate ver db opt = 
  let (l1, l2) = List.partition ((<ver) . fst) opt in
  snd (List.mapAccumR (\db' (v,m) -> let (m', db'') = updateLensL m db' in
                                     (db'', (v,m'))) db l1)
  ++
  snd (List.mapAccumL (\db' (v,m) -> let (m', db'') = updateLensR m db' in
                                    (db'', (v,m'))) db l2)   

updateAndGet :: (Version, Version) 
             -> Database 
             -> Optometrist 
             -> (Database, Optometrist)
updateAndGet (from, to) db opt =
  let opt' = runUpdate from db opt in
  let db'  = runOptometrist (from,to) opt' db in
  (db', opt')              

emailDoc :: Optometrist
emailDoc = 
  [(0, Map.fromList [("email", email1), ("moreEmail", moreEmail1)]),
   (1, Map.fromList [("email", email2), ("moreEmail", moreEmail2)]),
   (2, Map.empty)]
  where email1 = SC.apply $ SC.InsertColumn "timestamp" "Jan 1 1970"
        moreEmail1 = SC.apply $ SC.InsertColumn "starred" "false"
        email2 = SC.apply $ SC.Compose (SC.Compose (SC.Compose (SC.SwapColumn "timestamp" "body") (SC.DeleteColumn "body" "Hello"))
                                       (SC.InsertColumn "priority" "low")) (SC.SwapColumn "timestamp" "header")

        moreEmail2 = SC.apply $ SC.Compose (SC.Compose (SC.Compose (SC.SwapColumn "starred" "body") 
                                                       (SC.DeleteColumn "body" "Turn the crank!")) 
                                           (SC.InsertColumn "priority" "deadline-grade")) (SC.SwapColumn "starred" "header")
                                            
testEmail :: IO ()
testEmail = do
  let v1From2 = runOptometrist (2,1) emailDoc emailsV2
  print emailsV1
  print v1From2
  let emailDoc' = runUpdate 2 emailsV2 emailDoc
  let v2From1 = runOptometrist (1,2) emailDoc' emailsV1
  print emailsV2
  print v2From1
  let v3From2 = runOptometrist (2,3) emailDoc emailsV2
  print emailsV3
  print v3From2
  
testUpdate :: IO ()
testUpdate = do
  let emailDoc' = runUpdate 2 emailsV2' emailDoc
  let v3From2 = runOptometrist (2,3) emailDoc' emailsV2'
  let v1From3_1 = runOptometrist (3,1) emailDoc' v3From2
  print v3From2 
  print v1From3_1
  
main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static/")
    dbVar <- liftIO $ newMVar (emailsV3, (2 :: Version), emailDoc)
    -- Routes Definitions

    get "/" $ do
        file "index.html"
        header "Content-Type" "text/html"

    get "/tables" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        json $ Versioned ver $ DB.getTableNames db
    
    get "/table/:name" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar 
        name <- param "name"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        liftIO $ print ver
        json $ Versioned ver $ DB.getTableByName name db
        
    get "/table/:name/headers" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar 
        name <- param "name"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        json $ Versioned ver $ fmap DB.getHeaders $ DB.getTableByName name db
        
    get "/table/:name/records" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar 
        name <- param "name"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'   
        json $ Versioned ver $ fmap DB.getRecords $ DB.getTableByName name db
    
    get "/table/:name/record/:id" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar 
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        json $ Versioned ver $ DB.getRecordById i =<< DB.getTableByName name db
    
    -- The following operations are not atomic, as the reads and writes can interleave 
    -- with those of other threads. These need to be made atomic, by lifting the 
    -- modifyMVar operation to the top level, as this is an atomic operation.
    
    put "/table/:name" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar 
        name <- param "name"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        bs <- param "table"
        table <- maybe (raise "Table: no parse") return $ decode bs
        -- get table contents
        let db'' = DB.putTable name table db
        let (db''', doc') = updateAndGet (v, ver) db'' doc
        liftIO $ modifyMVar_ dbVar $ const . return $ (db''', ver, doc')
        json $ Versioned ver $ T.pack "Success"
        
    put "/table/:name/record/:id" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        bs <- param "fields"
        fields <- maybe (raise "Fields: no parse") return $ decode bs
        -- get field contents
        case DB.getTableByName name db of
          Just table -> do let db'' = DB.putTable name (DB.putRecord i fields table) db
                           let (db''', doc') = updateAndGet (v,ver) db'' doc
                           liftIO $ modifyMVar_ dbVar $ const . return $ (db''', ver, doc')
                           json $ Versioned ver $ T.pack "Success"
          Nothing    -> json $ Versioned ver $ T.pack "Failure"
          
    post "/table/:name/record" $ do
        (db', ver, doc) <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        bs <- param "fields"
        fields <- maybe (raise "Fields: no parse") return $ decode bs
        case DB.getTableByName name db of
          Just table -> do let (i, table') = DB.createRecord fields table
                           let db'' = DB.putTable name table' db 
                           let (db''', doc') = updateAndGet (v,ver) db'' doc
                           liftIO $ modifyMVar_ dbVar $ const . return $ (db''', ver, doc')
                           json $ Versioned ver i
          Nothing    -> json $ Versioned ver $ T.pack "Failure"
          
    delete "/table/:name" $ do   
        (db', ver, doc) <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        let db'' = DB.delTable name db
        let (db''', doc') = updateAndGet (v, ver) db'' doc
        liftIO $ modifyMVar_ dbVar $ const . return $ (db''', ver, doc') 
        json $ Versioned ver $ T.pack "Success"
        
    delete "/table/:name/record/:id" $ do   
        (db', ver, doc) <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        let db = runOptometrist (ver, v) doc db'
        case DB.getTableByName name db of
          Just table -> do let db'' = DB.putTable name (DB.delRecord i table) db 
                           let (db''', doc') = updateAndGet (v, ver) db'' doc
                           liftIO $ modifyMVar_ dbVar $ const . return $ (db''', ver, doc')
                           json $ Versioned ver $  T.pack "Success"
          Nothing    -> json $ Versioned ver $ T.pack "Failure"
        
--testDelete :: IO ()
--testDelete = do
--  let Just t = DB.getTableByName "email" emails
--  let t' = SC.insertColumn "starred" "false" t
--  let t'' = SC.deleteColumn "starred" t'
--  putStrLn $ show $ t'
--  putStrLn $ show $ t''
    
-- Testing
test :: IO ()
test = do 
  let Just t = DB.getTableByName "email" emails
  case SC.apply $ SC.InsertColumn "starred" "false" of
    (SC.SymLens mis pr pl) -> do let (t', _) = pr t mis 
                                 putStrLn $ show $ t'
                                 let (t'', cs) = pl t' mis
--                                 putStrLn$ show $ SC.deleteColumn "starred" t'
                                 let (t''', _) = pr t'' cs
                                 putStrLn $ show $ t'''
                                 
-- testAppend :: IO ()
-- testAppend = do 
--   let Just t = DB.getTableByName "email" emails
--   let Just t' = DB.getTableByName "moreEmail" emails
--   print t
--   print t' 
--   case SC.apply SC.Append of
--     SC.SymLens mis pr pl -> do let (u, c) = pr (t,t') mis
--                                print u 
--                                let ((u', u''), _) = pl u c
--                                print u'
--                                print u''
                               
                               
-- testSplit :: IO ()
-- testSplit = do 
--   let Just t = DB.getTableByName "email" emails
--   let Just t' = DB.getTableByName "moreEmail" emails
--   case SC.apply SC.Split of 
--     SC.SymLens mis pr pl -> do
--       let (u, c) = pl (t,t') mis
--       let ((v, v'), _) = pr u c
--       print v
--       print v'
--       let ((w, w'), _) = pr u mis
--       print w
--       print w'                      
  
tet :: IO ()
tet = case append (\_ _ -> True) "email" "moreEmail" "allEmail" of
    SymLens c pr pl -> do let (d, c') = pr emails c
                          print c'
                          let (Just t) = DB.getTableByName "allEmail" d
                          let (_, t') = DB.createRecord ["head", "testBody"] t
                          -- print t'
                          let t'' = DB.putRecord 1 ["ModHead", "ModBody"] t'
                          print $ fst $ pl (DB.putTable "allEmail" t'' d) c'
                          print d                 

tes :: IO ()
tes = case split (\i _ -> i `rem` 2 == 0) "email" "emaileven" "emailodd" of
    SymLens c pr pl -> do let (d, c') = pr emails c
                          print c'
                          putStrLn ""
                          let (Just t) = DB.getTableByName "emaileven" d
                          let (_, t') = DB.createRecord ["New rec1", "testBody"] t
                          let (_, t'') = DB.createRecord ["New rec2", "testBody"] t'
                          -- print t'
                          -- let t'' = DB.putRecord 1 ["ModHead", "ModBody"] t
                          let d' = DB.putTable "emaileven" t'' d
                          print d 
                          putStrLn ""           
                          print d'
                          putStrLn ""                  
                          print $ fst $ pl d' c'
                          


emailTable = DB.Table ["headers", "body", "Star"] records
 where records    = Map.fromList [ (0, ["From:Satvik", "Hello","1"])
                                 , (1, ["From:Raghu", "Greetings","3"])
                                 , (4, ["From:Nate", "Lets turn the crank","5"])
                                 ]


tei :: IO ()
tei = case insert "semail" emailTable of
  (SymLens i pr pl) -> do
    print "Email"
    print emails
    let (d,c) = pl emails i
    print "After pl"
    print d
    print "Complement"
    print c
    let (d',c') = pr d c
    print "After pr"
    print d'
    print "Complement"
    print c'
       
       
tej :: IO ()
tej =  
  let l = insertColumn "test" "def" in
  let t = DB.Table [] (Map.fromList [(1,[]),(2,[]),(3,[]),(4,[])]) in
  case l of
    (SymLens d pr pl) ->
      let (b,c) = pr t d in
      let (a,c') = pl b c in
      do print a 
         print a;
         putStrLn ""
         print c
         print c'
