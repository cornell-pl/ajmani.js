{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad.Trans
import Control.Concurrent.MVar
import Data.Aeson hiding (json)
import qualified Data.Map as M
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

import qualified Database as DB
import qualified SchemaChange as SC
import qualified Data.List as List
import Control.Applicative ((<$>),(<*>))
-- Just for Testing
emails :: DB.Database
emails = DB.putTable "moreEmail" emailTable2 
       $ DB.putTable "email" emailTable DB.empty
  where emailTable = DB.Table ["headers", "body"] records
        records    = M.fromList [(0, ["From:Satvik", "Hello"]),
                                 (1, ["From:Raghu", "Greetings"])]  
        emailTable2 = DB.Table ["headers", "body"] records2
        records2    = M.fromList [(1, ["From:Raghu", "Hello"]),
                                  (2, ["From:Satvik", "Greetings"])]                                 
                                
instance Parsable BL.ByteString where
  parseParam t = fmap (\a -> BL.fromChunks [a]) $ parseParam t 

type Version = Int
data Versioned a = Versioned Version a deriving (Show)

instance (ToJSON a) => ToJSON (Versioned a) where
  toJSON (Versioned v a) = object [ "version" .= v
                                  , "body"    .= a
                                  ]
instance (FromJSON a) => FromJSON (Versioned a) where
  parseJSON (Object v) = Versioned <$>
                        v .: "version" <*>
                        v .: "body"
  parseJSON _          = mzero

versioned :: MVar Version -> a -> ActionM (Versioned a)
versioned mv a = do
  v <- liftIO $ readMVar mv
  return $ Versioned v a
  
main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static/")
    dbVar <- liftIO $ newMVar emails
    dbVer <- liftIO $ newMVar (0 :: Version)
    -- Routes Definitions

    get "/" $ do
        file "index.html"
        header "Content-Type" "text/html"

    get "/tables" $ do
        db <- liftIO $ readMVar dbVar
        v :: Int <- param "version"
        json =<< (versioned dbVer $ DB.getTableNames db)
    
    get "/table/:name" $ do
        db <- liftIO $ readMVar dbVar
        name :: String <- param "name"
        v :: Int <- param "version"
        liftIO $ print v
        json =<< (versioned dbVer $ DB.getTableByName name db)
        
    get "/table/:name/headers" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"        
        json =<< (versioned dbVer $ fmap DB.getHeaders $ DB.getTableByName name db)
        
    get "/table/:name/records" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"                
        json =<< (versioned dbVer $ fmap DB.getRecords $ DB.getTableByName name db)
    
    get "/table/:name/record/:id" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        json =<< (versioned dbVer $ DB.getRecordById i =<< DB.getTableByName name db)
    
    -- The following operations are not atomic, as the reads and writes can interleave 
    -- with those of other threads. These need to be made atomic, by lifting the 
    -- modifyMVar operation to the top level, as this is an atomic operation.
    
    put "/table/:name" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        bs <- param "table"
        table <- maybe (raise "Table: no parse") return $ decode bs
        -- get table contents
        liftIO $ modifyMVar_ dbVar $ const . return $ DB.putTable name table db
        json =<< (versioned dbVer $ T.pack "Success")
        
    put "/table/:name/record/:id" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        bs <- param "fields"
        fields <- maybe (raise "Fields: no parse") return $ decode bs
        -- get field contents
        case DB.getTableByName name db of
          Just table -> do liftIO $ modifyMVar_ dbVar $ const . return $ 
                             DB.putTable name (DB.putRecord i fields table) db
                           json =<< (versioned dbVer $ T.pack "Success")
          Nothing    -> json =<< (versioned dbVer $ T.pack "Failure")
          
    post "/table/:name/record" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        bs <- param "fields"
        fields <- maybe (raise "Fields: no parse") return $ decode bs
        case DB.getTableByName name db of
          Just table -> let (i, table') = DB.createRecord fields table in
                        do liftIO $ modifyMVar_ dbVar $ const . return $ 
                             DB.putTable name table' db
                           json =<< (versioned dbVer i)
          Nothing    -> json =<< (versioned dbVer $ T.pack "Failure")
          
    delete "/table/:name" $ do   
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        liftIO $ modifyMVar_ dbVar $ const. return $ DB.delTable name db
        json =<< (versioned dbVer $ T.pack "Success")
        
    delete "/table/:name/record/:id" $ do   
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        case DB.getTableByName name db of
          Just table -> do liftIO $ modifyMVar_ dbVar $ const. return $ 
                             DB.putTable name (DB.delRecord i table) db
                           json =<< (versioned dbVer $  T.pack "Success")
          Nothing    -> json =<< (versioned dbVer $ T.pack "Failure")
        
testDelete :: IO ()
testDelete = do
  let Just t = DB.getTableByName "email" emails
  let t' = SC.insertColumn "starred" "false" t
  let t'' = SC.deleteColumn "starred" t'
  putStrLn $ show $ t'
  putStrLn $ show $ t''
    
-- Testing
test :: IO ()
test = do 
  let Just t = DB.getTableByName "email" emails
  case SC.apply $ SC.InsertColumn "starred" "false" of
    (SC.SymLens mis pr pl) -> do let (t', _) = pr t mis 
                                 putStrLn $ show $ t'
                                 let (t'', cs) = pl t' mis
                                 putStrLn$ show $ SC.deleteColumn "starred" t'
                                 let (t''', _) = pr t'' cs
                                 putStrLn $ show $ t'''
                                 
testAppend :: IO ()
testAppend = do 
  let Just t = DB.getTableByName "email" emails
  let Just t' = DB.getTableByName "moreEmail" emails
  print t
  print t' 
  case SC.apply SC.Append of
    SC.SymLens mis pr pl -> do let (u, c) = pr (t,t') mis
                               print u 
                               let ((u', u''), _) = pl u c
                               print u'
                               print u''
                               
                               
testSplit :: IO ()
testSplit = do 
  let Just t = DB.getTableByName "email" emails
  let Just t' = DB.getTableByName "moreEmail" emails
  case SC.apply SC.Split of 
    SC.SymLens mis pr pl -> do
      let (u, c) = pl (t,t') mis
      let ((v, v'), _) = pr u c
      print v
      print v'
      let ((w, w'), _) = pr u mis
      print w
      print w'                      
  