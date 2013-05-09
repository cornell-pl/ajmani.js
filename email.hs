{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Email where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad.Trans
import Control.Concurrent.MVar
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Monoid
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)
import Network.Wai
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Database as DB

-- Just for Testing
emails :: DB.Database
emails = DB.putTable "email" emailTable DB.empty
  where emailTable = DB.Table ["headers", "body"] records
        records    = M.fromList [(0, ["From:Satvik", "Hello"]),
                                 (1, ["From:Raghu", "Greetings"])]  
                                
instance Parsable BL.ByteString where
  parseParam t = fmap (\a -> BL.fromChunks [a]) $ parseParam t 

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static/")
    dbVar <- liftIO $ newMVar emails
    -- Routes Definitions

    get "/" $ do
        file "index.html"
        header "Content-Type" "text/html"

    get "/tables" $ do
        db <- liftIO $ readMVar dbVar
        v :: Int <- param "version"
        json $ DB.getTableNames db
    
    get "/table/:name" $ do
        db <- liftIO $ readMVar dbVar
        name :: String <- param "name"
        v :: Int <- param "version"
        json $ DB.getTableByName name db

        
    get "/table/:name/headers" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"        
        json $ fmap DB.getHeaders $ DB.getTableByName name db
        
    get "/table/:name/records" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"                
        json $ fmap DB.getRecords $ DB.getTableByName name db
    
    get "/table/:name/record/:id" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        json $ DB.getRecordById i =<< DB.getTableByName name db
    
    -- The following operations are not atomic, as the reads and writes can interleave 
    -- with those of other threads. These need to be made atomic, by lifting the 
    -- modifyMVar operation to the top level, as this is an atomic operation.
    
    put "/table/:name" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        bs <- param "table"
        table <- maybe (raise "Table: no parse") return $ A.decode bs
        -- get table contents
        liftIO $ modifyMVar_ dbVar $ const . return $ DB.putTable name table db
        text "Success"
        
    put "/table/:name/record/:id" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        bs <- param "fields"
        fields <- maybe (raise "Fields: no parse") return $ A.decode bs
        -- get field contents
	case DB.getTableByName name db of
          Just table -> do liftIO $ modifyMVar_ dbVar $ const . return $ 
                             DB.putTable name (DB.putRecord i fields table) db
                           text "Success"
          Nothing    -> text "Failure"
          
    post "/table/:name/record" $ do
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        bs <- param "fields"
        fields <- maybe (raise "Fields: no parse") return $ A.decode bs
	case DB.getTableByName name db of
          Just table -> let (i, table') = DB.createRecord fields table in
                        do liftIO $ modifyMVar_ dbVar $ const . return $ 
                             DB.putTable name table' db
                           json i
          Nothing    -> json $ T.pack "Failure"
          
    delete "/table/:name" $ do   
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        v :: Int <- param "version"
        liftIO $ modifyMVar_ dbVar $ const. return $ DB.delTable name db
        json $ T.pack "Success"
        
    delete "/table/:name/record/:id" $ do   
        db <- liftIO $ readMVar dbVar
        name <- param "name"
        i <- param "id"
        v :: Int <- param "version"
        case DB.getTableByName name db of
          Just table -> do liftIO $ modifyMVar_ dbVar $ const. return $ 
                             DB.putTable name (DB.delRecord i table) db
                           json $ T.pack "Success"
          Nothing    -> json $ T.pack "Failure"
        
    