{-# LANGUAGE OverloadedStrings #-}

module Email where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad.Trans
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Monoid
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)
import Network.Wai
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Database as DB

emails :: DB.Database
emails = putTable "email" emailTable DB.empty
  where emailTable = Table ["headers", "body"] records
        records    = M.fromList [(0, ["From:Satvik", "Hello"]),
                                 (1, ["From:Raghu", "Greetings"])]  

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static/")
    db <- liftIO $ newMVar emails
    -- Routes Definitions

    get "/" $ do
        file "index.html"
        header "Content-Type" "text/html"

    

    get "/db/:id" $ do
        docid <- param "id"
        dbMap <- liftIO $ readMVar db
        case M.lookup docid dbMap of
             Nothing -> raise $ "Id not found"
             Just fp -> file fp

