{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad.Trans
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Monoid
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)
import Network.Wai
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO 

import Data.Text.Lazy.Encoding (decodeUtf8)

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    
    db <- liftIO $ newMVar (M.singleton 0 "test.csv" :: M.Map Int String)
    -- Routes Definitions
    get "/" $ text "Evolution Test Application"
    get "/db/:id" $ do
    	docid <- param "id"
	dbMap <- liftIO $ readMVar db
	case M.lookup docid dbMap of
	     Nothing -> raise $ "Id not found"
	     Just fp -> do 
	     	  liftIO $ print "File"
	     	  liftIO $ print fp
	     	  file fp
	     	     

