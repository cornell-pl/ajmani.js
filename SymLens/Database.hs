{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module SymLens.Database where

import SymLens 
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe,fromJust,maybe)
import Control.Monad.State
import Data.List
import Control.Monad.Random
import Data.Char
import Control.Monad

import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Tuple as T

type TableLens = SymLens Table Table
type Conn = Connection
type DatabaseLens = SymLens Conn Conn

-- liftTableLens :: Name -> TableLens -> DatabaseLens
-- liftTableLens n (SymLens s pr pl) = SymLens s (doit pr) (doit pl)
--   where doit put d c = T.swap $ Map.mapAccumWithKey onT c d
--           where onT c n' t | n == n'   = T.swap $ put t c 
--                            | otherwise = (c,t) 

-- Assumes no table named temp_temp

type Name = String
type Header = (SqlValue, [SqlValue])
data Table = Table [SqlValue] (Map SqlValue [SqlValue])

rename :: Name -> Name -> DatabaseLens
rename n1 n2 = SymLens () put put
  where sigma n | n == n1   = n2
                | n == n2   = n1
                | otherwise = n
        put c = do
          ts <- lift $ getTables c
          lift (renameTable ts) >> return c
            where
              renameTable ts | elem n1 ts && elem n2 ts = do
                run c ("ALTER TABLE " ++ n1 ++ " RENAME TO temp_temp") []
                run c ("ALTER TABLE " ++ n2 ++ " RENAME TO " ++ n1) []
                run c ("ALTER TABLE temp_temp" ++ " RENAME TO " ++ n2 ) []
                             | elem n1 ts =
                run c ("ALTER TABLE " ++ n1 ++ " RENAME TO " ++ n2) []
                             | elem n2 ts =
                run c ("ALTER TABLE " ++ n2 ++ " RENAME TO " ++ n1) []
                             | otherwise = return 0

  
drop :: Name -> DatabaseLens
drop n = SymLens Nothing pr pl
  where pr :: Connection -> StateT (Maybe String) IO Connection
        pr c = do 
          ts <- lift $ getTables c
          lift (dropTable ts) >>= put
          return c
          where dropTable ts | elem n ts = do 
                  n' <- getUniqueName c
                  run c ("ALTER TABLE " ++ n ++ " RENAME TO " ++ n') []
                  return (Just n')
                             | otherwise = return Nothing
        pl :: Connection -> StateT (Maybe String) IO Connection                               
        pl c = do                    
          ts <- lift $ getTables c
          (Just n') <- get 
          lift (undropTable n' ts) >> return c
          where undropTable n' ts | elem n ts = do
                  run c ("DROP TABLE " ++ n) []
                  run c ("ALTER TABLE " ++ n' ++ " RENAME TO " ++ n) []
                               | otherwise = do
                  run c ("ALTER TABLE " ++ n' ++ " RENAME TO " ++ n) []

                               
--   where pr d = maybe (return d) trans $ Map.lookup n d
--             where
--               trans t' = put (Just t') >> return (Map.delete n d)
--         pl d = do
--           c <- get
--           maybe (return d) trans c
--             where
--               trans v = return $ Map.insert n v d

-- insert :: Name -> Table -> DatabaseLens
-- insert n t@(Table _ m) = SymLens Nothing pr pl
--   where pr d = maybe (put (Just t) >> return (Map.insert n t d)) trans =<< get
--           where trans v = return $ Map.insert n v d
--         pl d = maybe (return d) trans $ Map.lookup n d
--           where trans t' = put (Just t') >> return (Map.delete n d)

-- -- -- Take a pred depending on which the new records will go to n1 or n2.
-- -- -- putr :: Database with (n1,n2) and without n -> Database with n and without (n1,n2) 
-- -- -- putl :: Database with n and without (n1,n2) -> Database with (n1,n2) and without n

-- -- Compelement is a pair of maps mapping keys from the the two initial tables to the appended table 
-- append :: (Id -> Fields -> Bool) 
--        -> Name 
--        -> Name 
--        -> Name 
--        -> DatabaseLens
-- append on n1 n2 n = SymLens (Bimap.empty, Bimap.empty) pr pl
--   where pr d = do
--           c@(lc,rc) <- get
--           case (Map.lookup n1 d, Map.lookup n2 d) of
--             (Just (Table h1 m1), Just (Table h2 m2)) | h1 == h2  -> put (lc',rc') >> return (Map.insert n (Table h1 m') $ Map.delete n1 $ Map.delete n2 d)
--               where (m', rc', _) = Map.foldlWithKey combine (m, rc, newkey') m2
--                     (m, lc', newkey') = Map.foldlWithKey combine (Map.empty, lc, newkey) m1
--                     combine (m, c, nextkey) k v = 
--                       case Bimap.lookup k c of
--                         Just k' -> (Map.insert k' v m, c, nextkey)
--                         Nothing -> (Map.insert nextkey v m, Bimap.insert k nextkey c, nextkey + 1)
--                     newkey = (maxR lc `max` maxR rc) + 1
--                     maxR bm = if Bimap.null bm then -1 else fst $ Bimap.findMaxR bm
--             _                                                    -> return d
--         pl d = do
--           c@(lc, rc) <- get
--           case Map.lookup n d of
--             Just (Table h m) -> put (lc',rc') >> return (Map.insert n1 (Table h m1) $ Map.insert n2 (Table h m2) $ Map.delete n d)
--               where m1' = Map.mapKeys (fromJust  . flip Bimap.lookupR lc) $ Map.intersection m lcmap
--                     m2' = Map.mapKeys (fromJust  . flip Bimap.lookupR rc) $ Map.intersection m rcmap
--                     (m1,m2,(lc',rc'),_) = Map.foldlWithKey combine (m1',m2',(lc,rc),(next1,next2)) rest
--                     rest = Map.difference (Map.difference m lcmap) rcmap
--                     combine (m1,m2,(lc',rc'),(next1,next2)) k v 
--                       | on k v = (Map.insert next1 v m1, m2, (Bimap.insert next1 k lc', rc'), (next1 + 1, next2))
--                       | otherwise = (m1, Map.insert next2 v m2, (lc', Bimap.insert next2 k rc'), (next1, next2 + 1))
--                     next1 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m1'
--                     next2 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m2'
--                     lcmap = Bimap.toMapR lc 
--                     rcmap = Bimap.toMapR rc
--             _                                                    -> return d

-- {-        pl d c@(lc, rc) = case Map.lookup n d of
--           Just (Table h m) -> (Map.insert n1 (Table h m1) $ Map.insert n2 (Table h m2) $ Map.delete n d, (lc', rc'))
--             where m1' = Map.mapKeys (fromJust  . flip Map.lookup lc) $ Map.intersection m lc
--                   m2' = Map.mapKeys (fromJust  . flip Map.lookup rc) $ Map.intersection m rc
--                   (m1,m2,(lc',rc'),_) 
--                      = Map.foldlWithKey combine (m1',m2',(lc,rc),(next1,next2)) 
--                          $ rest
--                   rest = Map.difference (Map.difference m lc) rc
--                   combine (m1,m2,(lc',rc'),(next1,next2)) k v 
--                     | on k v = (Map.insert next1 v m1, m2, (Map.insert k next1 lc', rc'), (next1 + 1, next2))
--                     | otherwise = (m1, Map.insert next2 v m2, (lc', Map.insert k next2 rc'), (next1, next2 + 1))
--                   next1 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m1'
--                   next2 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m2'
                  
--           _                                                    -> (d,c)
-- -}        
-- -- | Takes a function to filter the table on and then splits the table
-- -- into two tables, first satisfying the predicate and other the rest.
-- -- I doubt this is not a lens. Consider t split on f to t1 and t2. Now
-- -- we insert a record r to t1 satisying ~f. Then the lens laws will be
-- -- broken.  Maintain two tables of additional records added and apply
-- -- f if r is not in these table otherwise just split based on r
-- -- belongs to which table.
-- split :: (Id -> Fields -> Bool) -> Name -> Name -> Name -> DatabaseLens
-- split on n n1 n2 = inv $ append on n1 n2 n

-- Returns Int from execute of HDBC. See doc for details. Deletes
-- previous table if already exist.

getUniqueName :: Conn -> IO Name
getUniqueName c = do
  ts <- getTables c
  getRandomName 12 0 ts
  where getRandomName n count ts | count > 50 = getRandomName (n+1) 0 ts
                                 | otherwise = do
                                   v <- rndString n
                                   if elem v ts then getRandomName n (count+1) ts else return v

rndString :: Int -> IO String
rndString n = do
  values <- evalRandIO (sequence (replicate n rnd))
  return (map chr values)
  where
    rnd :: (RandomGen g) => Rand g Int
    rnd = getRandomR (97,122)

hasTable :: Conn -> Name -> IO Bool
hasTable c n = getTables c >>= return . elem n

