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
import Data.List.Utils

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
data Table = Table Name                -- Name from which this table is read
                   (String,[Header])   -- first stores the create statement as it is and second stores the exact header names along with some other info received from PRAGMA table_info
                   [[SqlValue]]        -- first value is the rowid for each record
                   deriving (Show)
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
                n' <- getUniqueName c
                run c ("ALTER TABLE " ++ n1 ++ " RENAME TO " ++ n') []
                run c ("ALTER TABLE " ++ n2 ++ " RENAME TO " ++ n1) []
                run c ("ALTER TABLE " ++ n' ++ " RENAME TO " ++ n2 ) []
                             | elem n1 ts =
                run c ("ALTER TABLE " ++ n1 ++ " RENAME TO " ++ n2) []
                             | elem n2 ts =
                run c ("ALTER TABLE " ++ n2 ++ " RENAME TO " ++ n1) []
                             | otherwise = return 0

drop :: Name -> DatabaseLens
drop n = SymLens Nothing pr pl
  where pr c = do
          ts <- lift $ getTables c
          lift (dropTable ts) >>= put
          return c
          where dropTable ts | elem n ts = do
                  n' <- getUniqueName c
                  run c ("ALTER TABLE " ++ n ++ " RENAME TO " ++ n') []
                  return (Just n')
                             | otherwise = return Nothing
        pl c = do
          ts <- lift $ getTables c
          (Just n') <- get
          lift (undropTable n' ts) >> return c
          where undropTable n' ts | elem n ts = do
                  run c ("DROP TABLE " ++ n) []
                  run c ("ALTER TABLE " ++ n' ++ " RENAME TO " ++ n) []
                               | otherwise = do
                  run c ("ALTER TABLE " ++ n' ++ " RENAME TO " ++ n) []

-- This does not satisfy the lens laws completely.
-- It does so modulo equality of the temp table's name. 
-- However, it does satisfy equality of the contents of the tables.                    
         
insert :: Name -> Table -> DatabaseLens
insert n t = SymLens Nothing pr pl
   where pr c = do
           tn' <- get 
           lift $ case tn' of
                    Just n' -> run c ("ALTER TABLE " ++ n' ++ " RENAME TO " ++ n) [] >> return ()
                    Nothing -> createTable c n t
           return c 
         pl c = do lift doInsertTable >>= \n' -> put (Just n')
                   return c
           where doInsertTable = do 
                   n' <- getUniqueName c
                   run c ("ALTER TABLE " ++ n ++ " RENAME TO " ++ n') []
                   return n'                 
           
-- -- -- Take a pred depending on which the new records will go to n1 or n2.
-- -- -- putr :: Database with (n1,n2) and without n -> Database with n and without (n1,n2)
-- -- -- putl :: Database with n and without (n1,n2) -> Database with (n1,n2) and without n

-- -- Complement is a pair of maps mapping keys from the the two initial tables to the appended table
-- Complement is a pair of tables mapping keys from the two initial tables to those of the appended table.
-- String argument is an optional SQL predicate on the schema of the target (appended) table. 

--append :: Maybe String 
--       -> Name 
--       -> Name 
--       -> Name 
--       -> DatabaseLens
-- 
--append on n1 n2 n = SymLens Nothing pr pl
--  where pr c = do
--          comp <- get
--          lift $ do
--            ns <- case comp of
--                    (Just ns) -> return ns
--                    Nothing   -> do
--                      ns <- getUniqueName c
--                      run c ("CREATE TABLE " ++ ns ++ "(app INTEGER PRIMARY KEY, left INTEGER, right INTEGER)") []
--                      return ns 
--            copyTableStructure c n1 n
--            
--            -- Assumes that the primary key is not autogenerated numeric. 
--            -- If it is autogenerated, then that needs to be projected out in the select
--             
--            run c ("INSERT INTO " ++ ns ++ " (left, right) SELECT (rowid, NULL) FROM " ++ n1 ++
--                     " WHERE NOT EXISTS SELECT * FROM " ++ ns ++ " WHERE left = " ++ n1 ++ ".rowid") []
--            run c ("INSERT INTO " ++ ns ++ " (left, right) SELECT (NULL, rowid) FROM " ++ n2 ++
--                     " WHERE NOT EXISTS SELECT * FROM " ++ ns ++ " WHERE right = " ++ n2 ++ ".rowid") []
--            run c ("INSERT INTO " ++ n ++ " SELECT " ++ n1 ++ ".* FROM " ++ n1 ++ " INNER JOIN " ++ ns ++
--                      " ON " ++ n1 ++ ".rowid = " ++ ns ++ ".left") []
--            run c ("INSERT INTO " ++ n ++ " SELECT " ++ n2 ++ ".* FROM " ++ n2 ++ " INNER JOIN " ++ ns ++
--                      " ON " ++ n2 ++ ".rowid = " ++ ns ++ ".right") []
--          return c
--        pl c = undefined

toBimap :: Table -> Bimap.Bimap Integer Integer
toBimap (Table _ _ rs) = Bimap.fromList (map (\[a,b] -> (fromSql a, fromSql b)) rs)

fromBimap :: Bimap.Bimap Integer Integer -> [[SqlValue]]
fromBimap m = map (\(a,b) -> [toSql a, toSql b]) $ Bimap.toList m

toMap :: Table -> Map.Map Integer Integer
toMap (Table _ _ rs) = Map.fromList (map (\[a,b] -> (fromSql a, fromSql b)) rs)

fromMap :: Map.Map Integer Integer -> [[SqlValue]]
fromMap m = map (\(a,b) -> [toSql a, toSql b]) $ Map.toList m


--fromBimap :: Bimap.Bimap Integer Integer -> Table -> Table
--fromBimap m (Table n strct _) = Table n strct rs
--  where rs = map (\(a,b) -> [toSql a, toSql b]) $ Bimap.toList m
 
append :: ([SqlValue] -> Bool) 
       -> Name 
       -> Name 
       -> Name 
       -> DatabaseLens
append on n1 n2 n = SymLens Nothing pr pl
  where pr :: Conn -> StateT (Maybe (Name, Name)) IO Conn
        pr c = do
          comp <- get
          comp' <- lift $ do
            (ln, rn) <- case comp of
              Just (ln, rn) -> return (ln, rn)
              Nothing -> liftM2 (,) (getUniqueName c) (getUniqueName c)
            (lc, rc) <- case comp of
              Just _  -> do t1 <- (readTable c ln) 
                            t2 <- (readTable c rn)
                            return (toBimap t1, toBimap t2)
              Nothing -> return (Bimap.empty, Bimap.empty)
            (Table _ (sql, h1) r1)  <- readTable c n1
            (Table _ (_, h2) r2) <- readTable c n2
            --if h1 != h2 then error "Append error: Incompatible fields"
            let maxkey = (maxR lc `max` maxR rc) + 1 
            let (r, lc',nextkey) = foldl combine ([], lc, maxkey) r1
            let (r', rc',_) = foldl combine (r, rc, nextkey) r2
            createTable c n (Table n1 (sql, h1) r')
            case comp of  
              Just _ -> do runRaw c ("DELETE FROM " ++ ln) 
                           runRaw c ("DELETE FROM " ++ rn)
                           il <- prepare c $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                           ir <- prepare c $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                           executeMany il $ fromBimap lc'
                           executeMany ir $ fromBimap rc'
              Nothing -> do runRaw c ("CREATE TABLE " ++ ln ++ " (fkey INTEGER)") 
                            runRaw c ("CREATE TABLE " ++ rn ++ " (fkey INTEGER)")
                            il <- prepare c $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                            ir <- prepare c $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                            executeMany il $ fromBimap lc'
                            executeMany ir $ fromBimap rc'
            runRaw c ("DROP TABLE " ++ n1)
            runRaw c ("DROP TABLE " ++ n2)
            return $ Just (ln, rn)
          put comp'
          return c
          where combine (r,m,k) (i:fs) = case Bimap.lookupR (fromSql i) m of
                  Just k' -> (((toSql k'):fs):r,m,k)
                  Nothing -> (((toSql k):fs):r,Bimap.insert k (fromSql i) m, k+1)
                maxR bm = if Bimap.null bm then 0 else fst $ Bimap.findMaxR bm
        pl :: Conn -> StateT (Maybe (Name, Name)) IO Conn
        pl c = do
          comp <- get
          comp' <- lift $ do
            (ln, rn) <- case comp of
              Just (ln, rn) -> return (ln, rn)
              Nothing -> liftM2 (,) (getUniqueName c) (getUniqueName c)
            (lc, rc) <- case comp of
              Just _  -> do t1 <- (readTable c ln) 
                            t2 <- (readTable c rn)
                            return (toBimap t1, toBimap t2)
              Nothing -> return (Bimap.empty, Bimap.empty)
            (Table _ (sql, hs) rs)  <- readTable c n
            let ((r1, r2), (lc', rc'), _) = foldl split (([],[]),(lc,rc),(nextL lc, nextL rc)) rs
            createTable c n1 (Table n (sql, hs) r1)
            createTable c n2 (Table n (sql, hs) r2)
            case comp of  
              Just _ -> do runRaw c ("DELETE FROM " ++ ln) 
                           runRaw c ("DELETE FROM " ++ rn)
                           il <- prepare c $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                           ir <- prepare c $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                           executeMany il $ fromBimap lc'
                           executeMany ir $ fromBimap rc'
              Nothing -> do runRaw c ("CREATE TABLE " ++ ln ++ " (fkey INTEGER)") 
                            runRaw c ("CREATE TABLE " ++ rn ++ " (fkey INTEGER)")
                            il <- prepare c $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                            ir <- prepare c $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                            executeMany il $ fromBimap lc'
                            executeMany ir $ fromBimap rc'                            
            runRaw c ("DROP TABLE " ++ n)
            return $ Just (ln, rn)
          put comp'
          return c
          where split ((r1,r2), (lc, rc), (k1, k2)) (i:fs) = 
                  case (Bimap.lookup i' lc, Bimap.lookup i' rc) of
                    (Just k', _)  -> ((((toSql k'):fs):r1, r2), (lc, rc), (k1, k2))
                    (_, Just k')  -> ((r1, ((toSql k'):fs):r2), (lc, rc), (k1, k2))
                    _ | on fs     -> ((((toSql k1):fs):r1, r2), (Bimap.insert i' k1 lc, rc), (k1 + 1, k2))
                    _ | otherwise -> ((r1, ((toSql k2):fs):r2), (lc, Bimap.insert i' k2 rc), (k1, k2 + 1))
                  where i' = fromSql i
                nextL bm = 1 + if Bimap.null bm then 0 else fst $ Bimap.findMaxR bm
            
            
--          c@(lc,rc) <- get
--          case (Map.lookup n1 d, Map.lookup n2 d) of
--            (Just (Table h1 m1), Just (Table h2 m2)) | h1 == h2  -> put (lc',rc') >> return (Map.insert n (Table h1 m') $ Map.delete n1 $ Map.delete n2 d)
--              where (m', rc', _) = Map.foldlWithKey combine (m, rc, newkey') m2
--                    (m, lc', newkey') = Map.foldlWithKey combine (Map.empty, lc, newkey) m1
--                    combine (m, c, nextkey) k v = 
--                      case Bimap.lookup k c of
--                        Just k' -> (Map.insert k' v m, c, nextkey)
--                        Nothing -> (Map.insert nextkey v m, Bimap.insert k nextkey c, nextkey + 1)
--                    newkey = (maxR lc `max` maxR rc) + 1
--                    maxR bm = if Bimap.null bm then -1 else fst $ Bimap.findMaxR bm
--            _                                                    -> return d
--        pl d = do
--          c@(lc, rc) <- get
--          case Map.lookup n d of
--            Just (Table h m) -> put (lc',rc') >> return (Map.insert n1 (Table h m1) $ Map.insert n2 (Table h m2) $ Map.delete n d)
--              where m1' = Map.mapKeys (fromJust  . flip Bimap.lookupR lc) $ Map.intersection m lcmap
--                    m2' = Map.mapKeys (fromJust  . flip Bimap.lookupR rc) $ Map.intersection m rcmap
--                    (m1,m2,(lc',rc'),_) = Map.foldlWithKey combine (m1',m2',(lc,rc),(next1,next2)) rest
--                    rest = Map.difference (Map.difference m lcmap) rcmap
--                    combine (m1,m2,(lc',rc'),(next1,next2)) k v 
--                      | on k v = (Map.insert next1 v m1, m2, (Bimap.insert next1 k lc', rc'), (next1 + 1, next2))
--                      | otherwise = (m1, Map.insert next2 v m2, (lc', Bimap.insert next2 k rc'), (next1, next2 + 1))
--                    next1 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m1'
--                    next2 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m2'
--                    lcmap = Bimap.toMapR lc 
--                    rcmap = Bimap.toMapR rc
--            _                                                    -> return d

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

-- assumes that this table is not in the database
createTable :: Conn -> Name -> Table -> IO ()
createTable c n (Table n' (h,hs) rs) = do
  ctb' <- ctb
  execute ctb' []
  insert' <- insert
  sequence_ (map (execute insert') rs)
  commit c
  where ctb = prepare c $ copyCreateStatement h n' n
        insert = prepare c $ "INSERT INTO " ++ n ++ "( rowid,"  ++ concat (intersperse "," (map (\(a,_) -> fromSql a) hs)) ++
                                                    ") VALUES (?," ++ concat (intersperse "," (map (\_ -> "?") hs)) ++ " )"
  

readTable :: Conn -> Name -> IO Table
readTable c n = do
  ((h:_):_) <- quickQuery' c "SELECT sql FROM sqlite_master WHERE type=\'table\' AND name=?" [toSql n]
  hs' <- quickQuery' c  ("PRAGMA table_info(" ++ n ++ ")") []
  let hs = map fromTableColumn hs'
  rs <-  quickQuery' c ("SELECT rowid,"  ++ concat (intersperse "," (map (\(a,_) -> fromSql a) hs)) ++ " FROM " ++ n) []
  return (Table n (fromSql h,hs) rs) 

fromTableColumn :: [SqlValue] -> (SqlValue,[SqlValue])
fromTableColumn (_:n:rs) = (n,rs) -- _ is the key of PRAGMA

toTableColumn :: (SqlValue, [SqlValue]) -> String
toTableColumn (v,[dt,isNull,null,pk]) = concat $ intersperse " "
                                          [ fromSql v
                                          , fromSql dt
                                          , if (fromSql isNull == "1") then "NOT NULL" else case null of
                                            SqlNull -> "DEFAULT NULL"
                                            _       -> fromSql null
                                          , if (fromSql pk /= "0") then "PRIMARY KEY" else ""  -- problem for multiple primary keys
                                          ]

copyCreateStatement :: String -> String -> String -> String
copyCreateStatement q from to = replace ("\""++ from) ("\"" ++ to) $ replace ("," ++ from ) ("," ++ to) $ replace (" " ++ from) (" " ++ to) q

copyTableStructure :: Conn -> Name -> Name -> IO ()
copyTableStructure c from to = do
  ((a:_):_) <- quickQuery' c "SELECT sql FROM sqlite_master WHERE type=\'table\' AND name=?" [toSql from]
  runRaw c $ copyCreateStatement (fromSql a) from to
  return ()
