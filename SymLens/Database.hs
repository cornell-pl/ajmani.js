{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module SymLens.Database where

import SymLens
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe,fromJust,maybe)
import Control.Monad.State
import Data.List
import Data.List.Split
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

return' :: (Monad (t IO), MonadTrans t, IConnection b) 
        => b -> t IO b
return' c = lift (commit c) >> return c

type Name = String
type Header = (SqlValue, [SqlValue])
data Table = Table Name                -- Name from which this table is read
                   (String,[Header])   -- first stores the create statement as it is and second stores the exact header names along with some other info received from PRAGMA table_info
                   [[SqlValue]]        -- first value is the rowid for each record
                   deriving (Show)
rename :: Conn -> Name -> Name -> DatabaseLens
rename _ n1 n2 = SymLens () put put
  where sigma n | n == n1   = n2
                | n == n2   = n1
                | otherwise = n
        put c = do
          ts <- lift $ getTables c
          lift (renameTb ts) >> return' c
            where
              renameTb ts | elem n1 ts && elem n2 ts = do
                n' <- getUniqueName c
                renameTable c n1 n'
                renameTable c n2 n1
                renameTable c n' n2
                             | elem n1 ts =
                renameTable c n1 n2
                             | elem n2 ts =
                renameTable c n2 n1
                             | otherwise = return ()

drop :: Conn -> Name -> DatabaseLens
drop compConn n = SymLens Nothing pr pl
  where pr c = do
          ts <- lift $ getTables c
          lift dropT >>= put
          lift $ commit c
          return' c
          where dropT = do
                  n' <- getUniqueName compConn
                  t <- readTable c n
                  createTable compConn n' t
                  dropTable c n
                  return (Just n')
        pl c = do
          (Just n') <- get
          lift (undropTable n') >> return' c
          where undropTable n' = do
                  t <- readTable compConn n'
                  createTable c n t
                  dropTable compConn n'

-- This does not satisfy the lens laws completely.
-- It does so modulo equality of the temp table's name.
-- However, it does satisfy equality of the contents of the tables.

insert :: Conn -> Name -> Table -> DatabaseLens
insert compConn n t = SymLens Nothing pr pl
   where pr c = do
           tn' <- get
           lift $ case tn' of
                    Just n' -> do
                      tb <- readTable compConn n'
                      createTable c n tb
                      dropTable compConn n'
                    Nothing -> createTable c n t
           return' c
         pl c = do lift doInsertTable >>= put
                   return' c
           where doInsertTable = do
                   n' <- getUniqueName compConn
                   t <- readTable c n
                   createTable compConn n' t
                   dropTable c n
                   return (Just n')

-- -- -- Take a pred depending on which the new records will go to n1 or n2.
-- -- -- putr :: Database with (n1,n2) and without n -> Database with n and without (n1,n2)
-- -- -- putl :: Database with n and without (n1,n2) -> Database with (n1,n2) and without n

-- -- Complement is a pair of maps mapping keys from the the two initial tables to the appended table
-- Complement is a pair of tables mapping keys from the two initial tables to those of the appended table.
-- String argument is an optional SQL predicate on the schema of the target (appended) table.

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
 
append :: Conn
       -> ([SqlValue] -> Bool) 
       -> Name 
       -> Name 
       -> Name 
       -> DatabaseLens
append compConn on n1 n2 n = SymLens Nothing pr pl
  where pr :: Conn -> StateT (Maybe (Name, Name)) IO Conn
        pr c = do
          comp <- get
          comp' <- lift $ do
            (ln, rn) <- case comp of
              Just (ln, rn) -> return (ln, rn)
              Nothing -> liftM2 (,) (getUniqueName compConn) (getUniqueName compConn)
            (lc, rc) <- case comp of
              Just _  -> do t1 <- (readTable compConn ln) 
                            t2 <- (readTable compConn rn)
                            return (toBimap t1, toBimap t2)
              Nothing -> return (Bimap.empty, Bimap.empty)
            (Table _ (sql, h1) r1)  <- readTable c n1
            (Table _ (_, h2) r2) <- readTable c n2
            if h1 /= h2 then error "Append error: Incompatible Headers" else return ()
            let maxkey = (maxL lc `max` maxL rc) + 1
            let (r, lc',nextkey) = foldl combine ([], lc, maxkey) r1
            let (r', rc',_) = foldl combine (r, rc, nextkey) r2
            createTable c n (Table n1 (sql, h1) r')
            case comp of  
              Just _ -> do runRaw compConn ("DELETE FROM " ++ ln) 
                           runRaw compConn ("DELETE FROM " ++ rn)
                           il <- prepare compConn $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                           ir <- prepare compConn $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                           executeMany il $ fromBimap lc'
                           executeMany ir $ fromBimap rc'
              Nothing -> do runRaw compConn ("CREATE TABLE " ++ ln ++ " (fkey INTEGER)") 
                            runRaw compConn ("CREATE TABLE " ++ rn ++ " (fkey INTEGER)")
                            il <- prepare compConn $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                            ir <- prepare compConn $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                            executeMany il $ fromBimap lc'
                            executeMany ir $ fromBimap rc'
            commit compConn
            runRaw c ("DROP TABLE " ++ n1)
            runRaw c ("DROP TABLE " ++ n2)
            return $ Just (ln, rn)
          put comp'
          return' c
          where combine (r,m,k) (i:fs) = case Bimap.lookupR (fromSql i) m of
                  Just k' -> (((toSql k'):fs):r,m,k)
                  Nothing -> (((toSql k):fs):r,Bimap.insert k (fromSql i) m, k+1)
                maxL bm = if Bimap.null bm then 0 else fst $ Bimap.findMax bm
        pl :: Conn -> StateT (Maybe (Name, Name)) IO Conn
        pl c = do
          comp <- get
          comp' <- lift $ do
            (ln, rn) <- case comp of
              Just (ln, rn) -> return (ln, rn)
              Nothing -> liftM2 (,) (getUniqueName compConn) (getUniqueName compConn)
            (lc, rc) <- case comp of
              Just _  -> do t1 <- readTable compConn ln
                            t2 <- readTable compConn rn
                            return (toBimap t1, toBimap t2)
              Nothing -> return (Bimap.empty, Bimap.empty)
            (Table _ (sql, hs) rs)  <- readTable c n
            let ((r1, r2), (lc', rc'), _) = foldl split (([],[]),(lc,rc),(nextR lc, nextR rc)) rs
            createTable c n1 (Table n (sql, hs) r1)
            createTable c n2 (Table n (sql, hs) r2)
            case comp of  
              Just _ -> do runRaw compConn ("DELETE FROM " ++ ln) 
                           runRaw compConn ("DELETE FROM " ++ rn)
                           il <- prepare compConn $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                           ir <- prepare compConn $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                           executeMany il $ fromBimap lc'
                           executeMany ir $ fromBimap rc'
              Nothing -> do runRaw compConn ("CREATE TABLE " ++ ln ++ " (fkey INTEGER)") 
                            runRaw compConn ("CREATE TABLE " ++ rn ++ " (fkey INTEGER)")
                            il <- prepare compConn $ "INSERT INTO " ++ ln ++ "(rowid, fkey) VALUES (?,?)"
                            ir <- prepare compConn $ "INSERT INTO " ++ rn ++ "(rowid, fkey) VALUES (?,?)"
                            executeMany il $ fromBimap lc'
                            executeMany ir $ fromBimap rc'                            
            commit compConn
            runRaw c ("DROP TABLE " ++ n)
            return $ Just (ln, rn)
          put comp'
          return' c
          where split ((r1,r2), (lc, rc), (k1, k2)) (i:fs) = 
                  case (Bimap.lookup i' lc, Bimap.lookup i' rc) of
                    (Just k', _)  -> ((((toSql k'):fs):r1, r2), (lc, rc), (k1, k2))
                    (_, Just k')  -> ((r1, ((toSql k'):fs):r2), (lc, rc), (k1, k2))
                    _ | on fs     -> ((((toSql k1):fs):r1, r2), (Bimap.insert i' k1 lc, rc), (k1 + 1, k2))
                    _ | otherwise -> ((r1, ((toSql k2):fs):r2), (lc, Bimap.insert i' k2 rc), (k1, k2 + 1))
                  where i' = fromSql i
                nextR bm = 1 + if Bimap.null bm then 0 else fst $ Bimap.findMaxR bm
            
 -- | Takes a function to filter the table on and then splits the table
 -- into two tables, first satisfying the predicate and other the rest.
 -- I doubt this is not a lens. Consider t split on f to t1 and t2. Now
 -- we insert a record r to t1 satisying ~f. Then the lens laws will be
 -- broken.  Maintain two tables of additional records added and apply
 -- f if r is not in these table otherwise just split based on r
 -- belongs to which table.
split :: Conn
       -> ([SqlValue] -> Bool) 
       -> Name 
       -> Name 
       -> Name 
       -> DatabaseLens
split c on n n1 n2 = inv $ append c on n1 n2 n

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
  (Table _ (h,hs) _) <- readTableStructure c n
  rs <-  quickQuery c ("SELECT rowid,"  ++ concat (intersperse "," (map (\(a,_) -> fromSql a) hs)) ++ " FROM " ++ n) []
  return (Table n (h,hs) rs)

readTableStructure :: Conn -> Name -> IO Table
readTableStructure c n = do
  ((h:_):_) <- quickQuery' c "SELECT sql FROM sqlite_master WHERE type=\'table\' AND name=?" [toSql n]
  hs' <- quickQuery' c  ("PRAGMA table_info(" ++ n ++ ")") []
  let hs = map fromTableColumn hs'
  return (Table n (fromSql h,hs) [[]])

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

getColumns :: String -> [String]
getColumns h =
  let ls = map (dropWhile (flip elem [' ','\t','\n','\r'])) . wordsBy (==',') . Prelude.drop 1 . dropWhile (/='(') $ h
   in init ls ++ [reverse . Prelude.drop 1 . dropWhile (/=')') . reverse $ last ls]
   
makeCreateStatement :: Name -> [String] -> String
makeCreateStatement tname coldesc = "CREATE TABLE " ++ tname  ++ " (" ++ intercalate "," coldesc ++ ")" 

makeCreateExistsStatement :: Name -> [String] -> String
makeCreateExistsStatement tname coldesc = "CREATE TABLE IF NOT EXISTS " ++ tname  ++ " (" ++ intercalate "," coldesc ++ ")" 


removeColumnName :: [String] -> String -> (String, [String])
removeColumnName cols colName = (head match, nmatch) 
  where (match, nmatch)       = partition (checkColumn colName) $ cols
        checkColumn colName s =  head (words s) == colName
        
dropColumn :: Conn -> Name -> Name -> Name -> IO ()
dropColumn c from to colName = do
  (Table _ (h,_) _) <- readTableStructure c from
  let nls = filter (not . checkColumn colName) $ getColumns h
  let createStmt = makeCreateStatement to $ nls
  let colNames = map (head . words) nls
  let insertStmt = "INSERT INTO " ++ to ++ " SELECT " ++ intercalate "," colNames ++ " FROM " ++ from
  runRaw c createStmt
  runRaw c insertStmt
  where checkColumn colName s = head (words s) == colName
  
dropColumnTableStructure :: Conn -> Name -> Name -> Name -> IO ()
dropColumnTableStructure c from to colName = do
  (Table _ (h,_) _) <- readTableStructure c from
  let nls = getColumns h
  let q = makeCreateStatement to $ filter (not . checkColumn colName) nls
  print q
  where checkColumn colName s = head (words s) == colName
  

dropTable :: Conn -> Name -> IO ()
dropTable c n = runRaw c $ "DROP TABLE " ++ n

renameTable :: Conn -> Name -> Name -> IO ()
renameTable c from to = runRaw c $ "ALTER TABLE " ++ from ++ " RENAME TO " ++ to

  
