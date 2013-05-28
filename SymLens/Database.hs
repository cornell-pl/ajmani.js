{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module SymLens.Database where

import SymLens 
import Database.Memory
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe,fromJust,maybe)

import qualified Data.Tuple as T

type TableLens = SymLens Table Table
type DatabaseLens = SymLens Database Database

liftTableLens :: Name -> TableLens -> DatabaseLens
liftTableLens n (SymLens s pr pl) = SymLens s (doit pr) (doit pl)
  where doit put d c = T.swap $ Map.mapAccumWithKey onT c d
          where onT c n' t | n == n'   = T.swap $ put t c 
                           | otherwise = (c,t) 
         
rename :: Name -> Name -> DatabaseLens
rename n1 n2 = SymLens () put put
  where sigma n | n == n1   = n2
                | n == n2   = n1
                | otherwise = n
        put d () = (Map.mapKeys sigma d, ())

drop :: Name -> DatabaseLens
drop n = SymLens Nothing pr pl
  where pr d c = maybe (d,c) trans $ Map.lookup n d
          where trans t' = (Map.delete n d, Just t')
        pl d Nothing    = (d, Nothing)
        pl d c@(Just v) = (Map.insert n v d,c) 

-- Can this be implemented using drop ??
-- Keep track of added and deleted records while coming from right to left.
insert :: Name -> Table -> DatabaseLens
insert n t@(Table _ m) = SymLens Nothing pr pl
  where pr d Nothing     = (Map.insert n t d, Just t)
        pr d c@(Just t') = (Map.insert n t' d, c) 
        pl d c = maybe (d,c) trans $ Map.lookup n d
         where trans t' = (Map.delete n d, Just t') 

-- Take a pred depending on which the new records will go to n1 or n2.
-- putr :: Database with (n1,n2) and without n -> Database with n and without (n1,n2) 
-- putl :: Database with n and without (n1,n2) -> Database with (n1,n2) and without n


-- Compelement is a pair of maps mapping keys from the the two initial tables to the appended table 
append :: (Id -> Fields -> Bool) 
       -> Name 
       -> Name 
       -> Name 
       -> DatabaseLens
append on n1 n2 n = SymLens (Bimap.empty, Bimap.empty) pr pl
  where pr d c@(lc,rc) = case (Map.lookup n1 d, Map.lookup n2 d) of
          (Just (Table h1 m1), Just (Table h2 m2)) | h1 == h2  -> (Map.insert n (Table h1 m') $ Map.delete n1 $ Map.delete n2 d, (lc', rc'))
            where (m', rc', _) = Map.foldlWithKey combine (m, rc, newkey') m2
                  (m, lc', newkey') = Map.foldlWithKey combine (Map.empty, lc, newkey) m1
                  combine (m, c, nextkey) k v = 
                    case Bimap.lookup k c of
                      Just k' -> (Map.insert k' v m, c, nextkey)
                      Nothing -> (Map.insert nextkey v m, Bimap.insert k nextkey c, nextkey + 1)
                  newkey = (maxR lc `max` maxR rc) + 1
                  maxR bm = if Bimap.null bm then -1 else fst $ Bimap.findMaxR bm
          _                                                    -> (d,c)
        pl d c@(lc, rc) = case Map.lookup n d of
          Just (Table h m) -> (Map.insert n1 (Table h m1) $ Map.insert n2 (Table h m2) $ Map.delete n d, (lc', rc'))
            where m1' = Map.mapKeys (fromJust  . flip Bimap.lookupR lc) $ Map.intersection m lcmap
                  m2' = Map.mapKeys (fromJust  . flip Bimap.lookupR rc) $ Map.intersection m rcmap
                  (m1,m2,(lc',rc'),_) = Map.foldlWithKey combine (m1',m2',(lc,rc),(next1,next2)) rest
                  rest = Map.difference (Map.difference m lcmap) rcmap
                  combine (m1,m2,(lc',rc'),(next1,next2)) k v 
                    | on k v = (Map.insert next1 v m1, m2, (Bimap.insert next1 k lc', rc'), (next1 + 1, next2))
                    | otherwise = (m1, Map.insert next2 v m2, (lc', Bimap.insert next2 k rc'), (next1, next2 + 1))
                  next1 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m1'
                  next2 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m2'
                  lcmap = Bimap.toMapR lc 
                  rcmap = Bimap.toMapR rc
          _                                                    -> (d,c)

{-        pl d c@(lc, rc) = case Map.lookup n d of
          Just (Table h m) -> (Map.insert n1 (Table h m1) $ Map.insert n2 (Table h m2) $ Map.delete n d, (lc', rc'))
            where m1' = Map.mapKeys (fromJust  . flip Map.lookup lc) $ Map.intersection m lc
                  m2' = Map.mapKeys (fromJust  . flip Map.lookup rc) $ Map.intersection m rc
                  (m1,m2,(lc',rc'),_) 
                     = Map.foldlWithKey combine (m1',m2',(lc,rc),(next1,next2)) 
                         $ rest
                  rest = Map.difference (Map.difference m lc) rc
                  combine (m1,m2,(lc',rc'),(next1,next2)) k v 
                    | on k v = (Map.insert next1 v m1, m2, (Map.insert k next1 lc', rc'), (next1 + 1, next2))
                    | otherwise = (m1, Map.insert next2 v m2, (lc', Map.insert k next2 rc'), (next1, next2 + 1))
                  next1 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m1'
                  next2 = maybe 0 ((+1) . fst . fst) $ Map.maxViewWithKey m2'
                  
          _                                                    -> (d,c)
-}        
-- | Takes a function to filter the table on and then splits the table
-- into two tables, first satisfying the predicate and other the rest.
-- I doubt this is not a lens. Consider t split on f to t1 and t2. Now
-- we insert a record r to t1 satisying ~f. Then the lens laws will be
-- broken.  Maintain two tables of additional records added and apply
-- f if r is not in these table otherwise just split based on r
-- belongs to which table.
split :: (Id -> Fields -> Bool) -> Name -> Name -> Name -> DatabaseLens
split on n n1 n2 = inv $ append on n1 n2 n
  
