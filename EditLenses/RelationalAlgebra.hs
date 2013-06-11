{-# LANGUAGE TypeFamilies #-}

module EditLenses.RelationalAlgebra where

import Prelude hiding (init)
import Control.Monad.State
import Control.Applicative
import Data.List.Utils (delFromAL)
import EditLenses.Types
import EditLenses.Utils
import EditLenses.Translator

class SMO smo where
  type C smo :: *
  translateQuery :: smo -> C smo -> Query -> Query

instance (SMO a, SMO b) => SMO (Compose a b) where
  type C (Compose a b) = (C a, C b)
  translateQuery (Compose a b) (c1, c2) q =
    translateQuery b c2 (translateQuery a c1 q)

instance SMO CreateTable where
  type C CreateTable = String
  translateQuery _ _ q = q

instance SMO DeleteTable where
  type C DeleteTable = String
  translateQuery s@(DeleteTable n _) c q = case q of
    SelectQ p q' -> SelectQ p (translateQuery s c q')
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    TableQ n' | n == n'   -> TableQ c
              | otherwise -> q

instance SMO RenameTable where
  type C RenameTable = ()
  translateQuery s@(RenameTable n1 n2) c q = case q of
    SelectQ p q' -> SelectQ (rewritePredicate n1 n2 p) (translateQuery s c q')
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) (rewriteJC n1 n2 jc) n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    TableQ n' | n1 == n'  -> TableQ n2
              | otherwise -> q

instance SMO InsertColumn where
  type C InsertColumn = String
  translateQuery (InsertColumn _ _ _) c q = q

instance SMO DeleteColumn where
  type C DeleteColumn = String
  translateQuery s@(DeleteColumn n _ v) c q = case q of
    SelectQ p q' -> SelectQ p (translateQuery s c q')
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    -- Assumes that the complement is up-to-date. Otherwise, an outerjoin would be needed.
    TableQ n' | n == n'  -> JoinQ (TableQ n) (TableQ c) [(qualifiedKey n, qualifiedKey c)] n
              | otherwise -> q

instance SMO Append where
  type C Append = (String, String)
  translateQuery s@(Append n1 n2 n _) c@(c1,c2) q = case q of
    SelectQ p q' -> SelectQ p (translateQuery s c q')
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    -- Assumes that the complement is up-to-date. Otherwise, need to use append predicate.
    TableQ n' | n1 == n'  -> TableQ c1
                             -- RenameQ fkey key $ RenameQ key (Field "foo") $
                             --  JoinQ (TableQ n) (TableQ c1) [(qualifiedKey n1, qualifiedKey c1)] n1
              | n2 == n'  -> TableQ c2
                             -- RenameQ fkey key $ RenameQ key (Field "foo") $
                             --   JoinQ (TableQ n) (TableQ c2) [(qualifiedKey n2, qualifiedKey c2)] n2
              | otherwise -> q

instance SMO Split where
  type C Split = (String, String)
  translateQuery s@(Split n n1 n2 _) c@(c1,c2) q = case q of
    SelectQ p q' -> SelectQ (fmap trans p) (translateQuery s c q')
        where trans (Field c) | c == key'  = qualifiedFKey n
                              | otherwise = (QField n c)
              trans (QField n' c) | c == key'  = qualifiedFKey n
                                  | otherwise = (QField n c)
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    TableQ n' | n == n'   -> UnionQ (TableQ n1) (TableQ n2) n
                             -- RenameQ fkey key $ UnionQ
                             --   (JoinQ (TableQ n1) (RenameQ key fkey (TableQ c1)) [(qualifiedKey n1, qualifiedKey c1)] n1)
                             --   (JoinQ (TableQ n2) (RenameQ key fkey (TableQ c2)) [(qualifiedKey n2, qualifiedKey c2)] n2) n
              | otherwise -> q

instance SMO Join where
  type C Join = (String, String)
  -- For Join, complements are the entire tables)
  translateQuery s@(Join n1 n2 n jc) c@(c1, c2) q = case q of
    SelectQ p q' -> SelectQ p (translateQuery s c q')
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    TableQ n' | n1 == n'  -> TableQ c1
              | n2 == n'  -> TableQ c2
              | otherwise -> q

instance SMO Decompose where
  type C Decompose = (String, String)
  translateQuery s@(Decompose n n1 n2 jc) c@(c1, c2) q = case q of
    SelectQ p q' -> SelectQ p (translateQuery s c q')
    ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
    TableQ n' | n == n'   -> JoinQ (TableQ c1) (TableQ c2) jc n
              | otherwise -> q

class (SMO smo) => DBEditLens smo where
  init :: smo -> Database -> IO (C smo)
  putr :: Database -> smo -> Edit -> StateT (C smo) IO Edit
  putl :: Database -> smo -> Edit -> StateT (C smo) IO Edit
  migrate :: smo -> (C smo) -> Database -> IO ()
  
instance (DBEditLens a, DBEditLens b) => DBEditLens (Compose a b) where
  init (Compose a b) db = do
    c1 <- init a db
    c2 <- init b db
    return (c1,c2)
  putr db (Compose a b) e = do
    (c1, c2) <- get
    (e', c1') <- lift $ runStateT (putr db a e) c1
    (e'', c2') <- lift $ runStateT (putr db b e') c2
    put (c1', c2')
    return e''
  putl db (Compose a b) e = do
    (c1, c2) <- get
    (e', c2') <- lift $ runStateT (putl db b e) c2
    (e'', c1') <- lift $ runStateT (putl db a e') c1
    put (c1', c2')
    return e''

instance DBEditLens CreateTable where
  -- Complement table (name) is the same as the source table (name)
  init e@(CreateTable n fs) db = applyEdit db e >> return n
  putr _ _ e = return e
  putl db smo@(CreateTable n _) e = case e of
    InsertInto n' t | n == n' -> return IdEdit
    DeleteFrom n' p | n == n' -> return IdEdit
    UpdateWhere n' p fs | n == n' -> return IdEdit
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putl db smo e1) c
      (e2, c'') <- lift $ runStateT (putl db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
    
instance DBEditLens DeleteTable where
  init (DeleteTable n _) _ = return n
  putr db smo@(DeleteTable n _) e = case e of
    InsertInto n' t | n == n' -> return IdEdit
    DeleteFrom n' p | n == n' -> return IdEdit
    UpdateWhere n' p fs | n == n' -> return IdEdit
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putr db smo e) c
      (e2, c'') <- lift $ runStateT (putr db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
  putl _ _ e = return e
  
instance DBEditLens RenameTable where
  init _ _ = return ()
  putr db smo@(RenameTable n1 n2) e = case e of
    InsertInto n t | n == n1 -> return $ InsertInto n2 t
    DeleteFrom n p | n == n1 -> return $ DeleteFrom n2 p
    UpdateWhere n p fs | n == n1 -> return $ UpdateWhere n2 p fs
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putl db smo e1) c
      (e2, c'') <- lift $ runStateT (putl db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
  putl db smo@(RenameTable n1 n2) e = case e of
    InsertInto n t | n == n2 -> return $ InsertInto n1 t
    DeleteFrom n p | n == n2 -> return $ DeleteFrom n1 p
    UpdateWhere n p fs | n == n2 -> return $ UpdateWhere n1 p fs
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putl db smo e1) c
      (e2, c'') <- lift $ runStateT (putl db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e

-- NOTE: ASSUMES THAT INSERTED/DELETED COLUMN IS AT THE BEGINNING
-- THIS IS NOT ROBUST, AND NEEDS TO BE FIXED
instance DBEditLens InsertColumn where
  init (InsertColumn n (f,t) v) db = do
    cn <- getUniqueName db
    applyEdit db $ Compose (CopyTable n cn) (InsertColumn cn (f,t) v)
    return cn
  putr db smo@(InsertColumn n (f,_) v) e = do 
    c <- get
    lift $ applyEdit db $ rewriteEdit n c e
    case e of
      InsertInto n' t | n == n' -> return $ InsertInto n (v:t)
      UpdateWhere n' p fs | n == n' -> return $ UpdateWhere n p ((f,v):fs)
      ComposeEdits e1 e2 -> do
        (e1, c') <- lift $ runStateT (putl db smo e1) c
        (e2, c'') <- lift $ runStateT (putl db smo e2) c'
        return $ ComposeEdits e1 e2
      _ -> return e
  putl db smo@(InsertColumn n (f,_) v) e = do 
    c <- get 
    e' <- case e of
      InsertInto n' t | n == n' -> return $ InsertInto n (tail t)
--      UpdateWhere n' p fs | n == n' -> do
--        rn <- getUniqueName
--        applyEdit db $ undefined -- Insert Into 
--        return $ UpdateWhere TODO p (delFromAL fs f)
    -- DeleteFrom n' p | n == n' -> return $ DeleteFrom TODO p
      ComposeEdits e1 e2 -> do
        (e1, c') <- lift $ runStateT (putl db smo e1) c
        (e2, c'') <- lift $ runStateT (putl db smo e2) c'
        put c''
        return $ ComposeEdits e1 e2
      _ -> return e
    lift $ applyEdit db $ rewriteEdit n c e'
    return e'

instance DBEditLens DeleteColumn where
  init (DeleteColumn n (f, t) v) db = do
    cn <- getUniqueName db
    --createTable db cn [(f, t)]
    --applyEdit db $ "insert into " ++ cn ++ " select rowid, " ++ (getQName f) ++ " from " ++ n
    return cn
  putr db smo@(DeleteColumn n (f, _) v) e = do 
    c <- get 
    e' <- case e of
      InsertInto n' t | n == n' -> return $ InsertInto n (tail t)
    -- UpdateWhere n' p fs | n == n' -> return $ UpdateWhere TODO p (delFromAL fs f)
    -- DeleteFrom n' p | n == n' -> return $ DeleteFrom TODO p
      ComposeEdits e1 e2 -> do
        (e1, c') <- lift $ runStateT (putl db smo e1) c
        (e2, c'') <- lift $ runStateT (putl db smo e2) c'
        put c''
        return $ ComposeEdits e1 e2
      _ -> return e
    lift $ applyEdit db $ rewriteEdit n c e'
    return e'
  putl db smo@(DeleteColumn n (f, _) v) e = do 
    c <- get
    lift $ applyEdit db $ rewriteEdit n c e
    case e of
      InsertInto n' t | n == n' -> return $ InsertInto n (v:t)
      UpdateWhere n' p fs | n == n' -> return $ UpdateWhere n p ((f,v):fs)
      ComposeEdits e1 e2 -> do
        (e1, c') <- lift $ runStateT (putl db smo e1) c
        (e2, c'') <- lift $ runStateT (putl db smo e2) c'
        return $ ComposeEdits e1 e2
      _ -> return e

--instance DBEditLens Append where
--  init (Append n1 n2 n p) db = do
--    cn1 <- getUniqueName db
--    cn2 <- getUniqueName db
--    -- create table with fields of n1 + extra fields for key
--    -- create table with fields of n2 + extra fields for key
--    -- applyEdit db $ "insert into " ++ cn1 ++ ...
--    -- applyEdit db $ "insert into " ++ cn ++ ...
--    return (cn1, cn2)
--  putr db (Append n1 n2 n p) e = do
--    (c1, c2, k) <- get
--    case e of
--      InsertInto n' t | n' == n1 -> 
--        lift $ applyEdit db $ InsertInto c1 (k:t)
--        put (c1, c2, k+1)
--        return $ InsertInto n undefined -- (replace key with k)
--      InsertInto n' t | n' == n2 -> 
--        lift $ applyEdit db $ InsertInto c2 (k:t)
--        put (c1, c2, k+1)
--        return $ InsertInto n undefined -- (replace key with k)
--      DeleteFrom n' p | n' == n1 (* && rowid notin p *) -> do
--        lift $ applyEdit db $ DeleteFrom c1 (fmap _ p)
--        return $ DeleteFrom n p
--      DeleteFrom n' p | n' == n2 (* && rowid notin p *) -> do
--        lift $ applyEdit db $ DeleteFrom c2 (fmap _ p)
--        return $ DeleteFrom n p
--      DeleteFrom n' p | n' == n1 -> do
--        let p1, p2 = splitPredicate p rowid
--        let rs = runQuery db $ Project fkey $ Select c1 p2   
--        lift $ applyEdit db $ DeleteFrom c1 (fmap _ p)
--        return $ DeleteFrom n (AndP p1 (In key rs))
--      DeleteFrom n' p | n' == n2 -> do
--        let p1, p2 = splitPredicate p rowid
--        let rs = runQuery db $ Project fkey $ Select c2 p2   
--        lift $ applyEdit db $ DeleteFrom c2 (fmap _ p)
--        return $ DeleteFrom n (AndP p1 (In key rs))
--      UpdateWhere n' p fs | n' == n1 (* and key not in fs *) -> do
--        let p1, p2 = splitPredicate p rowid
--        let rs = runQuery fb $ Project fkey $ select c1 p2
--        lift $ applyEdit db $ DeleteFrom c1 (fmap _ p)
--        return $ UpdateWhere n (AndP p1 (In key rs)) fs
--      UpdateWhere n' p fs | n' == n2 (* and key not in fs *) -> do
--        let p1, p2 = splitPredicate p rowid
--        let rs = runQuery fb $ Project fkey $ select c1 p2
--        lift $ applyEdit db $ DeleteFrom c1 (fmap _ p)
--        return $ UpdateWhere n (AndP p1 (In key rs)) fs
--      ComposeEdits e1 e2 -> do 
--        (e1, c') <- lift $ runStateT (putl db smo e1) (c1, c2, k)
--        (e2, c'') <- lift $ runStateT (putl db smo e2) c'
--        return $ ComposeEdits e1 e2
--      _ -> return e
        

