{-# LANGUAGE TypeFamilies #-}

module EditLenses.RelationalAlgebra where

import Prelude hiding (init)
import Control.Monad.State
import Control.Applicative
import Data.List.Utils (delFromAL)
import EditLenses.Types
import EditLenses.Utils
import EditLenses.Translator

class DBEditLens smo where
  type C smo :: *
  init :: smo -> Database -> IO (C smo)
  putr :: Database -> smo -> Edit -> StateT (C smo) IO Edit
  putl :: Database -> smo -> Edit -> StateT (C smo) IO Edit
  translateR :: smo -> C smo -> Query -> Query
  translateL :: smo -> C smo -> Query -> Query
  migrate :: smo -> (C smo) -> Database -> IO ()

instance (DBEditLens a, DBEditLens b) => DBEditLens (Compose a b) where
  type C (Compose a b) = (C a, C b)
  translateR (Compose a b) (c1, c2) q =
    translateR b c2 (translateR a c1 q)
  translateL (Compose a b) (c1, c2) q =
    translateL a c1 (translateL b c2 q)
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
  migrate = undefined

instance DBEditLens CreateTable where
  type C CreateTable = String
  translateR _ _ q = q
  translateL (CreateTable t f) = translateR (DeleteTable t f)
  -- Complement table (name) is the same as the source table (name)
  init e@(CreateTable (Table n _) fs) db = applyEdit db e >> return n
  putr _ _ e = return e
  putl db smo@(CreateTable (Table n _) _) e = case e of
    InsertInto n' _ _ | n == n' -> return IdEdit
    DeleteFrom n' p | n == n' -> return IdEdit
    UpdateWhere n' p fs | n == n' -> return IdEdit
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putl db smo e1) c
      (e2, c'') <- lift $ runStateT (putl db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
  migrate _ _ _ = return ()

instance DBEditLens DeleteTable where
  type C DeleteTable = String
  translateR s@(DeleteTable (Table n _) _) c q = case q of
    SelectQ p q' -> SelectQ p (translateR s c q')
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    TableQ n' | n == n'   -> TableQ c
              | otherwise -> q
  translateL (DeleteTable t f) = translateR (CreateTable t f)
  init (DeleteTable (Table n _) _) _ = return n
  putr db smo@(DeleteTable (Table n _) _) e = case e of
    InsertInto n' _ _ | n == n' -> return IdEdit
    DeleteFrom n' p | n == n' -> return IdEdit
    UpdateWhere n' p fs | n == n' -> return IdEdit
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putr db smo e) c
      (e2, c'') <- lift $ runStateT (putr db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
  putl _ _ e = return e
  migrate = id

instance DBEditLens RenameTable where
  type C RenameTable = ()
  translateR s@(RenameTable n1 n2) c q = case q of
    SelectQ p q' -> SelectQ (rewritePredicate n1 n2 p) (translateR s c q')
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) (rewriteJC n1 n2 jc) n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    TableQ n' | n1 == n'  -> TableQ n2
              | otherwise -> q
  translateL s@(RenameTable n1 n2) = translateR (RenameTable n2 n1)
  init _ _ = return ()
  putr db smo@(RenameTable n1 n2) e = case e of
    InsertInto n fs q | n == n1 -> return $ InsertInto n2 fs $ translateL s q
    DeleteFrom n p | n == n1 -> return $ DeleteFrom n2 p
    UpdateWhere n p fs | n == n1 -> return $ UpdateWhere n2 p fs
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putl db smo e1) c
      (e2, c'') <- lift $ runStateT (putl db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
  putl db smo@(RenameTable n1 n2) e = case e of
    InsertInto n fs q | n == n2 -> return $ InsertInto n1 $ translateL smo q
    DeleteFrom n p | n == n2 -> return $ DeleteFrom n1 p
    UpdateWhere n p fs | n == n2 -> return $ UpdateWhere n1 p fs
    ComposeEdits e1 e2 -> do
      c <- get
      (e1, c') <- lift $ runStateT (putl db smo e1) c
      (e2, c'') <- lift $ runStateT (putl db smo e2) c'
      return $ ComposeEdits e1 e2
    _ -> return e
  migrate = undefined

-- NOTE: ASSUMES THAT INSERTED/DELETED COLUMN IS AT THE BEGINNING
-- THIS IS NOT ROBUST, AND NEEDS TO BE FIXED
instance DBEditLens InsertColumn where
  type C InsertColumn = String
  translateR (InsertColumn _ _ _) c q = q
  translateL (InsertColumn n c v) = translateR (DeleteColumn n c v)
  init (InsertColumn (Table n k) (f,t) v) db = do
    cn <- getUniqueName db
    applyEdit db $ Compose (CopyTable n cn) (InsertColumn (Table cn k) (f,t) v)
    return cn
  putr db smo@(InsertColumn (Table n _) (f,_) v) e = do 
    c <- get
    lift $ applyEdit db $ rewriteEdit n c e
    case e of
      InsertInto n' fs q | n == n' -> return $ InsertInto n (f:fs) undefined
      UpdateWhere n' p fs | n == n' -> return $ UpdateWhere n p ((f,v):fs)
      ComposeEdits e1 e2 -> do
        (e1, c') <- lift $ runStateT (putl db smo e1) c
        (e2, c'') <- lift $ runStateT (putl db smo e2) c'
        return $ ComposeEdits e1 e2
      _ -> return e
  putl db smo@(InsertColumn (Table n _) (f,_) v) e = do 
    c <- get 
    e' <- case e of
      InsertInto n' fs q | n == n' -> return $ InsertInto n undefined undefined
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
  migrate = undefined

instance DBEditLens DeleteColumn where
  type C DeleteColumn = String
  translateR s@(DeleteColumn (Table n _) _ v) c q = case q of
    SelectQ p q' -> SelectQ p (translateR s c q')
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    -- Assumes that the complement is up-to-date. Otherwise, an outerjoin would be needed.
    TableQ n' | n == n'  -> JoinQ (TableQ n) (TableQ c) [(qualifiedKey n, qualifiedKey c)] n
              | otherwise -> q
  translateL (DeleteColumn n c v) = translateR (InsertColumn n c v)
  init (DeleteColumn (Table n _) (f, t) v) db = do
    cn <- getUniqueName db
    
    --createTable db cn [(f, t)]
    --applyEdit db $ "insert into " ++ cn ++ " select rowid, " ++ (getQName f) ++ " from " ++ n
    return cn
  putr db smo@(DeleteColumn (Table n _) (f, _) v) e = do 
    c <- get 
    e' <- case e of
    -- InsertInto n' t | n == n' -> return $ InsertInto n (tail t)
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
  putl db smo@(DeleteColumn (Table n _) (f, _) v) e = do 
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
  migrate = undefined

instance DBEditLens Append where
  type C Append = (String, String)
  translateR s@(Append n1 n2 n _) c@(c1,c2) q = case q of
    SelectQ p q' -> SelectQ p (translateR s c q')
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    -- Assumes that the complement is up-to-date. Otherwise, need to use append predicate.
    TableQ n' | n1 == n'  -> TableQ c1
                             -- RenameQ fkey key $ RenameQ key (Field "foo") $
                             --  JoinQ (TableQ n) (TableQ c1) [(qualifiedKey n1, qualifiedKey c1)] n1
              | n2 == n'  -> TableQ c2
                             -- RenameQ fkey key $ RenameQ key (Field "foo") $
                             --   JoinQ (TableQ n) (TableQ c2) [(qualifiedKey n2, qualifiedKey c2)] n2
              | otherwise -> q
  translateL (Append n1 n2 n p) = translateR (Split n1 n2 n p)
  init = undefined
  putr = undefined
  putl = undefined
  migrate = undefined

instance DBEditLens Split where
  type C Split = (String, String)
  translateR s@(Split n n1 n2 _) c@(c1,c2) q = case q of
    SelectQ p q' -> SelectQ (fmap trans p) (translateR s c q')
        where trans (Field c) | c == key'  = qualifiedFKey n
                              | otherwise = (QField n c)
              trans (QField n' c) | c == key'  = qualifiedFKey n
                                  | otherwise = (QField n c)
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    TableQ n' | n == n'   -> UnionQ (TableQ n1) (TableQ n2) n
                             -- RenameQ fkey key $ UnionQ
                             --   (JoinQ (TableQ n1) (RenameQ key fkey (TableQ c1)) [(qualifiedKey n1, qualifiedKey c1)] n1)
                             --   (JoinQ (TableQ n2) (RenameQ key fkey (TableQ c2)) [(qualifiedKey n2, qualifiedKey c2)] n2) n
              | otherwise -> q
  translateL (Split n1 n2 n p) = translateR (Append n1 n2 n p)
  init = undefined
  putr = undefined
  putl = undefined
  migrate = undefined

instance DBEditLens Join where
  type C Join = (String, String)
  -- For Join, complements are the entire tables)
  translateR s@(Join n1 n2 n jc) c@(c1, c2) q = case q of
    SelectQ p q' -> SelectQ p (translateR s c q')
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    TableQ n' | n1 == n'  -> TableQ c1
              | n2 == n'  -> TableQ c2
              | otherwise -> q
  translateL (Join n1 n2 n jc) = translateR (Decompose n1 n2 n jc)
  init = undefined
  putr = undefined
  putl = undefined
  migrate = undefined

instance DBEditLens Decompose where
  type C Decompose = (String, String)
  translateR s@(Decompose n n1 n2 jc) c@(c1, c2) q = case q of
    SelectQ p q' -> SelectQ p (translateR s c q')
    ProjectQ jc q' -> ProjectQ jc (translateR s c q')
    RenameQ f1 f2 q' -> RenameQ f1 f2 (translateR s c q')
    JoinQ q1 q2 jc n' -> JoinQ (translateR s c q1) (translateR s c q2) jc n'
    UnionQ q1 q2 n' -> UnionQ (translateR s c q1) (translateR s c q2) n'
    TableQ n' | n == n'   -> JoinQ (TableQ c1) (TableQ c2) jc n
              | otherwise -> q
  translateL (Decompose n1 n2 n jc) = translateR (Join n1 n2 n jc)
  init = undefined
  putr = undefined
  putl = undefined
  migrate = undefined

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
