{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}


module EditLenses.RelationalAlgebra where

import Prelude hiding (init)
import Control.Monad.State
import Control.Applicative
import Data.List.Utils (delFromAL)

data Database

--data EditLens da db c = EditLens { init :: c,
--                                   putr :: da -> StateT c IO db,
--                                   putl :: db -> StateT c IO da }
--
--type DBEditLens da c = EditLens da da c
--
--data DBEditLens la = forall c. (Eq c, Show c)
--                     => DBEditLens { init :: c,
--                                     putr :: la -> StateT c IO la,
--                                     putl :: la -> StateT c IO la }

--inv :: EditLens da db c -> EditLens db da c
--inv (EditLens def pr pl) = EditLens def pl pr

data CreateTable = CreateTable Name [(Field, Type)]
data DeleteTable = DeleteTable Name [(Field, Type)]
data RenameTable = RenameTable Name Name
data InsertColumn = InsertColumn Name Field Value
data DeleteColumn = DeleteColumn Name Field Value
data Append = Append Name Name Name Predicate
data Split = Split Name Name Name Predicate
data Join = Join Name Name Name JoinCondition
data Decompose = Decompose Name Name Name JoinCondition
data Compose a b = Compose a b

-- Not SMO's but help tp abstract out certain operations
data CopyTable = CopyTable Name Name

class SMO smo where
  type C smo :: *
  translateQuery :: smo -> C smo -> Query -> Query

instance (SMO a, SMO b) => SMO (Compose a b) where
  type C (Compose a b) = (C a, C b)
  translateQuery (Compose a b) (c1, c2) q =
    translateQuery b c2 (translateQuery a c1 q)

instance SMO CreateTable where
  type C CreateTable = ()
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
  translateQuery s@(DeleteColumn n f v) c q = case q of
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

instance (DBEditLens a, DBEditLens b) => DBEditLens (Compose a b) where
  init (Compose a b) =
    \d -> do c1 <- init a d
             c2 <- init b d
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
  init _ _ = return ()
  putr _ _  e = return e
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

--instance DBEditLens DeleteTable where
--  init (DeleteTable n _) _ = return n
--  putr smo@(DeleteTable n _) e c = case e of
--    InsertInto n' t | n == n' -> (IdEdit, c)
--    DeleteFrom n' p | n == n' -> (IdEdit, c)
--    UpdateWhere n' p fs | n == n' -> (IdEdit, c)
--    ComposeEdits e1 e2 -> let (e1, c') = putr smo e1 c in
--                          let (e2, c'') = putr smo e2 c' in
--                          (ComposeEdits e1 e2, c'')
--    _ -> (e,c)
--  putl _  e c = (e,c)

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
  init (InsertColumn n f v) db = do
    cn <- getUniqueName db
    --createTable db cn [f]
    --applyEdit db $ "insert into " ++ cn ++ " select rowid, '" ++ v ++ "' from " ++ n
    return cn
  putr db smo@(InsertColumn n f v) e = do
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
  putl db smo@(InsertColumn n f v) e = do
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

instance DBEditLens DeleteColumn where
  init (DeleteColumn n f v) db = do
    cn <- getUniqueName db
    --createTable db cn [f]
    --applyEdit db $ "insert into " ++ cn ++ " select rowid, " ++ (getQName f) ++ " from " ++ n
    return cn
  putr db smo@(DeleteColumn n f v) e = do
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
  putl db smo@(DeleteColumn n f v) e = do
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

-- instance DBEditLens Append where
--   init (Append n1 n2 n p) db = do
--     cn1 <- getUniqueName db
--     cn2 <- getUniqueName db
--     -- create table with fields of n1 + extra fields for key
--     -- create table with fields of n2 + extra fields for key
--     -- applyEdit db $ "insert into " ++ cn1 ++ ...
--     -- applyEdit db $ "insert into " ++ cn ++ ...
--     return (cn1, cn2)
--   putr db (Append n1 n2 n p) e = do
--     (c1, c2, k) <- get
--     case e of
--       InsertInto n' t | n' == n1 ->
--         lift $ applyEdit db $ InsertInto c1 (k:t)
--         put (c1, c2, k+1)
--         return $ InsertInto n undefined -- (replace key with k)
--       InsertInto n' t | n' == n2 ->
--         lift $ applyEdit db $ InsertInto c2 (k:t)
--         put (c1, c2, k+1)
--         return $ InsertInto n undefined -- (replace key with k)
--       DeleteFrom n' p | n' == n1 (* && rowid notin p *) -> do
--         lift $ applyEdit db $ DeleteFrom c1 (fmap _ p)
--         return $ DeleteFrom n p
--       DeleteFrom n' p | n' == n2 (* && rowid notin p *) -> do
--         lift $ applyEdit db $ DeleteFrom c2 (fmap _ p)
--         return $ DeleteFrom n p
--       DeleteFrom n' p | n' == n1 -> do
--         let p1, p2 = splitPredicate p "rowid"
--         let rs = run $
--         lift $ applyEdit db $ DeleteFrom c2 (fmap _ p)
--         return $ DeleteFrom n p


class EditLanguage a l where
  applyEdit :: l -> a -> IO a

instance EditLanguage Edit Database where
  applyEdit = undefined

--instance EditLanguage Database SMO where
--  apply = undefined
--
type Name = String
data Field =
    Field Name
  | QField Name Name
  deriving (Show, Eq)

type Type = String
type Value = String
type Fields = [Field]
type Tuple = [Value]

key' :: Name
key' = "rowid"
fkey' :: Name
fkey' = "fkey"
key :: Field
key = Field key'
fkey :: Field
fkey = Field fkey'
split' :: Name
split' = "split"
split = Field split'
qualifiedKey :: Name -> Field
qualifiedKey n = QField n key'
qualifiedFKey :: Name -> Field
qualifiedFKey n = QField n fkey'

getName :: Field -> Name
getName (Field fn) = fn
getName (QField tn fn) = fn

getQName :: Name -> Field -> Name
getQName tn = maybeGetQName . (qualifyField tn)

maybeGetQName :: Field -> Name
maybeGetQName (Field fn) = fn
maybeGetQName (QField tn fn) = tn ++ "." ++ fn

qualifyField :: Name -> Field -> Field
qualifyField tn (Field fn) = QField tn fn
qualifyField tn (QField _ fn) = QField tn fn

unQualifyField :: Field -> Field
unQualifyField (QField _ fn) = Field fn
unQualifyField f = f

data Binop = Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show, Eq)

type Predicate = Pred Field
data Pred f =
    CompareC f Binop Value
  | CompareF f Binop f
  | AndP (Pred f) (Pred f)
  | OrP (Pred f) (Pred f)
--  | NotP (Pred f)
  | Nop
  deriving (Show, Eq)

instance Functor Pred where
    fmap f (CompareC f1 o c)  = CompareC (f f1) o c
    fmap f (CompareF f1 o f2) = CompareF (f f1) o (f f2)
    fmap f (AndP p1 p2)       = AndP (fmap f p1) (fmap f p2)
    fmap f (OrP p1 p2)        = OrP (fmap f p1) (fmap f p2)
--    fmap f (NotP p1)          = NotP (fmap f p1)
    fmap _ Nop                = Nop

type JoinCondition = [(Field, Field)]

data Edit =
    IdEdit
  -- Make this InsertInto Name Fields Query TODO
  | InsertInto Name Tuple
  -- | InsertQuery Name Query
  | DeleteFrom Name Predicate
  | UpdateWhere Name Predicate [(Field, Value)]
  | ComposeEdits Edit Edit
  deriving (Show, Eq)

--renameTable :: Edit -> Edit
--renameTable = undefined

data Query =
    SelectQ Predicate Query
  | ProjectQ [Field] Query
  | RenameQ Field Field Query
  | JoinQ Query Query JoinCondition Name
  | UnionQ Query Query Name
  | TableQ Name
  deriving (Show, Eq)

getUniqueName :: Database -> IO String
getUniqueName = undefined

copyTable :: Database -> Name -> Database -> Name -> IO ()
copyTable = undefined

createTable :: Database -> Name -> [(Field, Type)] -> IO ()
createTable = undefined

rewriteEdit :: Name -> Name -> Edit -> Edit
rewriteEdit = undefined

rewritePredicate :: Name -> Name -> Predicate -> Predicate
rewritePredicate n1 n2 p = case p of
  CompareC f o v  -> CompareC (repl f) o v
  CompareF f o f' -> CompareF (repl f) o (repl f')
  AndP p1 p2 -> AndP (rewritePredicate n1 n2 p1) (rewritePredicate n1 n2 p2)
  OrP  p1 p2 -> OrP (rewritePredicate n1 n2 p1) (rewritePredicate n1 n2 p2)
--  NotP p' -> NotP (rewritePredicate n1 n2 p')
  Nop -> Nop
  where repl (Field n) = QField n2 n
        repl (QField t n) | t == n1   = QField n2 n
                          | otherwise = QField t n

rewriteJC :: Name -> Name -> JoinCondition -> JoinCondition
rewriteJC = undefined

rewriteQuery :: Name -> Name -> Predicate -> Predicate
rewriteQuery = undefined

--translateQuery :: SMO -> (Name, Name) -> Query -> Query
--translateQuery (InsertTable n) _ q = q
--translateQuery s@(DeleteTable n) c@(c1, _) q = case q of
--  SelectQ p q' -> SelectQ p (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  TableQ n' | n == n'   -> TableQ c1
--            | otherwise -> q
--translateQuery s@(RenameTable n1 n2) c q = case q of
--  SelectQ p q' -> SelectQ (rewritePredicate n1 n2 p) (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) (rewriteJC n1 n2 jc) n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  TableQ n' | n1 == n'  -> TableQ n2
--            | otherwise -> q
--translateQuery s@(InsertColumn _ _ _) c q = q
--translateQuery s@(DeleteColumn n f v) c@(c1, _) q = case q of
--  SelectQ p q' -> SelectQ p (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  -- Assumes that the complement is up-to-date. Otherwise, an outerjoin would be needed.
--  TableQ n' | n == n'  -> JoinQ (TableQ n) (TableQ c1) [qualifiedKey n] n
--            | otherwise -> q
--translateQuery s@(Append n1 n2 n _) c@(c1,c2) q = case q of
--  SelectQ p q' -> SelectQ p (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  -- Assumes that the complement is up-to-date. Otherwise, need to use append predicate.
--  TableQ n' | n1 == n'  -> JoinQ (TableQ n) (TableQ c1) [qualifiedKey n1] n1
--            | n2 == n'  -> JoinQ (TableQ n) (TableQ c2) [qualifiedKey n2] n2
--            | otherwise -> q
--translateQuery s@(Split n n1 n2 _) c@(c1,c2) q = case q of
--  SelectQ p q' -> SelectQ p (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  TableQ n' | n == n'   -> RenameQ fkey key $ UnionQ
--                             (JoinQ (TableQ n1) (RenameQ key fkey (TableQ c1)) [key] n1)
--                             (JoinQ (TableQ n2) (RenameQ key fkey (TableQ c2)) [key] n2) n
--            | otherwise -> q
---- For Join, complements are the entire tables)
--translateQuery s@(Join n1 n2 n jc) c@(c1, c2) q = case q of
--  SelectQ p q' -> SelectQ p (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  TableQ n' | n1 == n'  -> TableQ c1
--            | n2 == n'  -> TableQ c2
--            | otherwise -> q
--translateQuery s@(Decompose n n1 n2 jc) c@(c1, c2) q = case q of
--  SelectQ p q' -> SelectQ p (translateQuery s c q')
--  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
--  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
--  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
--  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
--  TableQ n' | n == n'   -> JoinQ (TableQ c1) (TableQ c2) jc n
--            | otherwise -> q
--
--
--constructEditLens :: Database -> Database -> SMO -> IO (DBEditLens Edit Name)
--constructEditLens compDB c (InsertTable n) = do
--  compname <- getUniqueName compDB
--  copyTable c n compDB compname
--  return $ EditLens compname putr putl
--  where
--    putr = return
--    putl e = do
--      compname <- get
--      lift $ applyEdit compname e
--    applyEdit compname e =
--      case e of
--        InsertInto n' t | n == n' -> apply (InsertInto compname t) compDB >> return IdEdit
--        DeleteFrom n' p | n == n' -> apply (DeleteFrom compname p) compDB >> return IdEdit
--        UpdateWhere n' p fs | n == n' -> apply (UpdateWhere compname p fs) compDB >> return IdEdit
--        ComposeEdits e1 e2 -> composeEdits <$> (applyEdit compname e1) <*> (applyEdit compname e2)
--        _ -> return e
------constructEditLens compDB c (DeleteTable n) = liftM inv $ constructEditLens compDB c (InsertTable n)
------constructEditLens compDB c (RenameTable n1 n2) = return $ DBEditLens () putfn putfn
------  where putfn = undefined
------constructEditLens compDB c (Append n1 n2 n p) = do
------  max1 <- undefined :: IO Integer
------  max2 <- undefined :: IO Integer
------  max <- undefined :: IO Integer
------  c1 <- getUniqueName compDB
------  createTable compDB c1 [("lkey", "Integer")]
------  c2 <- getUniqueName compDB
------  createTable compDB c2 [("rkey", "Integer")]
------  let putr e = case e of
------        (InsertInto n' t) | n1 == n' -> do
------          lift $ apply (InsertInto c1 [show $ max1+1, show $ max+1]) compDB
------          put ((max1+1, max2, max+1), c1, c2)
------          return $ InsertInto n t
------        (InsertInto n' t) | n2 == n' -> do
------          lift $ apply (InsertInto c2 [show $ max2+1, show $ max+1]) compDB
------          put ((max1, max2+1, max+1), c1, c2)
------          return $ InsertInto n t
------        (DeleteFrom n' p) | n2 == n' -> undefined
------        _ -> return e
------  return $ DBEditLens ((max1, max2, max), c1, c2) putr putl
------  where  putl = undefined
-------- Important to have the default value in the complement (in the putr direction mainly)
-------- This is because the complement could be used in the translateQuery function.
------constructEditLens compDB c (DeleteColumn n f v) = undefined
------
--------constructEditLens compDB _ (DeleteTable n) = do
--------
--------  DBEditLens () putr putl
------  -- store table in complement
------  -- putr:apply edit to lens in complement
------  --
--------rewriteEdit (DeleteTable n) (InsertInto
------
