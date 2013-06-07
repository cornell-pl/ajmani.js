{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}

module EditLenses.RelationalAlgebra where

import Control.Monad.State
import Control.Applicative

data Database

data DBEditLens la = forall c. (Eq c, Show c) 
                     => DBEditLens { init :: c,
                                     putr :: la -> StateT c IO la,                     
                                     putl :: la -> StateT c IO la }

inv :: DBEditLens la -> DBEditLens la
inv (DBEditLens def pr pl) = DBEditLens def pl pr

composeLens :: DBEditLens la -> DBEditLens la -> DBEditLens la
composeLens (DBEditLens def1 pr1 pl1) (DBEditLens def2 pr2 pl2) = DBEditLens (def1, def2) pr pl
  where pr a = do
          (s1,s2) <- get
          (b, s1') <- lift $ runStateT (pr1 a) s1
          (c, s2') <- lift $ runStateT (pr2 b) s2
          put (s1', s2')
          return c
        pl c = do
          (s1,s2) <- get
          (b, s2') <- lift $ runStateT (pl2 c) s2
          (a, s1') <- lift $ runStateT (pl1 b) s1
          put (s1', s2')
          return a
                                                          
class EditLanguage a l where
  apply :: l -> a -> IO a                          
  
instance EditLanguage Database Edit where
  apply = undefined
  
instance EditLanguage Database SMO where
  apply = undefined  
  
type Name = String
data Field = 
    Field Name
  | QField Name Name
  
type Type = String
type Tuple = [Field]
type Value = String

key :: Field
key = Field "rowid"
fkey :: Field
fkey = Field "fkey"
qualifiedKey :: Name -> Field
qualifiedKey n = QField n "rowid" 
qualifiedFKey :: Name -> Field
qualifiedFKey n = QField n "fkey"

data Binop = Eq | Neq | Lt | Leq | Gt | Geq  

data Predicate = 
    CompareC Field Binop Value
  | CompareF Field Binop Field
  | AndP Predicate Predicate
  | OrP Predicate Predicate
  | NotP Predicate
  
type JoinCondition = [Field]

data SMO = 
    InsertTable Name
  | DeleteTable Name
  | RenameTable Name Name
  | InsertColumn Name Field Value
  | DeleteColumn Name Field Value
  | Append Name Name Name Predicate
  | Split  Name Name Name Predicate
  | Join Name Name Name JoinCondition
  | Decompose Name Name Name JoinCondition
  | ComposeSMO SMO SMO

data Edit =
    IdEdit
  | InsertInto Name Tuple
  -- | InsertQuery Name Query
  | DeleteFrom Name Predicate
  | UpdateWhere Name Predicate [(Field, Value)]
  | ComposeEdits Edit Edit
  
renameTable :: Edit -> Edit
renameTable = undefined
  
composeEdits :: Edit -> Edit -> Edit
composeEdits = ComposeEdits
  
data Query =
    SelectQ Predicate Query
  | ProjectQ [Field] Query
  | RenameQ Field Field Query
  | JoinQ Query Query JoinCondition Name
  | UnionQ Query Query Name
  | TableQ Name
  
getUniqueName :: Database -> IO String
getUniqueName = undefined

copyTable :: Database -> Name -> Database -> Name -> IO ()
copyTable = undefined

createTable :: Database -> Name -> [(Field, Type)] -> IO ()
createTable = undefined

rewritePredicate :: Name -> Name -> Predicate -> Predicate
rewritePredicate = undefined

rewriteJC :: Name -> Name -> JoinCondition -> JoinCondition
rewriteJC = undefined

rewriteQuery :: Name -> Name -> Predicate -> Predicate
rewriteQuery = undefined

translateQuery :: SMO -> (Name, Name) -> Query -> Query
translateQuery (InsertTable n) _ q = q
translateQuery s@(DeleteTable n) c@(c1, _) q = case q of
  SelectQ p q' -> SelectQ p (translateQuery s c q')
  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
  TableQ n' | n == n'   -> TableQ c1
            | otherwise -> q
translateQuery s@(RenameTable n1 n2) c q = case q of
  SelectQ p q' -> SelectQ (rewritePredicate n1 n2 p) (translateQuery s c q')
  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) (rewriteJC n1 n2 jc) n'
  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
  TableQ n' | n1 == n'  -> TableQ n2
            | otherwise -> q
translateQuery s@(InsertColumn _ _ _) c q = q
translateQuery s@(DeleteColumn n f v) c@(c1, _) q = case q of
  SelectQ p q' -> SelectQ p (translateQuery s c q')
  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
  -- Assumes that the complement is up-to-date. Otherwise, an outerjoin would be needed.
  TableQ n' | n == n'  -> JoinQ (TableQ n) (TableQ c1) [qualifiedKey n] n
            | otherwise -> q
translateQuery s@(Append n1 n2 n _) c@(c1,c2) q = case q of
  SelectQ p q' -> SelectQ p (translateQuery s c q')
  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
  -- Assumes that the complement is up-to-date. Otherwise, need to use append predicate.
  TableQ n' | n1 == n'  -> JoinQ (TableQ n) (TableQ c1) [qualifiedKey n1] n1
            | n2 == n'  -> JoinQ (TableQ n) (TableQ c2) [qualifiedKey n2] n2
            | otherwise -> q
translateQuery s@(Split n n1 n2 _) c@(c1,c2) q = case q of
  SelectQ p q' -> SelectQ p (translateQuery s c q')
  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
  TableQ n' | n == n'   -> RenameQ fkey key $ UnionQ 
                             (JoinQ (TableQ n1) (RenameQ key fkey (TableQ c1)) [key] n1)  
                             (JoinQ (TableQ n2) (RenameQ key fkey (TableQ c2)) [key] n2) n
            | otherwise -> q
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
translateQuery s@(Decompose n n1 n2 jc) c@(c1, c2) q = case q of
  SelectQ p q' -> SelectQ p (translateQuery s c q')
  ProjectQ jc q' -> ProjectQ jc (translateQuery s c q')
  RenameQ f1 f2 q' -> RenameQ f1 f2 (translateQuery s c q')
  JoinQ q1 q2 jc n' -> JoinQ (translateQuery s c q1) (translateQuery s c q2) jc n'
  UnionQ q1 q2 n' -> UnionQ (translateQuery s c q1) (translateQuery s c q2) n'
  TableQ n' | n == n'   -> JoinQ (TableQ c1) (TableQ c2) jc n
            | otherwise -> q
 
  
--constructEditLens :: Database -> Database -> SMO -> IO (DBEditLens Edit)
--constructEditLens compDB c (InsertTable n) = do
--  compname <- getUniqueName compDB
--  copyTable c n compDB compname
--  return $ DBEditLens compname putr putl
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
--constructEditLens compDB c (DeleteTable n) = liftM inv $ constructEditLens compDB c (InsertTable n)        
--constructEditLens compDB c (RenameTable n1 n2) = return $ DBEditLens () putfn putfn
--  where putfn = undefined
--constructEditLens compDB c (Append n1 n2 n p) = do
--  max1 <- undefined :: IO Integer 
--  max2 <- undefined :: IO Integer 
--  max <- undefined :: IO Integer
--  c1 <- getUniqueName compDB
--  createTable compDB c1 [("lkey", "Integer")] 
--  c2 <- getUniqueName compDB
--  createTable compDB c2 [("rkey", "Integer")]
--  let putr e = case e of
--        (InsertInto n' t) | n1 == n' -> do
--          lift $ apply (InsertInto c1 [show $ max1+1, show $ max+1]) compDB
--          put ((max1+1, max2, max+1), c1, c2)
--          return $ InsertInto n t
--        (InsertInto n' t) | n2 == n' -> do
--          lift $ apply (InsertInto c2 [show $ max2+1, show $ max+1]) compDB
--          put ((max1, max2+1, max+1), c1, c2)
--          return $ InsertInto n t
--        (DeleteFrom n' p) | n2 == n' -> undefined 
--        _ -> return e  
--  return $ DBEditLens ((max1, max2, max), c1, c2) putr putl
--  where  putl = undefined 
---- Important to have the default value in the complement (in the putr direction mainly)
---- This is because the complement could be used in the translateQuery function.
--constructEditLens compDB c (DeleteColumn n f v) = undefined          
--        
----constructEditLens compDB _ (DeleteTable n) = do
----  
----  DBEditLens () putr putl
--  -- store table in complement
--  -- putr:apply edit to lens in complement
--  --    
----rewriteEdit (DeleteTable n) (InsertInto   
--  