
module EditLenses.Utils where

import EditLenses.Types

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

replaceQual :: Name -> Name -> Field -> Field
replaceQual _ n' (Field fn) = QField n' fn
replaceQual n n' f@(QField n1 fn) | n == n'   = QField n' fn
                                  | otherwise = f             

getUniqueName :: Database -> IO String
getUniqueName = undefined

rewritePredicate :: Name -> Name -> Predicate -> Predicate
rewritePredicate n1 n2 = mapPredicate (replaceQual n1 n2) 
  
rewriteJC :: Name -> Name -> JoinCondition -> JoinCondition
rewriteJC n1 n2 = mapJC (replaceQual n1 n2)

rewriteEdit :: Name -> Name -> Edit -> Edit
rewriteEdit n1 n2 = mapEdit (replaceQual n1 n2)

mapPredicate :: (Field -> Field) -> Predicate -> Predicate
mapPredicate fun p = case p of                          
  CompareC f o v  -> CompareC (fun f) o v
  CompareF f o f' -> CompareF (fun f) o (fun f')
  AndP p1 p2 -> AndP (mapPredicate fun p1) (mapPredicate fun p2)
  OrP  p1 p2 -> OrP (mapPredicate fun p1) (mapPredicate fun p2)
--  NotP p' -> NotP (mapPredicate fun p')
  Nop -> Nop
  InC v q -> InC v (mapQuery fun q)
  InF f q -> InF (fun f) (mapQuery fun q)
  Exists q -> Exists (mapQuery fun q)
  NotExists q -> NotExists (mapQuery fun q)

mapQuery :: (Field -> Field) -> Query -> Query
mapQuery fun q = case q of
  SelectQ p q' -> SelectQ (mapPredicate fun p) (mapQuery fun q')
  ProjectQ fs q' -> ProjectQ (map fun fs) (mapQuery fun q')
  RenameQ f1 f2 q' -> RenameQ (fun f1) (fun f2) (mapQuery fun q')
  JoinQ q1 q2 jc n -> JoinQ (mapQuery fun q1) (mapQuery fun q2) (mapJC fun jc) n 
  UnionQ q1 q2 n -> UnionQ (mapQuery fun q1) (mapQuery fun q2) n  
  TableQ n -> TableQ n

mapJC :: (Field -> Field) -> JoinCondition -> JoinCondition
mapJC fun = map (\(x,y) -> (fun x, fun y)) 

mapEdit :: (Field -> Field) -> Edit -> Edit
mapEdit fun e = case e of
  InsertInto n t -> e
  DeleteFrom n p -> DeleteFrom n (mapPredicate fun p)
  UpdateWhere n p fvs -> UpdateWhere n (mapPredicate fun p) (map (\(f,v) -> (fun f, v)) fvs) 
  ComposeEdits e1 e2 -> ComposeEdits (mapEdit fun e1) (mapEdit fun e2)
  _ -> e
  