{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module EditLenses.Translator where

import EditLenses.RelationalAlgebra
import Data.List

type SQL = String

class Translate st where
  translate :: st -> SQL

-- Cas use predicate as join condition
instance Translate Query where
  translate (SelectQ p q) = unwords ["SELECT * FROM (",translate q,") WHERE (",translate p,")"]
  translate (ProjectQ f q) = unwords ["SELECT ",intercalate "," (map translate f),"FROM (",translate q,")"]
  translate (RenameQ f1 f2 q) = unwords ["SELECT",translate f1,"AS",translate f2,"FROM","(",translate q,")"]
  translate (JoinQ q1 q2 jc n) = unwords ["(SELECT * FROM (",translate q1,") JOIN (",translate q2,") ON",intercalate "AND" (map (\(a,b) -> unwords [translate a,"=",translate b]) jc),") AS", n]
  translate (UnionQ q1 q2 n) = unwords ["(SELECT * FROM (",translate q1,") UNION SELECT * FROM (",translate q1,")) AS",n]
  translate (TableQ n) = n

instance Translate Predicate where
  translate (CompareC f o c) = unwords ["(",translate f,translate o,c,")"]
  translate (CompareF f1 o f2) = unwords ["(",translate f1,translate o,translate f2,")"]
  translate (AndP p1 p2) = unwords ["(",translate p1,"AND",translate p2,")"]
  translate (OrP p1 p2) = unwords ["(",translate p1,"OR",translate p2,")"]
  translate Nop = "True"

instance Translate Field where
  translate (Field n) = n
  translate (QField t n) = intercalate "." [t,n]

instance Translate Binop where
  translate Eq  = "="
  translate Neq = "<>"
  translate Lt  = "<"
  translate Gt  = ">"
  translate Leq = "<="
  translate Geq = ">="

instance Translate CreateTable where
  translate (CreateTable n ft) = unwords ["CREATE TABLE",n,"(",intercalate "," (map (\(a,b) -> unwords [translate $ unqual a,b]) ft),")"]

instance Translate DeleteTable where
  translate (DeleteTable n ft) = unwords ["DROP TABLE",n]

instance Translate RenameTable where
  translate (RenameTable from to) = unwords ["ALTER TABLE",from,"RENAME TO",to]

instance Translate InsertColumn where
  translate (InsertColumn n (f,t) v) = unwords ["ALTER TABLE",n,"ADD COLUMN",translate $ unqual f,v,"NOT NULL DEFAULT(",v,")"]

instance Translate DeleteColumn where
  translate (DeleteColumn n (f,_) v) = unwords ["ALTER TABLE",n,"DROP COLUMN",translate $ unqual f]

-- Remember it forgets about the constraints like Primary key, Distinct etc.
instance Translate CopyTable where
  translate (CopyTable from to) = unwords ["SELECT * INTO",to,"FROM",from]

instance Translate Append where
  translate (Append t1 t2 to _) = unwords ["SELECT * INTO",to,"(","SELECT * FROM",t1,"UNION","SELECT * FROM",t2,")"]

instance Translate Split where
  translate (Split from t1 t2 p) = unwords ["SELECT * INTO",t1,"(","SELECT * FROM",from,"WHERE","(",translate p,")",")",";",
                                            "SELECT * INTO",t2,"(","SELECT * FROM",from,"NOT IN","(","SELECT * FROM",from,"WHERE","(",translate p,")",")",")",";"]

instance Translate Join where
  translate (Join n1 n2 to jc) = error "Not implemented"

instance Translate Decompose where
  translate (Decompose from n1 n2 jc) = error "Not implemented"

instance (Translate a, Translate b) => Translate (Compose a b) where
  translate (Compose a b) = unwords [translate a,";",translate b,";"]

unqual :: Field -> Field
unqual (QField _ n) = Field n
unqual f = f
