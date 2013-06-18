{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}
module EditLenses.Translator where

import Database.HDBC 
import EditLenses.Types
import EditLenses.Utils
import Data.List
import Control.Monad

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
  
instance Translate Edit where
  translate IdEdit = "0"
  translate (InsertInto n t) = unwords ["INSERT INTO",n,"VALUES",translate t]
  translate (DeleteFrom n p) = unwords ["DELETE FROM",n,"WHERE",translate p]
  translate (UpdateWhere n p fvs) = unwords ["UPDATE",n,"SET",translate assns,"WHERE",translate p]
    where assns = map (\(f,v) -> translate f ++ "=" ++ v) fvs
  translate (ComposeEdits e1 e2) = translate e1 ++ ";" ++ translate e2

instance (Translate f) => (Translate (Pred f)) where
  translate (CompareC f o c) = unwords ["(",translate f,translate o,c,")"]
  translate (CompareF f1 o f2) = unwords ["(",translate f1,translate o,translate f2,")"]
  translate (AndP p1 p2) = unwords ["(",translate p1,"AND",translate p2,")"]
  translate (OrP p1 p2) = unwords ["(",translate p1,"OR",translate p2,")"]
  translate Nop = "True"

instance Translate Field where
  translate (Field n) = n
  translate (QField t n) = intercalate "." [t,n]
  
instance Translate Value where
  translate = id
  
instance (Translate a) => Translate [a] where
  translate l = "(" ++ intercalate "," (map translate l) ++ ")" 

instance Translate Binop where
  translate Eq  = "="
  translate Neq = "<>"
  translate Lt  = "<"
  translate Gt  = ">"
  translate Leq = "<="
  translate Geq = ">="

instance Translate CreateTable where
  translate (CreateTable (Table n k) ft) = unwords ["CREATE TABLE",n,"(",fields,")"]
    where fields = intercalate "," ((map (\(a,b) -> unwords [getName a,b]) ft) ++ [pkey]) 
          pkey = "PRIMARY KEY(" ++ intercalate "," (map getName k) ++ ")"
          
instance Translate DeleteTable where
  translate (DeleteTable (Table n _) _) = unwords ["DROP TABLE",n]

instance Translate RenameTable where
  translate (RenameTable from to) = unwords ["ALTER TABLE",from,"RENAME TO",to]

instance Translate InsertColumn where
  translate (InsertColumn (Table n _) (f,t) v) = unwords ["ALTER TABLE",n,"ADD COLUMN",translate $ unQualifyField f,v,"NOT NULL DEFAULT(",v,")"]

instance Translate DeleteColumn where
  translate (DeleteColumn (Table n _) (f,_) v) = unwords ["ALTER TABLE",n,"DROP COLUMN",translate $ unQualifyField f]

-- Remember it forgets about the constraints like Primary key, Distinct etc.
instance Translate CopyTable where
  translate (CopyTable from to) = unwords ["SELECT * INTO",to,"FROM",from]

instance Translate SelectInto where
  translate (SelectInto from to fields pred) = unwords ["SELECT",translate $ map unQualifyField fields,"INTO",to,"FROM",from,"WHERE",translate pred]

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

class EditLanguage a l where
  applyEdit :: a -> l -> IO ()

class QueryLanguage a l where
  runQuery :: a -> l -> IO [[Value]]
  
instance QueryLanguage Database Query where
  runQuery db q = liftM (map . map $ fromSql) $ quickQuery' db (translate q ++ ";") []

instance (Translate l) => EditLanguage Database l where
  applyEdit db e = runRaw db $ translate e ++ ";"
   
