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
                            
                                             
