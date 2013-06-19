{-# LANGUAGE OverloadedStrings #-}
module EditLenses.Types where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.String
import Data.List.Split

type SQL = String
type Database = Connection
  
data CreateTable = CreateTable Table [(Field, Type)]
data DeleteTable = DeleteTable Table [(Field, Type)]
data RenameTable = RenameTable Name Name
data InsertColumn = InsertColumn Table (Field, Type) Value
data DeleteColumn = DeleteColumn Table (Field, Type) Value
data Append = Append Name Name Name Predicate
data Split = Split Name Name Name Predicate
data Join = Join Name Name Name JoinCondition
data Decompose = Decompose Name Name Name JoinCondition
data Compose a b = Compose a b

-- Not SMO's but help to abstract out certain operations
data CopyTable = CopyTable Name Name
data SelectInto = SelectInto Name Name Fields Predicate

-- TODO: Change this to contain more information about table like primary key.                
type Name = String

data Table = Table {getTableName :: Name,
                    getPrimaryKey :: Fields}

data Field =
    Field Name
  | QField Name Name
  deriving (Show, Eq)

instance IsString Field where
    fromString str = case splitOn "." str of
                        [f]   -> Field f
                        [t,f] -> QField t f
                        _     -> error "Illegal use of field"

                                 
type Type = String
type Value = String

type Fields = [Field]
type Tuple = [Value]

data Binop = Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show, Eq)

type Predicate = Pred Field

data Pred f =
    Nop
  | CompareC f Binop Value
  | CompareF f Binop f
  | AndP (Pred f) (Pred f)
  | OrP (Pred f) (Pred f)
  | InC Value Query
  | InF f Query
  | NotInC Value Query
  | NotInF f Query
  | Exists Query
  | NotExists Query
  deriving (Show, Eq)

instance Functor Pred where
  fmap f (CompareC f1 o c)  = CompareC (f f1) o c
  fmap f (CompareF f1 o f2) = CompareF (f f1) o (f f2)
  fmap f (AndP p1 p2)       = AndP (fmap f p1) (fmap f p2)
  fmap f (OrP p1 p2)        = OrP (fmap f p1) (fmap f p2)
  fmap f (InF f1 q)         = InF (f f1) q
  fmap f (NotInF f1 q)      = NotInF (f f1) q
  fmap _ Nop                = Nop
  
type JoinCondition = [(Field, Field)]

data Edit =
    IdEdit
  -- Make this InsertInto Name Fields Query TODO
  | InsertInto Name Fields Query
  | DeleteFrom Name Predicate
  | UpdateWhere Name Predicate [(Field, Value)]
  | ComposeEdits Edit Edit
  deriving (Show, Eq)

data Query =
    SelectQ Predicate Query
  | ProjectQ [Field] Query 
  | RenameQ Field Field Query
  | JoinQ Query Query JoinCondition Name
  | UnionQ Query Query Name
  | TableQ Name
  | TupleQ Tuple Name
  deriving (Show, Eq)
  
