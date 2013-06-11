
module EditLenses.Types where

import Database.HDBC
import Database.HDBC.PostgreSQL

type SQL = String
type Database = Connection
  
data CreateTable = CreateTable Name [(Field, Type)]
data DeleteTable = DeleteTable Name [(Field, Type)]
data RenameTable = RenameTable Name Name
data InsertColumn = InsertColumn Name (Field, Type) Value
data DeleteColumn = DeleteColumn Name (Field, Type) Value
data Append = Append Name Name Name Predicate
data Split = Split Name Name Name Predicate
data Join = Join Name Name Name JoinCondition
data Decompose = Decompose Name Name Name JoinCondition
data Compose a b = Compose a b

-- Not SMO's but help to abstract out certain operations
data CopyTable = CopyTable Name Name
data SelectInto = SelectInto Name Name Fields Predicate

type Name = String
data Field =
    Field Name
  | QField Name Name
  deriving (Show, Eq)
  
type Type = String
type Value = String

type Fields = [Field]
type Tuple = [Value]

data Binop = Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show, Eq)

type Predicate = Pred Field

data Pred f =
    CompareC f Binop Value
  | CompareF f Binop f
  | AndP (Pred f) (Pred f)
  | OrP (Pred f) (Pred f)
  | Nop
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
  | InsertInto Name Tuple
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
  deriving (Show, Eq)
  