
module Database 
  ( Database,
    Records,
    Table(..), 
    Field,
    Fields,
    Id, 
    Name, 
    Header, 
    Headers,
    empty,
    getTableNames,
    getTableByName, 
    getHeaders, 
    getRecords, 
    getRecordById,
    putTable, 
    putRecord, 
    createRecord, 
    delTable, 
    delRecord )
  where

import qualified Data.Map as Map
import Control.Concurrent.MVar
import qualified Data.List as List

type Header = String

type Headers = [Header]

type Name = String

type Id = Int

type Field = String

type Fields = [Field]

data Table = Table Headers Records
  deriving (Eq,Show)

type Records = Map.Map Id Fields

type Database = Map.Map Name Table

-- Accessors

empty :: Database
empty = Map.empty

getTableNames :: Database -> [Name]
getTableNames = Map.keys
  
getTableByName :: Name -> Database -> Maybe Table
getTableByName = Map.lookup

getHeaders :: Table -> Headers
getHeaders (Table hs _) = hs

getRecords :: Table -> Records
getRecords (Table _ rs) = rs

getRecordById :: Id -> Table -> Maybe Fields
getRecordById i (Table _ rs) = Map.lookup i rs

-- Mutators

putTable :: Name -> Table -> Database -> Database
putTable = Map.insert

putRecord :: Id -> Fields -> Table -> Table
putRecord i fs (Table hs rs) = Table hs $ Map.insert i fs rs

createRecord :: Fields -> Table -> (Id, Table)
createRecord fs (Table hs rs) = (i+1, Table hs $ Map.insert (i+1) fs rs)
  where i = foldl max 0 (Map.keys rs)

delTable :: Name -> Database -> Database
delTable = Map.delete 

delRecord :: Id -> Table -> Table
delRecord i (Table hs rs) = Table hs $ Map.delete i rs

