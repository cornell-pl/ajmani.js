module Modules.SymLens where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List

import Modules.Database () 
import SymLens
import qualified SymLens.Database as SD
import Database

prop_law1 :: (Eq a, Arbitrary a, Arbitrary b) => SymLens a b -> a -> Bool
prop_law1 (SymLens d pr pl) a = case pr a d of
  (b,c) -> (fst $ pl b c) == a

prop_law2 :: (Eq b, Arbitrary a, Arbitrary b) => SymLens a b -> b -> Bool
prop_law2 (SymLens d pr pl) b = case pl b d of
  (a,c) -> (fst $ pr a c) == b

-- Tests for SumLens.Database
genContainsTableDB :: [Name] -> Gen Database -> Gen Database
genContainsTableDB ns gd = do
  db <- gd
  foldM on db ns
    where on d n = do
            t <- arbitrary
            return $ Map.insert n t d

genNotContainsTableDB :: [Name] -> Gen Database -> Gen Database
genNotContainsTableDB ns gd = do
  db <- gd
  foldM (\ a v -> return $ Map.delete v a) db ns

genSameHeaderTable :: Table -> Gen Table
genSameHeaderTable (Table hs rs) = do
  let n = length hs
  m <- choose (0,20 :: Int)
  recs <- foldM (f n) (Map.empty) [1..m]
  return $ Table hs recs
    where f size a k = do
            recs <- vector size
            return $ Map.insert k recs a

genSameHeaderTableDB :: [Name] -> Gen Database -> Gen Database
genSameHeaderTableDB ns gd = do
  t <- arbitrary
  db <- gd
  foldM (on t) db ns
   where on t d n = do
           t' <- genSameHeaderTable t
           return $ Map.insert n t' d
            
test_rename :: Property
test_rename = 
  forAll (genContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.rename "test1" "test2"
      laws db = prop_law1 dl db && prop_law2 dl db 

test_insert :: Property
test_insert =
  forAll dbt laws
    where
      dbt = do
        db <- genNotContainsTableDB ["test"] arbitrary
        t <- arbitrary
        return (db,t)
      dl = SD.insert "test"
      laws (db,t) = prop_law1 (dl t) db && prop_law2 (dl t) db 

test_drop :: Property
test_drop =
  forAll (genContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.drop "test"
      laws db = prop_law1 dl db && prop_law2 dl db 

test_append :: Property
test_append =
  forAll (genSameHeaderTableDB ["test1","test2"] $ genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.append (\i _ -> i `rem` 2 ==0) "test1" "test2" "test"
      laws db = prop_law1 dl db && prop_law2 dl db

test_split :: Property
test_split =
  forAll (genContainsTableDB ["test"] $ genNotContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.split (\i _ -> i `rem` 2 ==0) "test" "test1" "test2"
      laws db = prop_law1 dl db && prop_law2 dl db

testLaws :: Test
testLaws = testGroup "Lens Laws:"
             [ testProperty "Renaming of tables" test_rename
             , testProperty "Inserting table" test_insert
             , testProperty "Deleting table" test_drop
             , testProperty "Appending of tables" test_append
             , testProperty "Splitting of tables" test_split
             ]

tests = [ testLaws
        ]