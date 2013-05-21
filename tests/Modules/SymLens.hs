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
import qualified SymLens.Table as ST
import Database

prop_law1 :: (Eq a, Arbitrary a, Arbitrary b) => SymLens a b -> a -> Bool
prop_law1 (SymLens d pr pl) a = case pr a d of
  (b,c) -> case pl b c of
    (a',c') -> a'==a && c'==c

prop_law2 :: (Eq b, Arbitrary a, Arbitrary b) => SymLens a b -> b -> Bool
prop_law2 (SymLens d pr pl) b = case pl b d of
  (a,c) -> case pr a c of
    (b',c') ->b==b' && c==c'

-- Tests for SumLens.Database
genContainsTableDB :: [Header] -> Gen Database -> Gen Database
genContainsTableDB ns gd = do
  db <- gd
  foldM on db ns
    where on d n = do
            t <- arbitrary
            return $ Map.insert n t d

genContainsColumn :: [Header] -> Gen Table -> Gen Table
genContainsColumn ns gt = do
  t <- gt
  foldM on t ns
    where on :: Table -> Header -> Gen Table
          on t@(Table hs rs) n | n `List.elem` hs = return t
                               | otherwise        = do
                                 rs'  <- mapMM with rs
                                 return $ Table (n:hs) rs'
          with cs = do
            c <- arbitrary
            return $ c:cs
          mapMM :: (Monad m, Ord k) => (a -> m b) -> Map.Map k a -> m (Map.Map k b)
          mapMM f m = do
            o <- mapM f' $ Map.toList m
            return $ Map.fromList o
            where f' (a,b) = do
                    b' <- f b
                    return (a,b')
            

genNotContainsColumn :: [Name] -> Gen Table -> Gen Table
genNotContainsColumn ns gt = do
  t <- gt
  return $ foldl on t ns
    where on t@(Table hs rs) n = maybe t (removeColumn t) $ List.elemIndex n hs
          removeColumn (Table hs rs) i = Table (delete i hs) $ Map.map (delete i) rs
          delete n fs = take n fs ++ drop (n+1) fs  
          
genNotContainsTableDB :: [Name] -> Gen Database -> Gen Database
genNotContainsTableDB ns gd = do
  db <- gd
  foldM (\ a v -> return $ Map.delete v a) db ns

genSameHeaderTable :: Table -> Gen Table
genSameHeaderTable (Table hs _) = do
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
            
test_rename1 :: Property
test_rename1 = 
  forAll (genContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.rename "test1" "test2"
      laws db = prop_law1 dl db

test_rename2 :: Property
test_rename2 = 
  forAll (genContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.rename "test1" "test2"
      laws db = prop_law2 dl db 

test_insert1 :: Property
test_insert1 =
  forAll dbt laws
    where
      dbt = do
        db <- genNotContainsTableDB ["test"] arbitrary
        t <- arbitrary
        return (db,t)
      dl = SD.insert "test"
      laws (db,t) = prop_law1 (dl t) db

test_insert2 :: Property
test_insert2 =
  forAll dbt laws
    where
      dbt = do
        db <- genContainsTableDB ["test"] arbitrary
        t <- arbitrary
        return (db,t)
      dl = SD.insert "test"
      laws (db,t) = prop_law2 (dl t) db 

test_drop1 :: Property
test_drop1 =
  forAll (genContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.drop "test"
      laws db = prop_law1 dl db

test_drop2 :: Property
test_drop2 =
  forAll (genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.drop "test"
      laws db = prop_law2 dl db 

test_append1 :: Property
test_append1 =
  forAll (genSameHeaderTableDB ["test1","test2"] $ genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.append (\i _ -> i `rem` 2 ==0) "test1" "test2" "test"
      laws db = prop_law1 dl db

test_append2 :: Property
test_append2 =
  forAll (genContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.append (\i _ -> i `rem` 2 ==0) "test1" "test2" "test"
      laws db =  prop_law2 dl db


test_split1 :: Property
test_split1 =
  forAll (genContainsTableDB ["test"] $ genNotContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.split (\i _ -> i `rem` 2 ==0) "test" "test1" "test2"
      laws db = prop_law1 dl db

test_split2 :: Property
test_split2 =
  forAll (genSameHeaderTableDB ["test1","test2"] $ genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.split (\i _ -> i `rem` 2 ==0) "test" "test1" "test2"
      laws db = prop_law2 dl db


test_insertColumn1 :: Property
test_insertColumn1 =
    forAll (genNotContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.insertColumn "test" "def" 
      laws db = prop_law1 dl db

test_insertColumn2 :: Property
test_insertColumn2 =
    forAll (genContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.insertColumn "test" "def" 
      laws db = prop_law2 dl db

test_deleteColumn1 :: Property
test_deleteColumn1 =
    forAll (genContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.deleteColumn "test" "def" 
      laws db = prop_law1 dl db

test_deleteColumn2 :: Property
test_deleteColumn2 =
    forAll (genNotContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.deleteColumn "test" "def" 
      laws db = prop_law2 dl db

test_renameColumn1 :: Property
test_renameColumn1 =
    forAll (genContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.renameColumn "test" "test1" 
      laws db = prop_law1 dl db

test_renameColumn2 :: Property
test_renameColumn2 =
    forAll (genContainsColumn ["test1"] arbitrary) laws
    where
      dl = ST.renameColumn "test" "test1" 
      laws db = prop_law2 dl db

test_swapColumn1 :: Property
test_swapColumn1 =
    forAll (genContainsColumn ["test","test1"] arbitrary) laws
    where
      dl = ST.swapColumn "test" "test1" 
      laws db = prop_law1 dl db

test_swapColumn2 :: Property
test_swapColumn2 =
    forAll (genContainsColumn ["test","test1"] arbitrary) laws
    where
      dl = ST.swapColumn "test" "test1" 
      laws db = prop_law2 dl db

testLaws :: Test
testLaws = testGroup "Lens Laws:"
             [
               testProperty "Renaming of tables Law 1" test_rename1
             , testProperty "Renaming of tables Law 2" test_rename2
             , testProperty "Inserting table Law 1" test_insert1
             , testProperty "Inserting table Law 2" test_insert2
             , testProperty "Deleting table Law 1" test_drop1
             , testProperty "Deleting table Law 2" test_drop2               
             , testProperty "Appending of tables Law 1" test_append1
             , testProperty "Appending of tables Law2" test_append2              
             , testProperty "Splitting of tables Law 1" test_split1
             , testProperty "Splitting of tables Law 2" test_split2
             , testProperty "Insert Column Law 1" test_insertColumn1
             , testProperty "Insert Column Law 2" test_insertColumn2
             , testProperty "Delete Column Law 1" test_deleteColumn1
             , testProperty "Delete Column Law 2" test_deleteColumn2
             , testProperty "Rename Column Law 1" test_renameColumn1
             , testProperty "Rename Column Law 2" test_renameColumn2
             ]

tests = [ testLaws
        ]