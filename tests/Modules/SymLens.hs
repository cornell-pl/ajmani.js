module Modules.SymLens where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Monad.State
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List

import Modules.Database () 
import SymLens
import qualified SymLens.Database as SD
import qualified SymLens.Table as ST
import Database.Memory

import Debug.Trace 

prop_law1 :: (Eq a, Arbitrary a, Arbitrary b) => SymLens a b -> a -> PropertyM IO ()
prop_law1 (SymLens d pr pl) a = do
  (b,c) <- run $ runStateT (pr a) d
  (a',c') <- run $ runStateT (pl b) c
  assert $ a'==a && c'==c

prop_law2 :: (Eq b, Arbitrary a, Arbitrary b) => SymLens a b -> b -> PropertyM IO ()
prop_law2 (SymLens d pr pl) b = do
  (a,c) <- run $ runStateT (pl b) d
  (b',c') <- run $ runStateT (pr a) c
  assert $ b==b' && c==c'

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
test_rename1 = monadicIO $ forAllM (genContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.rename "test1" "test2"
      laws db = prop_law1 dl db

test_rename2 :: Property
test_rename2 = 
  monadicIO $ forAllM (genContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.rename "test1" "test2"
      laws db = prop_law2 dl db 

test_insert1 :: Property
test_insert1 =
  monadicIO $ forAllM dbt laws
    where
      dbt = do
        db <- genNotContainsTableDB ["test"] arbitrary
        t <- arbitrary
        return (db,t)
      dl = SD.insert "test"
      laws (db,t) = prop_law1 (dl t) db

test_insert2 :: Property
test_insert2 =
  monadicIO $ forAllM dbt laws
    where
      dbt = do
        db <- genContainsTableDB ["test"] arbitrary
        t <- arbitrary
        return (db,t)
      dl = SD.insert "test"
      laws (db,t) = prop_law2 (dl t) db 

test_drop1 :: Property
test_drop1 =
  monadicIO $ forAllM (genContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.drop "test"
      laws db = prop_law1 dl db

test_drop2 :: Property
test_drop2 =
  monadicIO $ forAllM (genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.drop "test"
      laws db = prop_law2 dl db 

test_append1 :: Property
test_append1 =
  monadicIO $ forAllM (genSameHeaderTableDB ["test1","test2"] $ genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.append (\i _ -> i `rem` 2 ==0) "test1" "test2" "test"
      laws db = prop_law1 dl db

test_append2 :: Property
test_append2 =
  monadicIO $ forAllM (genContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.append (\i _ -> i `rem` 2 ==0) "test1" "test2" "test"
      laws db =  prop_law2 dl db


test_split1 :: Property
test_split1 =
  monadicIO $ forAllM (genContainsTableDB ["test"] $ genNotContainsTableDB ["test1","test2"] arbitrary) laws
    where
      dl = SD.split (\i _ -> i `rem` 2 ==0) "test" "test1" "test2"
      laws db = prop_law1 dl db

test_split2 :: Property
test_split2 =
  monadicIO $ forAllM (genSameHeaderTableDB ["test1","test2"] $ genNotContainsTableDB ["test"] arbitrary) laws
    where
      dl = SD.split (\i _ -> i `rem` 2 ==0) "test" "test1" "test2"
      laws db = prop_law2 dl db


-- Laws tests for SymLens.Table

test_insertColumn1 :: Property
test_insertColumn1 =
    monadicIO $ forAllM (genNotContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.insertColumn "test" "def" 
      laws db = prop_law1 dl db

test_insertColumn2 :: Property
test_insertColumn2 =
    monadicIO $ forAllM (genContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.insertColumn "test" "def" 
      laws db = prop_law2 dl db

test_deleteColumn1 :: Property
test_deleteColumn1 =
    monadicIO $ forAllM (genContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.deleteColumn "test" "def" 
      laws db = prop_law1 dl db

test_deleteColumn2 :: Property
test_deleteColumn2 =
    monadicIO $ forAllM (genNotContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.deleteColumn "test" "def" 
      laws db = prop_law2 dl db

test_renameColumn1 :: Property
test_renameColumn1 =
    monadicIO $ forAllM (genContainsColumn ["test"] arbitrary) laws
    where
      dl = ST.renameColumn "test" "test1" 
      laws db = prop_law1 dl db

test_renameColumn2 :: Property
test_renameColumn2 =
    monadicIO $ forAllM (genContainsColumn ["test1"] arbitrary) laws
    where
      dl = ST.renameColumn "test" "test1" 
      laws db = prop_law2 dl db

test_swapColumn1 :: Property
test_swapColumn1 =
    monadicIO $ forAllM (genContainsColumn ["test","test1"] arbitrary) laws
    where
      dl = ST.swapColumn "test" "test1" 
      laws db = prop_law1 dl db

test_swapColumn2 :: Property
test_swapColumn2 =
    monadicIO $ forAllM (genContainsColumn ["test","test1"] arbitrary) laws
    where
      dl = ST.swapColumn "test" "test1" 
      laws db = prop_law2 dl db

-- Specific tests for SymLens.Table

-- test_insertColumn :: Property
-- test_insertColumn =
--   monadicIO $ forAllM (genNotContainsColumn ["test"] arbitrary) $ laws dl
--     where
--       dl = ST.insertColumn "test" "def" 
--       laws (SymLens def pr pl) t = case pr t def of
--         (t', c) ->

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
             , testProperty "Rename Swap Law 1" test_swapColumn1
             , testProperty "Rename Swap Law 2" test_swapColumn2

             ]

tests = [ testLaws
        ]
