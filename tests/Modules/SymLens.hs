module Modules.SymLens where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad
import qualified Data.Map as Map

import Modules.Database () 
import SymLens
import SymLens.Database
import Database

prop_law1 :: (Eq a, Arbitrary a, Arbitrary b) => SymLens a b -> a -> Bool
prop_law1 (SymLens d pr pl) a = case pr a d of
  (b,c) -> (fst $ pl b c) == a

prop_law2 :: (Eq b, Arbitrary a, Arbitrary b) => SymLens a b -> b -> Bool
prop_law2 (SymLens d pr pl) b = case pl b d of
  (a,c) -> (fst $ pr a c) == b

-- Tests for SumLens.Database
genContainsTableDB :: [Name] -> Gen Database
genContainsTableDB ns = do
  db <- arbitrary
  foldM on db ns
 where
   on d n = do
     t <- arbitrary
     return $ Map.insert n t d

test_rename :: Database -> Bool
test_rename db = 
  let sl = rename "test1" "test2" in
  prop_law1 sl db && prop_law2 sl db 
  
test_append :: Database -> Bool
test_append = undefined

testLaws :: Test
testLaws = testGroup "Lens Laws:"
             [ testProperty "Renaming of tables" $ forAll (genContainsTableDB ["test1","test2"]) test_rename
             ]

tests = [ testLaws
        ]