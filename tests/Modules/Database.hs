{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Modules.Database where

import Control.Applicative
import qualified Data.Map as Map
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.List

import Control.Monad

import Database

boundedInt :: Int -> Int -> Gen Int
boundedInt lb ub = choose (lb,ub)

uniqueList :: (Eq a, Arbitrary a) => Int -> Gen [a]
uniqueList n = unique n []
  where
    unique n ls | n <= 0 =  return ls
                | otherwise = do
                    el <- arbitrary
                    -- unique (n-1) (el:ls)
                    if el `elem` ls then unique n ls else unique (n-1) (el:ls)
  
instance Arbitrary Table where
  arbitrary = do
    n <- boundedInt 0 5
    m <- boundedInt 0 20
    headers <- vector n
    recs <- foldM (f n) (Map.empty) [1..m] 
    return $ Table headers recs
   where f size a k = do
           recs <- vector size
           return $ Map.insert k recs a

instance Arbitrary Database where
  arbitrary = do
    n <- boundedInt 0 10
    foldM on Map.empty [1..n]
   where 
    on d _ = do
      t <- arbitrary
      n <- arbitrary
      return $ Map.insert n t d
      
prop_create_get :: Fields -> Table -> Bool
prop_create_get f t = Just f == getRecordById i t'
  where (i,t') = createRecord f t

test_create_get :: Test
test_create_get = testProperty "createRecord . get should return the same record" prop_create_get

tests :: [Test]
tests = [ test_create_get
        ]
                  
  
      