{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Modules.Database where

import Control.Applicative
import qualified Data.Map as Map
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad

import Database

newtype BInt = BInt Int deriving (Integral, Enum, Real, Num, Ord, Eq, Show)

instance Bounded BInt where
  minBound = BInt 0
  maxBound = BInt 50

instance Arbitrary Table where
  arbitrary = do
    (BInt n) <- arbitrarySizedBoundedIntegral
    (BInt m) <- arbitrarySizedBoundedIntegral
    headers <- vector n
    recs <- foldM (f n) (Map.empty) [1..m] 
    return $ Table headers recs
   where f size a k = do
           recs <- vector size
           return $ Map.insert k recs a

prop_create_get :: Fields -> Table -> Bool
prop_create_get f t = Just f == getRecordById i t'
  where (i,t') = createRecord f t

test_create_get :: Test
test_create_get = testProperty "createRecord . get should return the same record" prop_create_get

tests :: [Test]
tests = [ test_create_get
        ]
                  
  
      