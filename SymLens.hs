{-# LANGUAGE ExistentialQuantification #-}

module SymLens where

import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data SymLens a b = forall c. SymLens {init :: c,
                                      putr :: a -> c -> (b,c),
                                      putl :: b -> c -> (a,c)}
                                      
inv :: SymLens a b -> SymLens b a
inv (SymLens def pr pl) = SymLens def pl pr                                                 

idL :: SymLens a a
idL = 
  SymLens ()
          (\a _ -> (a,()))
          (\a _ -> (a,()))
                                      
compose :: SymLens a b -> SymLens b c -> SymLens a c
compose (SymLens def1 pr1 pl1) (SymLens def2 pr2 pl2) =
  SymLens (def1, def2)
          (\a (s1,s2) -> let (b, s1') = pr1 a s1 in
                         let (c, s2') = pr2 b s2 in
                         (c, (s1', s2')))
          (\c (s1,s2) -> let (b, s2') = pl2 c s2 in
                         let (a, s1') = pl1 b s1 in
                         (a, (s1', s2')))
                                                                              
instance Category SymLens where
    id = idL
    (.) = flip compose

projRight :: d -> SymLens (a,d) a
projRight def = 
  SymLens def
          (\(a, d) _ -> (a, d))
          (\a d -> ((a,d), d)) 

projLeft :: d -> SymLens a (a,d)
projLeft def = 
  SymLens def
          (\a d -> ((a,d), d))
          (\(a, d) _ -> (a, d))
                        
prod :: SymLens a b -> SymLens c d -> SymLens (a,c) (b,d)
prod (SymLens d1 pr1 pl1) (SymLens d2 pr2 pl2) = 
  SymLens (d1,d2) 
          (\(a,c) (s1,s2) -> let (b, s1') = pr1 a s1 in                        
                             let (d, s2') = pr2 c s2 in
                             ((b,d), (s1', s2')))
          (\(b,d) (s1,s2) -> let (a, s1') = pl1 b s1 in                        
                             let (c, s2') = pl2 d s2 in
                             ((a,c), (s1', s2')))
     

term :: a -> SymLens a ()
term def = 
  SymLens def
          (\a  _ -> ((), a))
          (\() a -> (a, a))
                   
disconnect :: a -> b -> SymLens a b
disconnect defa defb = term defa `compose` inv (term defb)

swap :: SymLens (a,b) (b,a)
swap = 
  SymLens () f f
  where f = (\(a,b) _ -> ((b,a), ()))
          
assocl :: SymLens (a,(b,c)) ((a,b),c)
assocl =
  SymLens () 
          (\(a,(b,c)) _ -> (((a,b),c),()))
          (\((a,b),c) _ -> ((a,(b,c)),()))
          
assocr :: SymLens ((a,b),c) (a,(b,c))
assocr = inv assocl

transpose :: SymLens ((a,b),(c,d)) ((a,c),(b,d))
transpose = assocr . ((assocl . (id `prod` swap) . assocr) `prod` id) . assocl

genDup :: (Eq a) => (a -> a -> String) -> SymLens a (a,a)
genDup errFn = 
  SymLens () 
          (\a _ -> ((a,a), ()))
          (\(a,a') _ -> if a == a' then (a, ())
                        else error (errFn a a')) 

dup :: (Eq a) => String -> SymLens a (a,a)
dup errMsg = 
  SymLens () 
          (\a _ -> ((a,a), ()))
          (\(a,a') _ -> if a == a' then (a, ())
                        else error errMsg) 
          
