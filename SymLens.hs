{-# LANGUAGE ExistentialQuantification #-}

module SymLens where

import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Category (Category)
import qualified Control.Category as C

data SymLens a b = forall c. SymLens { state :: c,
                                       putr :: a -> c -> (b,c),
                                       putl :: b -> c -> (a,c) }

instance Category SymLens where
    id = idL
    (.) = flip compose
                        
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

compose :: SymLens a b -> SymLens b c -> SymLens a c
compose (SymLens def1 pr1 pl1) (SymLens def2 pr2 pl2) = SymLens (def1, def2) pr pl
  where pr a (s1,s2) = (c, (s1', s2'))
          where (b, s1') = pr1 a s1
                (c, s2') = pr2 b s2
        pl c (s1,s2) = (a, (s1', s2'))
          where (b, s2') = pl2 c s2
                (a, s1') = pl1 b s1

projRight :: d -> SymLens (a,d) a
projRight def = SymLens def pr pl
  where pr     = const
        pl a d = ((a,d),d)

projLeft :: d -> SymLens a (a,d)
projLeft = inv . projRight

prod :: SymLens a b -> SymLens c d -> SymLens (a,c) (b,d)
prod (SymLens d1 pr1 pl1) (SymLens d2 pr2 pl2) = SymLens (d1,d2) pr pl
  where pr (a,c) (s1,s2) = ((b,d), (s1', s2'))
          where (b, s1') = pr1 a s1
                (d, s2') = pr2 c s2             
        pl (b,d) (s1,s2) = ((a,c), (s1', s2'))
          where (a, s1') = pl1 b s1
                (c, s2') = pl2 d s2

inv :: SymLens a b -> SymLens b a
inv (SymLens def pr pl) = SymLens def pl pr

idL :: SymLens a a
idL = SymLens () pr pl
  where pr a _ = (a,())
        pl a _ = (a,())

term :: a -> SymLens a ()
term def = SymLens def pr pl
  where pr a  _ = ((), a)
        pl () a = (a, a)

disconnect :: a -> b -> SymLens a b
disconnect defa defb = term defa `compose` inv (term defb)

