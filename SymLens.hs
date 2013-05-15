{-# LANGUAGE ExistentialQuantification #-}

module SymLens where

import Control.Category (Category)
import qualified Control.Category as C

data SymLens a b = forall c. SymLens {init :: c,
                                      putr :: a -> c -> (b,c),
                                      putl :: b -> c -> (a,c)}

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

rlmap :: SymLens a b -> SymLens [a] [b]
rlmap (SymLens def pr pl) = SymLens (repeat def) pr' pl'
  where pr' as cs = (bs, cs' ++ drop (length cs') cs)
          where (bs, cs') = unzip $ map (uncurry pr) (zip as cs)
        pl' bs cs = (as, cs' ++ drop (length cs') cs)
          where (as, cs') = unzip $ map (uncurry pl) (zip bs cs) 

flmap :: SymLens a b -> SymLens [a] [b]
flmap (SymLens def pr pl) = SymLens (repeat def) pr' pl'
  where  pr' as cs = unzip $ map (uncurry pr) (zip as cs)
         pl' bs cs = unzip $ map (uncurry pl) (zip bs cs)

instance Category SymLens where
  id  = idL
  (.) = flip compose
