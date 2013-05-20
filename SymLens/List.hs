
module SymLens.List where

import SymLens
import Data.List

addColumn :: (Eq a, Show a) => a -> SymLens ([a], Int) [a]
addColumn a = 
  SymLens ()
          (\(l, n) _ -> (insertAt n a l, ()))
          (\l _ -> case elemIndex a l of
                     Just n  -> let (l1, _:l2) = splitAt n l in
                                ((l1 ++ l2, n), ()) 
                     Nothing -> ((l,0), ()))
  where insertAt n a l = l1 ++ a:l2
          where (l1,l2) = splitAt n l
                            

cons :: (Eq a, Show a) => a -> SymLens [a] [a]
cons initA = 
  SymLens initA
          (\l a -> (a:l, a))
          (\(a:l) _ -> (l,a))

hd :: (Eq a, Show a) => SymLens [a] a
hd = 
  SymLens []
          (\(h:t) _ -> (h,t))
          (\h t -> (h:t,t))
              
tl :: (Eq a, Show a) => a -> SymLens [a] [a]
tl initA = 
  SymLens initA
          (\(h:t) _ -> (t,h))
          (\t h -> (h:t,h))
          
swapElem :: Int -> Int -> SymLens [a] [a]
swapElem i1 i2 | i1 == i2 = idL
               | i1 > i2  = swapElem i2 i1
               | i1 < i2  = 
  SymLens () f f
  where f l _ | length l > max i1 i2 = let (l1, a:l2) = splitAt i1 l in
                                       let (l3, b:l4) = splitAt (i2 - i1 - 1) l2 in
                                       (l1 ++ b:l3 ++ a:l4, ()) 
                    
append :: SymLens ([a],[a]) [a]
append = 
  SymLens 0
          (\(l1,l2) _ -> (l1++l2, length l1))
          (\l n -> ((take n l, drop n l), n))                             

rlmap :: (Eq a, Show a) => SymLens a b -> SymLens [a] [b]
rlmap (SymLens def pr pl) = 
  SymLens (repeat def)
          (\as cs -> let (bs, cs') = unzip $ map (uncurry pr) (zip as cs) in
                     (bs, cs' ++ drop (length cs') cs))
          (\bs cs -> let (as, cs') = unzip $ map (uncurry pl) (zip bs cs) in
                     (as, cs' ++ drop (length cs') cs))
                      
flmap :: (Eq a, Show a) => SymLens a b -> SymLens [a] [b]
flmap (SymLens def pr pl) = 
  SymLens []
          (\as cs -> unzip $ map (uncurry pr) (zip as (cs ++ repeat def)))                      
          (\bs cs -> unzip $ map (uncurry pl) (zip bs (cs ++ repeat def)))
