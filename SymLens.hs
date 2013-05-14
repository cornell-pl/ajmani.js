{-# LANGUAGE ExistentialQuantification #-}

module SymLens where

data SymLens a b = forall c. SymLens {init :: c,
                                      putr :: a -> c -> (b,c),
                                      putl :: b -> c -> (a,c)}
                                      
compose :: SymLens a b -> SymLens b c -> SymLens a c
compose (SymLens def1 pr1 pl1) (SymLens def2 pr2 pl2) =
  SymLens (def1, def2)
          (\a (s1,s2) -> let (b, s1') = pr1 a s1 in
                         let (c, s2') = pr2 b s2 in
                         (c, (s1', s2')))
          (\c (s1,s2) -> let (b, s2') = pl2 c s2 in
                         let (a, s1') = pl1 b s1 in
                         (a, (s1', s2')))
                                                                              

projRight :: d -> SymLens (a,d) a
projRight def = SymLens def
                        (\(a, d) _ -> (a, d))
                        (\a d -> ((a,d), d)) 

projLeft :: d -> SymLens a (a,d)
projLeft def = SymLens def
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
     
inv :: SymLens a b -> SymLens b a
inv (SymLens def pr pl) = SymLens def pl pr                                                 

term :: a -> SymLens a ()
term def = SymLens def
                   (\a  _ -> ((), a))
                   (\() a -> (a, a))
                   
disconnect :: a -> b -> SymLens a b
disconnect defa defb = term defa `compose` inv (term defb)

rlmap :: SymLens a b -> SymLens [a] [b]
rlmap (SymLens def pr pl) = 
  SymLens (repeat def)
          (\as cs -> let (bs, cs') = unzip $ map (uncurry pr) (zip as cs) in
                     (bs, cs' ++ drop (length cs') cs))
          (\bs cs -> let (as, cs') = unzip $ map (uncurry pl) (zip bs cs) in
                     (as, cs' ++ drop (length cs') cs))
                      
flmap :: SymLens a b -> SymLens [a] [b]
flmap (SymLens def pr pl) = 
  SymLens []
          (\as cs -> unzip $ map (uncurry pr) (zip as (cs ++ repeat def)))                      
          (\bs cs -> unzip $ map (uncurry pl) (zip bs (cs ++ repeat def)))
                                     
                                     