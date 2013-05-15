
module SymLens.Map where

import SymLens
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map

appendInto :: (Enum k, Ord k) => SymLens (Map.Map k a, Map.Map k a) (Map.Map k a)
appendInto = 
  SymLens Map.empty
          (\(m1, m2) mc -> fst $ Map.foldlWithKey (\((m, mc'), nextkey) k a -> ((Map.insert nextkey a m, 
                                                                                 Map.insert nextkey k mc'), 
                                                                                succ nextkey))
                                                  ((m1, Map.empty), succ $ fst $ Map.findMax m1) m2)
          (\m mc -> ((Map.difference m mc, Map.mapKeys (fromJust . flip Map.lookup mc) $ Map.intersection m mc), mc))  
                                          
insert :: (Ord k) => k -> a -> SymLens (Map.Map k a) (Map.Map k a)
insert k initA = 
  SymLens (initA, Nothing) pr pl
  where pr m (a,_) = (Map.insert k a m, (a, Map.lookup k m))
        pl m (a,Nothing) = (Map.delete k m, (fromMaybe a $ Map.lookup k m, Nothing))
        pl m (a,Just a') = (Map.insert k a' m, (fromMaybe a $ Map.lookup k m, Just a'))

fmmap :: (Ord k) => SymLens a b -> SymLens (Map.Map k a) (Map.Map k b)
fmmap (SymLens i pr pl) =
  SymLens Map.empty
          (\ma mc -> Map.foldlWithKey (\(mb, mc') k a -> let (b,c) = pr a $ fromMaybe i $ Map.lookup k mc in
                                                         (Map.insert k b mb, Map.insert k c mc')) 
                                      (Map.empty, Map.empty) ma)
          (\mb mc -> Map.foldlWithKey (\(ma, mc') k b -> let (a,c) = pl b $ fromMaybe i $ Map.lookup k mc in
                                                         (Map.insert k a ma, Map.insert k c mc')) 
                                      (Map.empty, Map.empty) mb)
