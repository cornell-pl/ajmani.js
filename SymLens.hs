{-# LANGUAGE ExistentialQuantification #-}

module SymLens where

import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Category (Category)
import qualified Control.Category as C

import Control.Monad.State

data SymLens a b = forall c. (Eq c, Show c) => SymLens { state :: c,
                                                         putr ::  a -> StateT c IO b,
                                                         putl ::  b -> StateT c IO a }

instance Category SymLens where
  id = idL
  (.) = flip compose

idL :: SymLens a a
idL = SymLens () pr pl
  where pr a = put () >> return a
        pl a = put () >> return a

inv :: SymLens a b -> SymLens b a
inv (SymLens def pr pl) = SymLens def pl pr

-- term :: (Eq a, Show a) => a -> SymLens a ()
-- term def = SymLens def pr pl
--   where pr a  _ = ((), a)
--         pl () a = (a, a)

-- disconnect :: (Eq a, Eq b, Show a, Show b) => a -> b -> SymLens a b
-- disconnect defa defb = term defa `compose` inv (term defb)

compose :: SymLens a b -> SymLens b c -> SymLens a c
compose (SymLens def1 pr1 pl1) (SymLens def2 pr2 pl2) = SymLens (def1, def2) pr pl
  where pr a = do
          (s1,s2) <- get
          (b, s1') <- lift $ runStateT (pr1 a) s1
          (c, s2') <- lift $ runStateT (pr2 b) s2
          put (s1', s2')
          return c
        pl c = do
          (s1,s2) <- get
          (b, s2') <- lift $ runStateT (pl2 c) s2
          (a, s1') <- lift $ runStateT (pl1 b) s1
          put (s1', s2')
          return a

swap :: SymLens (a,b) (b,a)
swap = SymLens () f f
  where f = (\(a,b) -> put () >> return (b,a))

fstl :: SymLens a b -> SymLens (a,d) (b,d)
fstl (SymLens i pr pl) = SymLens i (put pr) (put pl)
  where put p (a,d) = do
          a' <- p a
          return (a',d)

sndl :: SymLens a b -> SymLens (d,a) (d,b)
sndl s = swap . fstl s . swap

prod :: SymLens a b -> SymLens c d -> SymLens (a,c) (b,d)
prod l1 l2 = sndl l2 . fstl l1

-- assocl :: SymLens (a,(b,c)) ((a,b),c)
-- assocl = SymLens () pr pl
--   where pr (a,(b,c)) _ = (((a,b),c),())
--         pl ((a,b),c) _ = ((a,(b,c)),())

-- assocr :: SymLens ((a,b),c) (a,(b,c))
-- assocr = inv assocl

-- transpose :: SymLens ((a,b),(c,d)) ((a,c),(b,d))
-- transpose = assocr . ((assocl . (id `prod` swap) . assocr) `prod` id) . assocl

-- genDup :: (Eq a) => (a -> a -> String) -> SymLens a (a,a)
-- genDup errFn = SymLens () pr pl
--   where pr a _ = ((a,a), ())
--         pl (a,a') _ | a==a'     = (a, ())
--                     | otherwise = error (errFn a a')

-- dup :: (Eq a) => String -> SymLens a (a,a)
-- dup errMsg =
--   SymLens ()
--           (\a _ -> ((a,a), ()))
--           (\(a,a') _ -> if a == a' then (a, ())
--                         else error errMsg)

-- projRight :: (Eq d, Show d) => d -> SymLens (a,d) a
-- projRight def = SymLens def pr pl
--   where pr     = const
--         pl a d = ((a,d),d)

-- projLeft :: (Eq d, Show d) => d -> SymLens a (a,d)
-- projLeft = inv . projRight