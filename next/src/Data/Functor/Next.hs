{-# Language MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances #-}
module Data.Functor.Next where

{-
implements a functor via a lens to a shifting cursor
a complete pass of the suspended traversal is required at the functor instance
-}

class Next ix a | a -> ix where
 cursor :: a -> ix
 nudge :: (ix -> ix,(ix -> ix) -> a -> a)

-- length, cursor, entries.
data ListCursor a = ListCursor Int Int [a] deriving Show

newListCursor xs = ListCursor 0 0 xs

instance Next Int (ListCursor a) where
 cursor (ListCursor _ ix _) = ix
 nudge = ((+1),\f (ListCursor l ix xs) -> let (ix',ix'') = (f ix,if ix' == l then 0 else ix') in ListCursor l ix'' xs)

next :: Next ix a => a -> a
next a = g f a
 where
  (f,g) = nudge
  
class Next ix b => Zipper ix a b | b -> a, b -> ix where
 lengthZ :: b -> Int
 getZ :: ix -> b -> a
 setZ :: ix -> a -> b -> b

instance Zipper Int Int (ListCursor Int) where
 lengthZ (ListCursor l ix xs) = l
 getZ ix (ListCursor l _ xs) = xs !! ix
 setZ ix a (ListCursor l ix' xs) | ix == ix' = ListCursor l ix' $ take ix xs ++ [ {-head (drop ix xs)-} a ] ++ tail (drop ix xs)

editZ :: Zipper ix a b => (a->a) -> (b->b)
editZ f x = z
 where
  ix = cursor x
  y = getZ ix x
  z = setZ ix (f y) x

monoMap :: Zipper ix a (f a) => (a->a) -> f a -> f a
monoMap f x = iterate (next . editZ f) x !! (lengthZ x) 

test = monoMap (error.show::Int->Int) $ newListCursor [1,2,3]
