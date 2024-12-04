{-# Language MultiParamTypeClasses , FunctionalDependencies #-}
module Data.Functor.Next where

{-
implements a functor via a lens to a shifting cursor
a complete pass of the suspended traversal is required at the functor instance
-}

class Next ix a | a -> ix where
 cursor :: a -> ix
 nudge :: (ix -> ix,(ix -> ix) -> a -> a)

next :: Next ix a => a -> a
next a = g f a
 where
  (f,g) = nudge
  
class Next ix b => Zipper ix a b | b -> a, b -> ix where
 lengthZ :: b -> Int
 getZ :: ix -> b -> a
 setZ :: ix -> a -> b -> b

editZ :: Zipper ix a b => (a->a) -> (b->b)
editZ f x = z
 where
  ix = cursor x
  y = getZ ix x
  z = setZ ix (f y) x

monoMap :: Zipper ix a (f a) => (a->a) -> f a -> f a
monoMap f x = iterate (next . editZ f) x !! (lengthZ x) 
