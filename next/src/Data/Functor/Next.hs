module Data.Functor.Next where

{-
implements a functor via a lens to a shifting cursor
a complete pass of the suspended traversal is required at the functor instance
-}

class Next ix a | a -> ix where
 nudge :: (ix -> ix,(ix -> ix) -> a -> a)

next :: Next ix a => a -> a
next a = g f a
 where
  (f,g) = nudge
  
class Next ix b => Zipper ix a b where
 lengthZ :: Int
 getZ :: ix -> b -> a
 setZ :: ix -> a -> b -> b

fmapDefaultZ :: Zipper ix a (f a) => (a->a) -> f a -> f a
fmapDefaultZ = undefined
