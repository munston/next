module Data.Functor.Next where

{-
implements a functor via a lens to a shifting cursor
a complete pass of the suspended traversal is required at the functor instance
-}

class Lens a b where
 get :: b -> a
 set :: a -> b -> b

class Next a where
 next :: a -> a

type Nudge ix a = (Lens ix a,Next ix)

nudge :: Nudge ix a => a -> a
nudge a = set (next ix) a
 where
  ix = get a
  
class (Nudge ix b,Lens a b) => Zipper ix a b where
 lengthZ :: Int
 getZ :: ix -> b -> a
 setZ :: ix -> a -> b -> b

fmapDefaultZ :: Zipper ix a (f a) => (a->a) -> f a -> f a
fmapDefaultZ = undefined
