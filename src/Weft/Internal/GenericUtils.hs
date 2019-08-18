module Weft.Internal.GenericUtils where

import Data.Proxy
import GHC.TypeLits
import GHC.Generics
import Data.Kind

class MapP1 (c :: * -> Constraint)
            (r :: *)
            (irep :: * -> *)
            (orep :: * -> *) where
  mapP1 :: (forall t. c t => String -> t -> r) -> irep x -> orep x

instance MapP1 c r irep orep
      => MapP1 c r (M1 D _2 irep) (M1 D _2 orep) where
  mapP1 f (M1 rep) = M1 $ mapP1 @c @r f rep

instance MapP1 c r irep orep
      => MapP1 c r (M1 C _2 irep) (M1 C _2 orep) where
  mapP1 f (M1 rep) = M1 $ mapP1 @c @r f rep

instance ( MapP1 c r irep1 orep1
         , MapP1 c r irep2 orep2
         )
      => MapP1 c r (irep1 :*: irep2) (orep1 :*: orep2) where
  mapP1 f (rep1 :*: rep2) = mapP1 @c @r f rep1 :*: mapP1 @c @r f rep2

instance (KnownSymbol n, c t)
      => MapP1 c r
               (M1 S ('MetaSel ('Just n) _1 _2 _3) (K1 _4 t))
               (M1 S ('MetaSel ('Just n) _1 _2 _3) (K1 _4 r)) where
  mapP1 f (M1 (K1 t)) = M1 $ K1 $ f (symbolVal $ Proxy @n) t



class FoldP1 (c :: * -> Constraint)
             (rep :: * -> *) where
  foldP1 :: Semigroup m => (forall t. c t => String -> t -> m) -> rep x -> m

instance FoldP1 c rep => FoldP1 c (M1 D _2 rep) where
  foldP1 f (M1 rep) = foldP1 @c f rep

instance FoldP1 c rep => FoldP1 c (M1 C _2 rep)  where
  foldP1 f (M1 rep) = foldP1 @c f rep

instance (FoldP1 c rep1, FoldP1 c rep2)
      => FoldP1 c (rep1 :*: rep2)  where
  foldP1 f (rep1 :*: rep2) = foldP1 @c f rep1 <> foldP1 @c f rep2

instance (KnownSymbol n, c t)
      => FoldP1 c (M1 S ('MetaSel ('Just n) _1 _2 _3) (K1 _4 t)) where
  foldP1 f (M1 (K1 t)) = f (symbolVal $ Proxy @n) t

