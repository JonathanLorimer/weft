module Weft.Generics.Hydrate
  ( HasHydrate
  , hydrate
  , hydrateF
  ) where

import Weft.Internal.Types hiding (query)
import GHC.Generics


------------------------------------------------------------------------------
-- |
type HasHydrate record =
  ( Generic (record 'Data)
  , Generic (record 'Query)
  , Generic (record 'Response)
  , GHydrate (Rep (record 'Data))
             (Rep (record 'Query))
             (Rep (record 'Response))
  )


------------------------------------------------------------------------------
-- |
hydrate :: HasHydrate record => record 'Data -> record 'Query -> record 'Response
hydrate d query = to $ gHydrate (from d) (from query)

hydrateF :: (HasHydrate record, Functor f )
         => f (record 'Data)
         -> record 'Query
         -> f (record 'Response)
hydrateF fd q = (flip hydrate q) <$> fd


------------------------------------------------------------------------------
-- |
class GHydrate fd fq fr where
    gHydrate :: fd x -> fq x -> fr x

instance (GHydrate fd fq fr) =>
    GHydrate (M1 x y fd)
             (M1 x y fq)
             (M1 x y fr) where
    gHydrate (M1 fd) (M1 fq) = M1 $ (gHydrate fd fq)

instance (GHydrate fd fq fr, GHydrate gd gq gr) =>
    GHydrate (fd :*: gd)
             (fq :*: gq)
             (fr :*: gr) where
    gHydrate (fd :*: gd) (fq :*: gq) = (gHydrate fd fq) :*: (gHydrate gd gq)

instance GHydrate (K1 x a)
                  (K1 x (Maybe (Args args, ())))
                  (K1 x (Maybe a)) where
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing
    gHydrate (K1 d) (K1 (Just (_, ()))) = K1 $ Just d

instance HasHydrate record =>
         GHydrate (K1 x (record 'Data))
                  (K1 x (Maybe (Args args, record 'Query)))
                  (K1 x (Maybe (record 'Response))) where
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing
    gHydrate (K1 d) (K1 (Just (_, q))) = K1 $ Just $ hydrate d q

instance HasHydrate record =>
         GHydrate (K1 x [record 'Data])
                  (K1 x (Maybe (Args args, record 'Query)))
                  (K1 x (Maybe [record 'Response])) where
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing
    gHydrate (K1 d) (K1 (Just (_, q))) = K1 $ Just $ (flip hydrate $ q) <$> d

