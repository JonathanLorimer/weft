module Weft.Generics.Hydrate
  ( HasHydrate
  , hydrate
  , hydrateF
  ) where

import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Weft.Internal.Types hiding (query)


------------------------------------------------------------------------------
-- |
type HasHydrate record =
  ( Generic record
  , GHydrate (Rep record)
             (J record 'Query)
             (J record 'Response)
  )


------------------------------------------------------------------------------
-- |
hydrate :: HasHydrate record => record -> JHKD record 'Query -> JHKD record 'Response
hydrate d query = HKD $ gHydrate (from d) (runHKD query)

hydrateF :: (HasHydrate record, Functor f )
         => f record
         -> JHKD record 'Query
         -> f (JHKD record 'Response)
hydrateF fd q = (flip hydrate q) <$> fd


------------------------------------------------------------------------------
-- |
class GHydrate (fd :: * -> *) (fq :: * -> *) (fr :: * -> *) where
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

instance (GHydrateTerm d q r) =>
    GHydrate (K1 _1 d)
             (K1 _1 q)
             (K1 _1 r) where
  gHydrate (K1 d) (K1 q) = K1 $ gHydrateTerm d q

class GHydrateTerm d q r where
  gHydrateTerm :: d -> q -> r

------------------------------------------------------------------------------
-- |
instance GHydrateTerm d (Magic 'Query q) (Magic 'Response r) =>
      GHydrateTerm d
                   (ToMagic 'Query q)
                   (ToMagic 'Response r) where
  gHydrateTerm d (ToMagic q) = ToMagic $ gHydrateTerm d q


------------------------------------------------------------------------------
-- |
instance GHydrateTerm a
                      (M.Map Text (Args args, ()))
                      (M.Map Text a) where
  gHydrateTerm d q = fmap (const d) q

------------------------------------------------------------------------------
-- |
instance GHydrateTerm (Method args a)
                      (M.Map Text (Args args, ()))
                      (M.Map Text a) where
  gHydrateTerm (Method d) q = fmap (const d) q

------------------------------------------------------------------------------
-- |
instance ( Generic record
         , GHydrate (Rep record)
                    (M1 _1 _2 fq)
                    (M1 _1 _2 fr)
         ) =>
      GHydrateTerm (Method args record)
                   (M.Map Text (Args args, M1 _1 _2 fq _4))
                   (M.Map Text (M1 _1 _2 fr _4)) where
  gHydrateTerm (Method d) q = fmap (gHydrate (from d) . snd) q


------------------------------------------------------------------------------
-- |
instance HasHydrate record =>
      GHydrateTerm [record]
                   (M.Map Text (Args args, JHKD record 'Query))
                   (M.Map Text [JHKD record 'Response]) where
  gHydrateTerm d q = fmap ((<$> d) . flip hydrate . snd) q

instance ( Generic record
         , GHydrate (Rep record)
                    (M1 _1 _2 fq)
                    (M1 _1 _2 fr)
         ) =>
      GHydrateTerm [record]
                   (M.Map Text (Args args, M1 _1 _2 fq _4))
                   (M.Map Text [M1 _1 _2 fr _4]) where
  gHydrateTerm d q = fmap ((<$> fmap from d) . flip gHydrate . snd) q

