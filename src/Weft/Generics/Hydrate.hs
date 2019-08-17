module Weft.Generics.Hydrate
  ( HasMagicHydrate
  , magicHydrate
  -- , hydrateF
  ) where

import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Weft.Internal.Types hiding (query)


------------------------------------------------------------------------------
-- |
type HasMagicHydrate record =
  ( Generic record
  , GHydrate (Rep record)
             (J record 'Query)
             (J record 'Response)
  )


------------------------------------------------------------------------------
-- |
magicHydrate :: HasMagicHydrate record => record -> JHKD record 'Query -> JHKD record 'Response
magicHydrate d query = HKD $ gHydrate (from d) (runHKD query)

-- hydrateF :: (HasHydrate record, Functor f )
--          => f (record 'Data)
--          -> record 'Query
--          -> f (record 'Response)
-- hydrateF fd q = (flip hydrate q) <$> fd


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

instance GHydrateTerm a
                      (M.Map Text (Args args, ()))
                      (M.Map Text a) where
  gHydrateTerm d q = fmap (const d) q

instance GHydrateTerm (Method args a)
                      (M.Map Text (Args args, ()))
                      (M.Map Text a) where
  gHydrateTerm (Method d) q = fmap (const d) q

instance HasMagicHydrate record =>
      GHydrateTerm record
                   (M.Map Text (Args args, JHKD record 'Query))
                   (M.Map Text (JHKD record 'Response)) where
  gHydrateTerm d q = fmap (magicHydrate d . snd) q

instance HasMagicHydrate record =>
      GHydrateTerm (Method args record)
                   (M.Map Text (Args args, JHKD record 'Query))
                   (M.Map Text (JHKD record 'Response)) where
  gHydrateTerm (Method d) q = fmap (magicHydrate d . snd) q

instance HasMagicHydrate record =>
      GHydrateTerm [record]
                   (M.Map Text (Args args, JHKD record 'Query))
                   (M.Map Text [JHKD record 'Response]) where
  gHydrateTerm d q = fmap ((<$> d) . flip magicHydrate . snd) q

instance {-# OVERLAPPING #-} GHydrateTerm d (Magic 'Query q) (Magic 'Response r) =>
      GHydrateTerm d
                   (ToMagic 'Query q)
                   (ToMagic 'Response r) where
  gHydrateTerm d (ToMagic q) = ToMagic $ gHydrateTerm d q

instance GHydrate (M1 _1 _2 d) (M1 _1 _2 q) (M1 _1 _2 r) =>
      GHydrateTerm (M1 _1 _2 d _3)
                   (M1 _1 _2 q _3)
                   (M1 _1 _2 r _3) where
  gHydrateTerm = gHydrate


instance (Generic record, GHydrate (Rep record)
                  (M1 _1 _2 fq)
                  (M1 _1 _2 fr)) =>
      GHydrateTerm [record]
                   (M.Map Text (Args args, M1 _1 _2 fq _4))
                   (M.Map Text [M1 _1 _2 fr _4]) where
  gHydrateTerm d q = fmap ((<$> fmap from d) . flip gHydrate . snd) q


-- instance (Generic record, GHydrate (Rep record)
--                   (M1 _1 _2 fq)
--                   (M1 _1 _2 fr)) =>
--       GHydrateTerm record
--                    (M.Map Text (Args args, M1 _1 _2 fq _4))
--                    (M.Map Text (M1 _1 _2 fr _4)) where
--   gHydrateTerm d q = fmap (gHydrate (from d) . snd) q

instance (Generic record, GHydrate (Rep record)
                  (M1 _1 _2 fq)
                  (M1 _1 _2 fr)) =>
      GHydrateTerm (Method args record)
                   (M.Map Text (Args args, M1 _1 _2 fq _4))
                   (M.Map Text (M1 _1 _2 fr _4)) where
  gHydrateTerm (Method d) q = fmap (gHydrate (from d) . snd) q


