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
  ( Generic (record 'Data)
  , Generic (record 'Query)
  , Generic (record 'Response)
  , GHydrate (Rep (record 'Data))
             (Rep (record 'Query))
             (Rep (record 'Response))
  )

type HasMagicHydrate record =
  ( Generic record
  , Generic record
  , Generic record
  , GHydrate (J record 'Data)
             (J record 'Query)
             (J record 'Response)
  )


------------------------------------------------------------------------------
-- |
hydrate :: HasHydrate record => record 'Data -> record 'Query -> record 'Response
hydrate d query = to $ gHydrate (from d) (from query)

magicHydrate
    :: HasMagicHydrate record
    => J' record 'Data
    -> J' record 'Query
    -> J' record 'Response
magicHydrate = gHydrate

hydrateF :: (HasHydrate record, Functor f )
         => f (record 'Data)
         -> record 'Query
         -> f (record 'Response)
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

instance GHydrateTerm f q r
      => GHydrate (K1 _1 f)
                  (K1 _1 (M.Map Text q))
                  (K1 _1 (M.Map Text r)) where
  gHydrate (K1 f) (K1 q) = K1 $ gHydrateTerm f q

class GHydrateTerm (d :: *) (q :: *) (r :: *) where
  gHydrateTerm :: d -> M.Map Text q -> M.Map Text r

instance GHydrateTerm (Magic tsd d)
                      (Magic tsq q)
                      (Magic tsr r)
      => GHydrateTerm (ToMagic tsd d)
                      (ToMagic tsq q)
                      (ToMagic tsr r) where
  gHydrateTerm (ToMagic d) =
    fmap ToMagic . gHydrateTerm d . fmap unMagic

-- instance GHydrate (M1 _1 _2 fd)
--                   (M1 _1 _2 fq)
--                   (M1 _1 _2 fr)
--       => GHydrateTerm (M1 _1 _2 fd _3)
--                       (M1 _1 _2 fq _3)
--                       (M1 _1 _2 fr _3) where
--   gHydrateTerm = _ gHydrate

instance GHydrateTerm a (Args args, ()) a where
  gHydrateTerm d q = fmap (const d) q

instance HasHydrate record
      => GHydrateTerm (record 'Data)
                      ((Args args, record 'Query))
                      (record 'Response) where
  gHydrateTerm d q = fmap (hydrate d . snd) q

-- instance HasHydrate d (Args args, q) r => GHydrateTerm [d] (Args args, q) [r] where
--   gHydrateTerm d q = fmap ((r$> d) . _ . snd) q


-- instance GHydrateTerm r q r =>
--          GHydrateTerm ([record 'Data])
--                       ((M.Map Text (Args args, record 'Query)))
--                       ((M.Map Text [record 'Response])) where
--     gHydrate (K1 d) (K1 q) = K1 $ fmap ((<$> d) . flip hydrate . snd) q
