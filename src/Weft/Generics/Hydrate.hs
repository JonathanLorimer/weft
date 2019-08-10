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
magicHydrate d query = HKD $ gHydrate (from d) (from query)

-- hydrateF :: (HasHydrate record, Functor f )
--          => f (record 'Data)
--          -> record 'Query
--          -> f (record 'Response)
-- hydrateF fd q = (flip hydrate q) <$> fd


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
                  (K1 x (M.Map Text (Args args, ())))
                  (K1 x (M.Map Text a)) where
    gHydrate (K1 d) (K1 q) = K1 $ fmap (const d) q

instance HasMagicHydrate record =>
         GHydrate (K1 x record)
                  (K1 x (M.Map Text (Args args, record 'Query)))
                  (K1 x (M.Map Text (record 'Response))) where
    gHydrate (K1 d) (K1 q) = K1 $ fmap (hydrate d . snd) q

instance HasMagicHydrate record =>
         GHydrate (K1 x [record])
                  (K1 x (M.Map Text (Args args, record 'Query)))
                  (K1 x (M.Map Text [record 'Response])) where
    gHydrate (K1 d) (K1 q) = K1 $ fmap ((<$> d) . flip hydrate . snd) q

