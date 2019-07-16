module ResponseGenerator where

import GHC.Generics
import SchemaGenerator

type IsHydrateable record = ( Generic (record 'Data)
                            , Generic (record 'Query)
                            , Generic (record 'Response)
                            , GHydrate (Rep (record 'Data))
                                    (Rep (record 'Query))
                                    (Rep (record 'Response))
                            )

hydrate :: IsHydrateable record => record 'Data -> record 'Query -> record 'Response
hydrate d query = to $ gHydrate (from d) (from query)

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

-- Q3, R3
instance GHydrate (K1 x (d))
                  (K1 x (Bool))
                  (K1 x (Maybe d)) where 
    gHydrate (K1 _) (K1 False) = K1 Nothing 
    gHydrate (K1 d) (K1 True)  = K1 $ Just d

-- Q2, R2
instance IsHydrateable record =>
         GHydrate (K1 x (record 'Data))
                  (K1 x (Maybe (record 'Query)))
                  (K1 x (Maybe (record 'Response))) where 
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing 
    gHydrate (K1 d) (K1 (Just q)) = K1 $ Just $ hydrate d q

-- Q1, R1 List
instance IsHydrateable record =>
         GHydrate (K1 x [record 'Data])
                  (K1 x (Maybe (record 'Query)))
                  (K1 x (Maybe [record 'Response])) where 
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing 
    gHydrate (K1 d) (K1 (Just q)) = K1 $ Just $ (flip hydrate $ q) <$> d